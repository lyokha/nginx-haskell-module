Nginx Haskell module
====================

This Nginx module allows compiling and running Haskell source code found in a
configuration file or an existing shared library.

Table of contents
-----------------

- [Motivational example](#motivational-example)
- [Static content in HTTP responses](#static-content-in-http-responses)
- [Optimized unsafe content handler](#optimized-unsafe-content-handler)
- [Reloading of haskell code and static content](#reloading-of-haskell-code-and-static-content)
- [Wrapping haskell code organization](#wrapping-haskell-code-organization)
- [Static linkage against basic haskell libraries](#static-linkage-against-basic-haskell-libraries)
- [Some facts about efficiency](#some-facts-about-efficiency)
- [Some facts about exceptions](#some-facts-about-exceptions)
- [Troubleshooting](#troubleshooting)
- [See also](#see-also)

Motivational example
--------------------

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    haskell ghc_extra_flags
            '-hide-package regex-pcre -XFlexibleInstances -XTupleSections';

    haskell compile standalone /tmp/ngx_haskell.hs '

import qualified Data.Char as C
import           Text.Regex.PCRE
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Error
import           Data.Function (on)
import           Control.Monad
import           Safe

toUpper = map C.toUpper
NGX_EXPORT_S_S (toUpper)

takeN = take . readDef 0
NGX_EXPORT_S_SS (takeN)

NGX_EXPORT_S_S (reverse)

-- does not match when any of the 2 args is empty or not decodable
matches = (fromMaybe False .) . liftM2 (=~) `on` (doURLDecode =<<) . toMaybe
    where toMaybe [] = Nothing
          toMaybe a  = Just a
NGX_EXPORT_B_SS (matches)

firstNotEmpty = headDef "" . filter (not . null)
NGX_EXPORT_S_LS (firstNotEmpty)

isInList [] = False
isInList (x : xs) = x `elem` xs
NGX_EXPORT_B_LS (isInList)

jSONListOfInts :: B.ByteString -> Maybe [Int]
jSONListOfInts = (decode =<<) . doURLDecode . L.fromStrict

isJSONListOfInts = isJust . jSONListOfInts
NGX_EXPORT_B_Y (isJSONListOfInts)

jSONListOfIntsTakeN x = encode $ maybe [] (take n) $ jSONListOfInts y
    where (readDef 0 . C8.unpack -> n, B.tail -> y) = B.break (== 124) x
NGX_EXPORT_Y_Y (jSONListOfIntsTakeN)

class UrlDecodable a
    where doURLDecode :: a -> Maybe a

instance UrlDecodable String where
    -- adopted from
    -- http://www.rosettacode.org/wiki/URL_decoding#Haskell
    doURLDecode [] = Just []
    doURLDecode (\'%\' : xs) =
        case xs of
            (a : b : xss) ->
                (:) . C.chr <$> readMay (\'0\' : \'x\' : [a, b])
                            <*> doURLDecode xss
            _ -> Nothing
    doURLDecode (\'+\' : xs) = (\' \' :) <$> doURLDecode xs
    doURLDecode (x : xs) = (x :) <$> doURLDecode xs

instance UrlDecodable L.ByteString where
    -- adopted for ByteString arguments from
    -- http://www.rosettacode.org/wiki/URL_decoding#Haskell
    doURLDecode (L.null -> True) = Just L.empty
    doURLDecode (L.uncons -> Just (37, xs))
        | L.length xs > 1 =
            let (C8L.unpack -> c, xss) = L.splitAt 2 xs
            in L.cons <$> readMay (\'0\' : \'x\' : c)
                      <*> doURLDecode xss
        | otherwise = Nothing
    doURLDecode (L.uncons -> Just (43, xs)) = (32 `L.cons`) <$> doURLDecode xs
    doURLDecode (L.uncons -> Just (x, xs)) = (x `L.cons`) <$> doURLDecode xs

urlDecode = fromMaybe "" . doURLDecode
NGX_EXPORT_S_S (urlDecode)

fromMd (C8.unpack -> x) = uncurry (, "text/html", ) $
    case readMarkdown def x of
        Right p -> (writeHtml p, 200)
        Left  e -> (, 500) $ writeError $ case e of
                      ParseFailure  e -> show e
                      ParsecError _ e -> show e
    where writeHtml = C8L.pack . writeHtmlString defHtmlWriterOptions
          writeError = writeHtml . doc . para . singleton . Str
          defHtmlWriterOptions = def
              { writerStandalone = True,
                writerTemplate = "<html>\\n<body>\\n$body$</body></html>" }
NGX_EXPORT_HANDLER (fromMd)

toYesNo "0" = "No"
toYesNo "1" = "Yes"
toYesNo  _  = "Unknown"
NGX_EXPORT_S_S (toYesNo)

    ';

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        location / {
            haskell_run toUpper $hs_a $arg_a;
            echo "toUpper ($arg_a) = $hs_a";
            if ($arg_b) {
                haskell_run takeN $hs_a $arg_b $arg_a;
                echo "takeN ($arg_a, $arg_b) = $hs_a";
                break;
            }
            if ($arg_c) {
                haskell_run reverse $hs_a $arg_c;
                echo "reverse ($arg_c) = $hs_a";
                break;
            }
            if ($arg_d) {
                haskell_run matches $hs_a $arg_d $arg_a;
                haskell_run urlDecode $hs_b $arg_a;
                echo "matches ($arg_d, $hs_b) = $hs_a";
                break;
            }
            if ($arg_e) {
                haskell_run firstNotEmpty $hs_a $arg_f $arg_g $arg_a;
                echo "firstNotEmpty ($arg_f, $arg_g, $arg_a) = $hs_a";
                break;
            }
            if ($arg_l) {
                haskell_run isInList $hs_a $arg_a secret1 secret2 secret3;
                echo "isInList ($arg_a, <secret words>) = $hs_a";
                break;
            }
            if ($arg_m) {
                haskell_run isJSONListOfInts $hs_a $arg_m;
                haskell_run urlDecode $hs_b $arg_m;
                echo "isJSONListOfInts ($hs_b) = $hs_a";
                break;
            }
            if ($arg_n) {
                haskell_run jSONListOfIntsTakeN $hs_a $arg_take|$arg_n;
                haskell_run urlDecode $hs_b $arg_n;
                echo "jSONListOfIntsTakeN ($hs_b, $arg_take) = $hs_a";
                break;
            }
        }

        location /content {
            haskell_run isJSONListOfInts $hs_a $arg_n;
            haskell_run toYesNo $hs_b $hs_a;
            haskell_run jSONListOfIntsTakeN $hs_c $arg_take|$arg_n;
            haskell_run urlDecode $hs_d $arg_n;
            haskell_content fromMd "
## Do some JSON parsing

### Given ``$hs_d``

* Is this list of integer numbers?

    + *$hs_b*

* Take $arg_take elements

    + *``$hs_c``*
    ";

        }
    }
}
```

Haskell source code is loaded with directives *haskell compile* or *haskell
load*. Both directives accept an absolute path to a haskell source file as their
first argument and a haskell source code as their second argument. The code is
getting saved to the path and compiled to a shared library when nginx starts.
The directives have a subtle distinction: *haskell compile* always requires the
code argument and runs compiler unconditionally, whereas *haskell load* checks
if the target library exists and does not compile source code in this case, thus
eliminating necessity of the source code argument.

The module may load an arbitrary haskell code but only those functions are
accessible from nginx that are exported with special macros *NGX_EXPORT_S_S*,
*NGX_EXPORT_S_SS*, *NGX_EXPORT_B_S* and *NGX_EXPORT_B_SS* (here *S_S*, *S_SS*,
*B_S* and *B_SS* stand for mnemonic types *returns-String-accepts-String*,
*returns-String-accepts-String-String*, *returns-Bool-accepts-String* and
*returns-Bool-accepts-String-String*), their *list* counterparts
*NGX_EXPORT_S_LS* and *NGX_EXPORT_B_LS* (*LS* stands for *List-of-Strings*) and
two macros that deal with *byte strings*: *NGX_EXPORT_Y_Y* and *NGX_EXPORT_B_Y*
(*Y* stands for *bYte*). For the sake of efficiency byte string macros accept
*strict* but return (only *Y_Y*) *lazy* byte strings. Effectively this means
that only those functions are supported that return strings, byte strings or
booleans and accept one, two or more (only *S_LS* and *B_LS*) string arguments
or one byte string.

In this example 10 custom haskell functions are exported: *toUpper*, *takeN*,
*reverse* (which is normal *reverse* imported from *Prelude*), *matches*
(which requires module *Text.Regex.PCRE*), *firstNotEmpty*, *isInList*,
*isJSONListOfInts*, *jSONListOfIntsTakeN*, *urlDecode* and *toYesNo*. As soon as
probably this code won't compile due to ambiguity involved by presence of the
two packages *regex-pcre* and *regex-pcre-builtin*, I had to add an extra *ghc*
compilation flag *-hide-package regex-pcre* with directive *haskell
ghc_extra_flags*. Another flag *-XFlexibleInstances* passed into the directive
allows declaration of *instance UrlDecodable String*. Class *UrlDecodable*
provides function *doURLDecode* for decoding strings and byte strings that was
adopted from [here](http://www.rosettacode.org/wiki/URL_decoding#Haskell). Byte
string instance of *doURLDecode* makes use of *view patterns* in its clauses,
however this extension does not have to be declared explicitly because it was
already enabled in a pragma from the wrapping haskell code provided by this
module (see details in section [Wrapping haskell code
organization](#wrapping-haskell-code-organization)). In several clauses of
*doURLDecode* there are explicit characters wrapped inside single quotes which
are in turn escaped with backslashes to not confuse nginx parser as the haskell
code itself is wrapped inside single quotes. Exported function *urlDecode* is
defined via the string instance of *doURLDecode*: if decoding fails it returns
an empty string.

Let's look inside the *server* clause, in *location /* where the exported
haskell functions are used. Directive *haskell_run* takes three or more
arguments: it depends on the type of the exported function (*S_S*, *S_SS etc.*).
The first argument of the directive is the name of an exported haskell function,
the second argument is a custom variable where the function's return value will
be stored, and the remaining (one or two) arguments are complex values (in the
nginx notion: it means that they may contain arbitrary number of variables and
plain symbols) that correspond to the arguments of the exported function.
Directive *haskell_run* is allowed in *server*, *location* and *location-if*
clauses. In this example all returned strings are stored in the same variable
*hs_a* which is not a good habit for nginx configuration files. I only wanted to
show that upper nginx configuration levels being merged with lower levels behave
as normally expected.

There is another haskell directive *haskell_content* which accepts a haskell
function to generate HTTP response and an optional string that will be passed to
the function. The function may have one of the two types:
*strictByteString-to-lazyByteString* and
*strictByteString-to-3tuple(lazyByteString,String,Int)*. It must be exported
with *NGX_EXPORT_DEF_HANDLER* (*default* content handler) in the first case and
*NGX_EXPORT_HANDLER* in the second case. The elements in the *3tuple* correspond
to returned content, its type (e.g. *text/html* etc.) and HTTP status. Default
content handler sets content type to *text/plain* and HTTP status to *200*.
Directive *haskell_content* is allowed in *location* and *location-if* clauses
of the nginx configuration. In the location */content* from the above example
the directive *haskell_content* makes use of a haskell function *fromMd* to
generate HTML response from a markdown text. Function *fromMd* translates a
markdown text to HTML using Pandoc library.

What about doing some tests? Let's first start nginx.

```ShellSession
# nginx -p. -c./nginx.conf
[1 of 1] Compiling NgxHaskellUserRuntime ( /tmp/ngx_haskell.hs, /tmp/ngx_haskell.o )
Linking /tmp/ngx_haskell.so ...
```

Nginx compiles haskell code at its start. Had compilation failed and nginx would
not have started (see details about starting nginx in section [Reloading of
haskell code and static
content](#reloading-of-haskell-code-and-static-content)). In this case the code
is OK and we are moving forward.

```ShellSession
$ curl 'http://localhost:8010/?a=hello_world'
toUpper (hello_world) = HELLO_WORLD
$ curl 'http://localhost:8010/?a=hello_world&b=4'
takeN (hello_world, 4) = hell
$ curl 'http://localhost:8010/?a=hello_world&b=oops'
takeN (hello_world, oops) = 
$ curl 'http://localhost:8010/?c=intelligence'
reverse (intelligence) = ecnegilletni
$ curl 'http://localhost:8010/?d=intelligence&a=%5Ei'              # URL-encoded ^i
matches (intelligence, ^i) = 1
$ curl 'http://localhost:8010/?d=intelligence&a=%5EI'              # URL-encoded ^I
matches (intelligence, ^I) = 0
$ curl 'http://localhost:8010/?e=1&g=intelligence&a=smart'
firstNotEmpty (, intelligence, smart) = intelligence
$ curl 'http://localhost:8010/?e=1&g=intelligence&f=smart'
firstNotEmpty (smart, intelligence, ) = smart
$ curl 'http://localhost:8010/?e=1'
firstNotEmpty (, , ) = 
$ curl 'http://localhost:8010/?l=1'
isInList (, <secret words>) = 0
$ curl 'http://localhost:8010/?l=1&a=s'
isInList (s, <secret words>) = 0
$ curl 'http://localhost:8010/?l=1&a=secret2'
isInList (secret2, <secret words>) = 1
$ curl 'http://localhost:8010/?m=%5B1%2C2%2C3%5D'                  # URL-encoded [1,2,3]
isJSONListOfInts ([1,2,3]) = 1
$ curl 'http://localhost:8010/?m=unknown'
isJSONListOfInts (unknown) = 0
$ curl 'http://localhost:8010/?n=%5B10%2C20%2C30%2C40%5D&take=3'   # URL-encoded [10,20,30,40]
jSONListOfIntsTakeN ([10,20,30,40], 3) = [10,20,30]
$ curl 'http://localhost:8010/?n=%5B10%2C20%2C30%2C40%5D&take=undefined'
jSONListOfIntsTakeN ([10,20,30,40], undefined) = []
```

Let's try location */content* (in a browser it looks great!)

```ShellSession
$ curl -D- 'http://localhost:8010/content?n=%5B10%2C20%2C30%2C40%5D&take=3'
HTTP/1.1 200 OK
Server: nginx/1.8.0
Date: Fri, 04 Mar 2016 15:17:44 GMT
Content-Type: text/html
Content-Length: 323
Connection: keep-alive

<html>
<body>
<h2 id="do-some-json-parsing">Do some JSON parsing</h2>
<h3 id="given-10203040">Given <code>[10,20,30,40]</code></h3>
<ul>
<li><p>Is this list of integer numbers?</p>
<ul>
<li><em>Yes</em></li>
</ul></li>
<li><p>Take 3 elements</p>
<ul>
<li><em><code>[10,20,30]</code></em></li>
</ul></li>
</ul></body></html>
```

Static content in HTTP responses
--------------------------------

Reading files in runtime inescapably drops nginx performance. Fortunately there
is a haskell module
[Data.FileEmbed](http://hackage.haskell.org/package/file-embed) that makes it
possible to embed files during *ghc* compilation time. Consider the following
haskell content handler

```haskell
fromFile (C8.unpack -> "content.html") =
    (L.fromStrict $(embedFile "/path/to/content.html"), "text/html", 200)
fromFile (C8.unpack -> "content.txt") =
    (L.fromStrict $(embedFile "/path/to/content.txt"), "text/plain", 200)
fromFile _ =
    (C8L.pack "File not found", "text/plain", 500)
NGX_EXPORT_HANDLER (fromFile)
```

(to make it compile another option *-XTemplateHaskell* must be added into the
directive *haskell ghc_extra_flags* and the module *Data.FileEmbed* must be
imported too). Now with a new location

```nginx
        location /static {
            haskell_static_content fromFile "content.html";
            if ($arg_a) {
                haskell_static_content fromFile "content.txt";
                break;
            }
        }
```

HTTP requests with *URIs* that start with */static* will be responded with
contents of files listed in the clauses of the function *fromFile* that have
been embedded into the function during *ghc* compilation. Directive
*haskell_static_content* runs its haskell handler and allocates response data
only once in nginx worker's lifetime when the first request arrives and is
processed in the location. On further requests these data are sent back without
running the haskell handler. This makes directive *haskell_static_content* more
optimal for returning static data comparing with *haskell_content*.

Directive *haskell_static_content* is useful not only for returning files but
for any content that can be evaluated only once in nginx worker's lifetime.

Optimized unsafe content handler
--------------------------------

Let's go back to the example from the previous section. All the content handlers
we met so far receive a copy of data produced in haskell handlers. Using
references to the original data would lead to nasty things after haskell's
garbage collector wakeup so the only *safe* choice seems to be copying the
original data. Handler *fromFile* from the example takes static data embedded
into the haskell library by *Data.FileEmbed*, makes a copy of this and passes it
to the C code. It runs once per location during location configuration lifetime
thanks to the directive *haskell_static_content* implementation. Nonetheless
there are two duplicate static data copies in the program during its run which
looks wasteful. It can get even worse when using *haskell_static_content* is not
an option.

Here is an example. Module *Data.FileEmbed* allows embedding all files in a
directory recursively using template function *embedDir*. This make it possible
to emulate nginx static files delivery feature. The following is a quick and
dirty implementation.

*Haskell content handler.*

```haskell
fromFile (tailSafe . C8.unpack -> f) =
    case lookup f $(embedDir "/rootpath") of
        Just p  -> (L.fromStrict p,            "text/plain", 200)
        Nothing -> (C8L.pack "File not found", "text/plain", 404)
NGX_EXPORT_HANDLER (fromFile)
```

*Corresponding nginx location.*

```nginx
        location /static {
            haskell_content fromFile $uri;
        }
```

In this example the files are expected in the directory */rootpath/static*. As
soon as the target file is parameterized by the value of the *``$uri``*, the
directive *haskell_content* must be used in place of *haskell_static_content*.
It means that now files contents will be copied and freed on every single
request to location */static*.

To address unnecessary copying of static data, a new directive
*haskell_unsafe_content* is introduced. With it the above example can be
rewritten as follows.

*Haskell content handler.*

```haskell
fromFile (tailSafe . C8.unpack -> f) =
    case lookup f $(embedDir "/rootpath") of
        Just p  -> (p,                         text_plain, 200)
        Nothing -> (pack 14 "File not found"#, text_plain, 404)
    where pack l s = unsafePerformIO $ unsafePackAddressLen l s
          text_plain = pack 10 "text/plain"#
NGX_EXPORT_UNSAFE_HANDLER (fromFile)
```

*Corresponding nginx location.*

```nginx
        location /static {
            haskell_unsafe_content fromFile $uri;
        }
```

The *unsafe* handler returns *3tuple(strictByteString,strictByteString,Int)*.
The two strict byte strings in it must correspond to the *really* static data,
i.e. string literals like *"File not found"#*, *"text/plain"#* and those
embedded by the *Data.FileEmbed*, otherwise the nasty things may happen! Literal
strings that end with *hashes* (*#*) are actually addresses of compiled static
byte arrays that do not change during runtime. To enable the hash literals
option *-XMagicHash* must be added into the directive *haskell ghc_extra_flags*.
Working on such a low level requires using functions *unsafePackAddressLen* and
*unsafePerformIO* from modules *Data.ByteString.Unsafe* and *System.IO.Unsafe*
respectively (in this example *unsafePerformIO* can be safely replaced with the
fastest and the unsafest *IO unwrapper* *accursedUnutterablePerformIO* from
module *Data.ByteString.Internal*). Minimum requirements for using static byte
arrays in the module *Data.FileEmbed* are: *file-embed* version *0.0.7* and
*Template Haskell* version *2.5.0* (bundled with *ghc* since version *7.0.1*).

A working nginx configuration file with this unsafe content handler
implementation can be found in directory [test/tsung](test/tsung) of the project
tree.

Reloading of haskell code and static content
--------------------------------------------

When nginx master process receives signal *HUP* it loads new configuration,
starts new worker processes and shuts down the old workers gracefully. If the
configuration has been changed then there are 3 possible scenarios with regard
to the new haskell code.

- *Haskell code compiles.* The new workers will start, the old workers will
  shut down.

- *Haskell code fails to compile.* The new workers won't start, the old workers
  will proceed.

- *Haskell code compiles but type check of handlers fails.* (For example a
  haskell function that had been exported as content handler was passed to a
  directive *haskell_run* in some location.) The new workers will start but fail
  the module initialization and shut down, the old workers will shut down. No
  live workers will exist.

All errors are logged, so the best way to find out if errors occurred during
reloading of the nginx configuration (and at the start of nginx too) is to refer
to the logs.

Besides haskell code reloading, restart of workers makes data loaded by
directive *haskell_static_content* reload too.

Wrapping haskell code organization
----------------------------------

Macros NGX_EXPORT_S_S and others are really *cpp* macros expanded by program
*cpphs* during *ghc* compilation stage. The code where these macros and other
auxiliary functions are defined wraps the user's haskell code around thus
producing a single source file that contains a *standalone* module with name
*NgxHaskellUserRuntime*. This implementation imposes limitations on the user's
haskell code in the nginx configuration file, of which the most important is
inability to use haskell *file-header* pragmas like *LANGUAGE* and
*OPTIONS_GHC*. However this particular limitation can be worked around with
*-X...* options in directive *haskell ghc_extra_flags*. Standalone module
wrapping approach also brings ghc extensions *ForeignFunctionInterface*, *CPP*
and *ViewPatterns* into scope of the user's haskell code. Building the module
from scratch for later loading with directive *haskell load* is also difficult.

To address limitations of the standalone module approach, another *modular*
approach was introduced. In it, the wrapping haskell code must be built in a
separate haskell module *NgxExport* and installed in the system with *cabal*.
The source code of the module is located in directory
[haskell/ngx-export](haskell/ngx-export) of the project tree. The user's haskell
code in this approach must import the module *NgxExport*.

The export macros in *modular* approach syntactically and semantically differ
from the standalone approach's export macros! They are *template haskell*
functions expanded by *ghc* during compilation. It means that their names must
start with lower case letters and they must accept *names* rather than plain
haskell functions. In *Template Haskell*, *names* can be constructed with a
*single quote* placed before a normal function name. If the user's haskell code
is wrapped inside single quotes, the single quote that starts an exported
haskell handler must be escaped with a *backslash*. Altogether the standalone
approach's declarations like *NGX_EXPORT_S_SS (handler1)* and *NGX_EXPORT_S_S
(handler2)* are translated into corresponding modular approach's declarations
*ngxExportSSS \'handler1* and *ngxExportSS \'handler2*.

In the modular approach the user's haskell code is compiled with option
*-XTemplateHaskell* as soon as single quotes as names starters require it. The
module must be declared explicitly. You can find an nginx configuration file
equivalent to the example in the first section with the haskell code translated
for the modular approach in directory [haskell/ngx-export](haskell/ngx-export)
of the project tree.

It's worth saying that the standalone module compilation gets enabled with
keyword *standalone* passed as the first argument in directives *haskell
compile* and *haskell load* whereas the modular compilation gets enabled with
keyword *modular* or without any keyword.

Static linkage against basic haskell libraries
----------------------------------------------

By default *ghc* is configured to link the built haskell module against dynamic
haskell libraries which means that basic haskell packages must have been
installed on the target machine even when directive *haskell load* only loads an
already compiled library. In principle *ghc* permits building an independent
all-in-one shared library with static linkage against other haskell libraries,
but unfortunately the system linker will likely fail if those have been compiled
without compilation flag *-fPIC* on vast majority of modern platforms, notably
on *GNU/Linux x86_64*.

Here I want to show how to build haskell code from
[test/tsung/nginx.conf](test/tsung/nginx.conf) into an independent all-in-one
shared library on *Fedora 23 x86_64* with *ghc 7.10.2* installed from a [copr
repository](http://copr.fedorainfracloud.org/coprs/petersen/ghc-7.10.2/).

First of all, building of such a library is only possible from command-line as
soon as it requires tuning of some *ghc* options not available from directive
*haskell ghc_extra_flags*. Therefore the haskell code from the configuration
file must be extracted in a separate source file, say
*NgxHaskellUserRuntime.hs*. Here it is.

```haskell
{-# LANGUAGE TemplateHaskell, MagicHash, ViewPatterns #-}

module NgxHaskellUserRuntime where

import           NgxExport
import           Data.FileEmbed
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Unsafe
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Safe

fromFile (tailSafe . C8.unpack -> f) =
    case lookup f $(embedDir "/usr/local/webdata") of
        Just p  -> (p,                         text_plain, 200)
        Nothing -> (pack 14 "File not found"#, text_plain, 404)
    where pack l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s
          text_plain = pack 10 "text/plain"#

ngxExportUnsafeHandler 'fromFile
```

(Notice that a new *LANGUAGE* pragma *TemplateHaskell* was added and the
backslash before the *'fromFile* on the last line was removed). To compile this
as an independent library all dependent libraries such as *rts*, *base*, *safe*,
*file-embed* and their sub-dependencies must be compiled with flag *-fPIC* and
archived in static libraries. This is not an easy task considering that we do
not aim to replace the whole system *ghc* and installed packages.

So let's start with *rts*. The *rts* is not a haskell package but rather a part
of *ghc*, therefore we have to retrieve *ghc* sources from a branch that
corresponds to the version of the system *ghc*.

```ShellSession
$ git clone -b ghc-7.10.2-release --recursive git://git.haskell.org/ghc.git ghc-7.10.2
```

Now *cd* to the source directory and perform the first usual steps.

```ShellSession
$ cd ghc-7.10.2
$ ./boot
$ ./configure
```

Here we are going to do a trick. Static FFI library must be compiled with
*-fPIC* but *ghc* seems to not have a hook for this, so we must put the option
into the *CFLAGS* declaration in *libffi/ghc.mk* manually.

```ShellSession
$ sed -i 's/CFLAGS="/&-fPIC /' libffi/ghc.mk
```

Now we are ready to compile *rts*.

```ShellSession
$ cd rts
$ make EXTRA_HC_OPTS=-fPIC
```

Making *rts* takes a long time. After it's done we can check that the built
static libraries contain relocations.

```ShellSession
$ readelf --relocs dist/build/libCffi.a | egrep '(GOT|PLT|JU?MP_SLOT)'
$ readelf --relocs dist/build/libHSrts.a | egrep '(GOT|PLT|JU?MP_SLOT)'
```

(This method was found [here](http://stackoverflow.com/a/1351771/5655455)). If
these commands have produced long outputs then the libraries are good. Now we
must put them in a directory that will be passed to *ghc* while compiling the
final library. Let the directory be located in a related to *ghc* system path.
The following commands must be executed with a superuser privileges.

```ShellSession
# mkdir $(ghc --print-libdir)/static-fpic
# cp -r dist/build/ $(ghc --print-libdir)/static-fpic/rts
```

*Template-haskell* must also be built from here as soon as ghc seems to apply
some magic when building it and I do not manage to get a compatible static
archive when building from the list of dependent libraries as shown hereinafter.
Make sure that version to build corresponds to that of the system
*template-haskell* package (it holds true for most cases)!

```ShellSession
$ cd ../libraries/template-haskell
$ make EXTRA_HC_OPTS=-fPIC
```

Wait a bit and then copy the built artifacts to the directory *static-fpic*
(being a superuser).

```ShellSession
# cp -r dist-install/build/ $(ghc --print-libdir)/static-fpic/template-haskell
```

Now let's turn to haskell packages and their dependencies. *Cd* to a new
directory and try to track down all dependencies and sub-dependencies of
packages that we're going to use: *base*, *file-embed*, *template-haskell* (only
dependencies, not itself), *bytestring*, *safe* and *ngx-export*. To see
versions and dependencies of the installed packages command *ghc-pkg field* can
be used. For example

```ShellSession
$ ghc-pkg field base version,depends
version: 4.8.1.0
depends:
    builtin_rts ghc-prim-0.4.0.0-af16264bc80979d06e37ac63e3ba9a21
    integer-gmp-1.0.0.0-8e0f14d0262184533b417ca1f8b44482
$ ghc-pkg field bytestring version,depends
version: 0.10.6.0
depends:
    base-4.8.1.0-4f7206fd964c629946bb89db72c80011
    deepseq-1.4.1.1-8fb9688ae42216e388cee132aef3d148
    ghc-prim-0.4.0.0-af16264bc80979d06e37ac63e3ba9a21
    integer-gmp-1.0.0.0-8e0f14d0262184533b417ca1f8b44482
```

Package *base* in my system has version *4.8.1.0* and depends on packages
*ghc-prim* and *integer-gmp*, package *bytestring* has version *0.10.6.0* and
depends on packages *base*, *deepseq*, *ghc-prim* and *integer-gmp*. There could
be multiple *version* and *depends* clauses per single package: the safest way
to choose versions and dependencies is taking clauses with the latest version.
We must track dependencies down and collect all sub-dependencies recursively
(*deepseq* and its dependencies and sub-dependencies etc.). It looks boring and
I wish I knew an automatic way for such dependency tracking<sup>[1](#fn1)</sup>.
Finally the following list of libraries to build was collected (in an arbitrary
order): *ghc-prim*, *integer-gmp*, *deepseq*, *array*, *bytestring*,
*directory*, *filepath*, *file-embed*, *time*, *unix*, *pretty* and *safe* (I
excluded *base* and *ngx-export* from the list because they differ in the way
how they are built).

Let's first build and install package *base*<sup>[2](#fn2)</sup>.

```ShellSession
$ cabal get base-4.8.1.0
$ cd base-4.8.1.0
$ cabal configure --ghc-options=-fPIC -finteger-gmp2
$ cabal build
$ sudo cp -r dist/build $(ghc --print-libdir)/static-fpic/base
$ cd -
```

Then build and install the libraries from the dependency list shown above.

```ShellSession
$ export DEPPACKS=$(for p in ghc-prim integer-gmp deepseq array bytestring directory filepath file-embed time unix pretty safe ; do ghc-pkg field $p version | head -1 | cut -d' ' -f2 | sed "s/^/$p-/" ; done)
$ for p in $DEPPACKS ; do cabal get $p ; cd $p ; cabal configure --ghc-options=-fPIC ; cabal build ; cd - ; done
```

The next command requires a superuser privileges.

```ShellSession
# for p in $DEPPACKS ; do DEPDST=$(echo $p | sed 's/-\([0-9]\+\.\)*[0-9]\+$//') ; cp -r $p/dist/build $(ghc --print-libdir)/static-fpic/$DEPDST ; done
```

Now *cd* to the *ngx-export* source directory and do all the same.

```ShellSession
$ cd haskell/ngx-export
$ cabal configure --ghc-options=-fPIC
$ cabal build
$ sudo cp -r dist/build $(ghc --print-libdir)/static-fpic/ngx-export
```

At this moment all dependent libraries have been installed. Let's build
*ngx_haskell.so*.

```ShellSession
$ GHCSTATICLIBS=$(find $(ghc --print-libdir)/static-fpic -maxdepth 1 | sed 's/^/-L/')
$ ghc -O2 -shared -fPIC -o ngx_haskell.so $GHCSTATICLIBS -lHSrts -lCffi -lrt NgxHaskellUserRuntime.hs
[1 of 1] Compiling NgxHaskellUserRuntime ( NgxHaskellUserRuntime.hs, NgxHaskellUserRuntime.o )
ghc: panic! (the 'impossible' happened)
  (GHC version 7.10.2 for x86_64-unknown-linux):
	Loading archives not supported

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
```

Pull in the reins! Being unable to load archives seems too restrictive,
especially when it is cheatable.

```ShellSession
$ ghc -O2 -shared -fPIC -o ngx_haskell.so $GHCSTATICLIBS NgxHaskellUserRuntime.hs
[1 of 1] Compiling NgxHaskellUserRuntime ( NgxHaskellUserRuntime.hs, NgxHaskellUserRuntime.o )
Linking ngx_haskell.so ...
/usr/bin/ld: NgxHaskellUserRuntime.o: relocation R_X86_64_PC32 against undefined symbol `stg_newMutVarzh' can not be used when making a shared object; recompile with -fPIC
/usr/bin/ld: final link failed: Bad value
collect2: error: ld returned 1 exit status
$ ghc -O2 -shared -fPIC -o ngx_haskell.so $GHCSTATICLIBS -lHSrts -lCffi -lrt NgxHaskellUserRuntime.hs
Linking ngx_haskell.so ...
```

The library was built. Check that *ngx_haskell.so* does not depend on shared
haskell libraries.

```ShellSession
$ ldd ngx_haskell.so 
	linux-vdso.so.1 (0x00007ffca784d000)
	librt.so.1 => /lib64/librt.so.1 (0x00007f51e0681000)
	libutil.so.1 => /lib64/libutil.so.1 (0x00007f51e047d000)
	libdl.so.2 => /lib64/libdl.so.2 (0x00007f51e0279000)
	libpthread.so.0 => /lib64/libpthread.so.0 (0x00007f51e005c000)
	libgmp.so.10 => /lib64/libgmp.so.10 (0x00007f51dfde3000)
	libc.so.6 => /lib64/libc.so.6 (0x00007f51dfa22000)
	/lib64/ld-linux-x86-64.so.2 (0x000055e11c5c7000)
```

Yes, *ldd* shows only system *C* libraries. Install the library.

```ShellSession
$ cp ngx_haskell.so /tmp
```

Replace directive *haskell compile* in *nginx.conf* with directive

```nginx
    haskell load /tmp/ngx_haskell.so;
```

and finally run nginx with haskell code inside but without external dependencies
on *ghc* and haskell libraries!

<br><hr><a name="fn1"><sup>**1**</sup></a>&nbsp; There is a way! As it was
suggested
[here](https://www.reddit.com/r/haskell/comments/4my2cn/a_story_of_how_i_built_static_haskell_libraries/d4047uz),
all dependencies can be extracted from a shared library with command *ldd*.
Let's first make a shared library with name, say *libtmp.so*.

```ShellSession
$ ghc -O2 -dynamic -shared -fPIC -lHSrts-ghc$(ghc --numeric-version) -o libtmp.so NgxHaskellUserRuntime.hs
```

Now we can extract the list of all dependencies in a variable, say *DEPS*.

```ShellSession
$ DEPS=$(ldd libtmp.so | sed -r '/^\s*libHS/!d; s/^\s*libHS//; /^(rts|base|ngx-export|template-haskell)-/d; s/^(\S+)-([0-9]+\.){2,}.*/\1/')
```

<br><a name="fn2"><sup>**2**</sup></a>&nbsp; When using the newer *ghc 8.0.1*,
*cabal configure* may require an additional option *``--ipid=$(ghc-pkg field
base id | head -1 | cut -d' ' -f2)``*. This also refers to building other
dependent libraries and *ngx-export*. Values of *ipid* must be extracted from
system packages because different values will cause loading of the system
packages in place of their built counterparts, and consequently symbol
relocation errors when linking the final library.

Some facts about efficiency
---------------------------

- Advantages

    + The haskell library gets compiled at the very start of nginx and later
      loaded with *dlopen()* in every nginx worker process.
    + Nginx strings are passed to haskell exported functions as strings with
      lengths, no extra allocations are needed.
    + *Template Haskell* extension makes it possible to read files into the
      haskell code during *ghc* compilation (see sections [Static content in
      HTTP responses](#static-content-in-http-responses) and [Optimized unsafe
      content handler](#optimized-unsafe-content-handler)).

- Pitfalls

    + (This does not refer to *byte strings*.) Haskell strings are simple lists,
      they are not contiguously allocated (but on the other hand they are lazy,
      which usually means efficient).
    + Haskell exported functions of types *S_S*, *S_SS*, *S_LS* and *Y_Y* and
      *safe* content handlers allocate new strings with *malloc()* which get
      freed upon the request termination (or when the location configuration
      gets reloaded in case of *haskell_static_content* handler).
    + Haskell content handlers are not suspendable so you cannot use
      long-running haskell functions without hitting the overall nginx
      performance.

Some facts about exceptions
---------------------------

Haskell source code must preferably be pure and safe as soon as C code is known
to be unfamiliar with catching Haskell exceptions. That is why I used functions
from the *Safe* module in the above example. Causes of probable exceptions may
hide deeply in details. Innocuously looking function *matches* from the above
example not only is inefficient but also unsafe. The only reason for both the
problems is simple: *matches* accepts a regular expression in every single user
request meaning that it must be compiled every time again (inefficiency) and
having been wrongly composed it may lead to uncatchable compilation errors
(unsafety). An obvious solution that must fix the problems is to not allow users
passing regular expressions but create and compile them in the haskell code
instead. If it is not acceptable by some reasons and users still may send
regular expressions in requests then they must at least be checked against
compilation errors. To achieve this a lower level API functions *compile* and
*execute* are required (they are imported from module *Text.Regex.PCRE.String*).
Below is a safe version of *matches*.

```haskell
matches = (fromMaybe False .) . liftM2 safeMatch `on`
          (doURLDecode =<<) . toMaybe
    where safeMatch a b = unsafePerformIO $ do
            p <- compile compBlank execBlank b
            case p of
                Right x -> do
                    r <- execute x a
                    return $ case r of
                        Right (Just _) -> True
                        _ -> False
                _ -> return False
          toMaybe [] = Nothing
          toMaybe a  = Just a
```

Functions *compile* and *execute* expose IO monad: that is why the result of
*safeMatch* gets unwrapped with *unsafePerformIO* (imported from
*System.IO.Unsafe*). There is nothing bad about that in this particular case:
internally higher level API regex functions like *(=~)* and *match* do all the
same.

You may notice that function *jSONListOfIntsTakeN* is not safe too because of
using of *B.tail* in it. However the way it is used in nginx rules gives a
guarantee that the argument of *B.tail* will always have at least the *vertical
bar* character (*|*) at its head.

Troubleshooting
---------------

- _Haskell source code fails to compile with messages ``Not in scope: ‘<$>’``
  and ``Not in scope: ‘<*>’``_.

  This happens in *standalone module approach* with *ghc* older than *7.10* and
  can be fixed by adding line

    ```haskell
  import Control.Applicative  
    ```

  in the import list inside the haskell source code.

See also
--------

[*An article*](http://lin-techdet.blogspot.com/2015/12/nginx-module-to-enable-haskell-binding.html)
about the module in my blog.

