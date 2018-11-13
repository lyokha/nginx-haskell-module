Nginx Haskell module
====================

[![Build Status](https://travis-ci.org/lyokha/nginx-haskell-module.svg?branch=master)](https://travis-ci.org/lyokha/nginx-haskell-module)
[![Hackage](https://img.shields.io/hackage/v/ngx-export.svg?label=hackage%20%7C%20ngx-export)](https://hackage.haskell.org/package/ngx-export)
[![Hackage](https://img.shields.io/hackage/v/ngx-export-tools.svg?label=hackage%20%7C%20ngx-export-tools)](https://hackage.haskell.org/package/ngx-export-tools)
[![Doc](https://img.shields.io/badge/doc-yet_another_doc_with_examples-786D5F.svg)](https://nbviewer.jupyter.org/github/lyokha/nginx-haskell-module/blob/master/docs/yet-another-doc-with-examples/nginx-haskell-module-yadwe.pdf)

This Nginx module allows compiling and running Haskell source code found in a
configuration file or an existing shared library. It allows for writing in
Haskell synchronous variable handlers, asynchronous tasks, services (i.e.
asynchronous tasks that are not bound to requests), shared services (i.e.
services that work exclusively on a single Nginx worker process all the time),
content handlers and POST request handlers.

Table of contents
-----------------

- [Motivational example](#motivational-example)
- [Static content in HTTP responses](#static-content-in-http-responses)
- [Optimized unsafe content handler](#optimized-unsafe-content-handler)
- [Asynchronous tasks with side effects](#asynchronous-tasks-with-side-effects)
- [Asynchronous content handlers](#asynchronous-content-handlers)
- [Asynchronous services](#asynchronous-services)
- [Client request body handlers](#client-request-body-handlers)
- [Synchronous short circuit bang-handler](#synchronous-short-circuit-bang-handler)
- [Miscellaneous nginx directives](#miscellaneous-nginx-directives)
- [Service variables in shared memory and integration with other nginx modules](#service-variables-in-shared-memory-and-integration-with-other-nginx-modules)
- [Shared services and global states](#shared-services-and-global-states)
- [Service hooks](#service-hooks)
- [C plugins with low level access to the Nginx request object](#c-plugins-with-low-level-access-to-the-nginx-request-object)
- [Reloading of haskell code and static content](#reloading-of-haskell-code-and-static-content)
- [Wrapping haskell code organization](#wrapping-haskell-code-organization)
- [Static linkage against basic haskell libraries](#static-linkage-against-basic-haskell-libraries)
- [Debugging and tracing of haskell code](#debugging-and-tracing-of-haskell-code) 
- [Some facts about efficiency](#some-facts-about-efficiency)
- [Some facts about exceptions](#some-facts-about-exceptions)
- [Termination of nginx worker and asynchronous exception ThreadKilled](#termination-of-nginx-worker-and-asynchronous-exception-threadkilled)
- [Some facts about foreign functions that may block](#some-facts-about-foreign-functions-that-may-block)
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

    haskell ghc_extra_options
                -ignore-package regex-pcre
                -XFlexibleInstances -XMagicHash -XTupleSections;

    haskell compile standalone /tmp/ngx_haskell.hs '

import qualified Data.Char as C
import           Text.Regex.PCRE
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Unsafe
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Text.Pandoc
import           Text.Pandoc.Builder
import qualified Data.Text.Encoding as T
import           Data.Function (on)
import           Control.Monad
import           Control.Exception
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

-- compatible with Pandoc 2.0 (will not compile for older versions)
fromMd (T.decodeUtf8 -> x) = uncurry (, packLiteral 9 "text/html"#, ) $
    case runPure $ readMarkdown def x >>= writeHtml of
        Right p -> (fromText p, 200)
        Left (displayException -> e) -> (case runPure $ writeError e of
                                             Right p -> fromText p
                                             Left  _ -> C8L.pack e, 500)
    where packLiteral l s =
              accursedUnutterablePerformIO $ unsafePackAddressLen l s
          fromText = C8L.fromStrict . T.encodeUtf8
          writeHtml = writeHtml5String defHtmlWriterOptions
          writeError = writeHtml . doc . para . singleton . Str
          defHtmlWriterOptions = def
              { writerTemplate = Just "<html>\\n<body>\\n$body$</body></html>" }
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
two macros that deal with *bytestrings*: *NGX_EXPORT_Y_Y* and *NGX_EXPORT_B_Y*
(*Y* stands for *bYte*). For the sake of efficiency, bytestring macros accept
*strict* but return (only *Y_Y*) *lazy* bytestrings. Effectively this means that
only those functions are supported that return strings, bytestrings or booleans
and accept one, two or more (only *S_LS* and *B_LS*) string arguments or one
bytestring.

In this example 10 custom haskell functions are exported: *toUpper*, *takeN*,
*reverse* (which is normal *reverse* imported from *Prelude*), *matches*
(which requires module *Text.Regex.PCRE*), *firstNotEmpty*, *isInList*,
*isJSONListOfInts*, *jSONListOfIntsTakeN*, *urlDecode* and *toYesNo*. In my case
this code won't compile due to ambiguity involved by presence of the two
installed packages *regex-pcre* and *regex-pcre-builtin*, so I had to add an
extra *ghc* compilation flag *-ignore-package regex-pcre* using directive
*haskell ghc_extra_options*. Other flags include *-XFlexibleInstances* which
allows declaration of *instance UrlDecodable String*. Class *UrlDecodable*
provides function *doURLDecode* for decoding strings and bytestrings that was
adopted from [here](http://www.rosettacode.org/wiki/URL_decoding#Haskell). The
bytestring instance of *doURLDecode* makes use of *view patterns* in its
clauses, however this extension does not have to be declared explicitly because
it was already enabled in a pragma from the wrapping haskell code provided by
this module (see details in section [Wrapping haskell code
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
``$hs_a`` which is not a good habit for nginx configuration files. I only wanted
to show that upper nginx configuration levels being merged with lower levels
behave as normally expected.

There is another haskell directive *haskell_content* which accepts a haskell
function to generate HTTP response and an optional string that will be passed to
the function. The function must be of one of the two types:
*strictByteString-to-lazyByteString* and
*strictByteString-to-3tuple(lazyByteString,strictByteString,Int)*. It must be
exported with *NGX_EXPORT_DEF_HANDLER* (*default* content handler) in the first
case and *NGX_EXPORT_HANDLER* in the second case. The elements in the *3tuple*
correspond to returned content, its type (e.g. *text/html* etc.) and HTTP
status. Default content handler sets content type to *text/plain* and HTTP
status to *200*. Directive *haskell_content* is allowed in *location* and
*location-if* clauses of the nginx configuration. In the location */content*
from the above example the directive *haskell_content* makes use of a haskell
function *fromMd* to generate HTML response from a markdown text. Function
*fromMd* translates a markdown text to HTML using Pandoc library. Notice that
content type is built from a string literal with a *magic hash* at the end to
avoid unnecessary expenses (see details about using string literals in section
[Optimized unsafe content handler](#optimized-unsafe-content-handler)).

What about doing some tests? Let's first start nginx (in this example, from the
directory where file *nginx.conf* is located).

```ShellSession
# nginx -c`pwd`/nginx.conf
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
directive *haskell ghc_extra_options* and the module *Data.FileEmbed* must be
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

*Notice that starting from version 1.3 of this module, all content handlers do
not pass copies to the C side! Instead, the underlying lazy bytestrings share
their contents with nginx. So all the reasons about extra copying below are no
longer actual.*

Let's go back to the example from the previous section. All the content handlers
we met so far receive a copy of data produced in haskell handlers. Using
references to the original data would lead to nasty things after haskell's
garbage collector wakeup, so the only *safe* choice seems to be copying the
original data<sup>[1](#fnuch1)</sup>. Handler *fromFile* from the example takes
static data embedded into the haskell library by *Data.FileEmbed*, makes a copy
of this and passes it to the C code. It runs once per location during location
configuration lifetime thanks to the directive *haskell_static_content*
implementation. Nonetheless there are two duplicate static data copies in the
program during its run which looks wasteful. It can get even worse when using
*haskell_static_content* is not an option.

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
        Just p  -> (p,                                text_plain, 200)
        Nothing -> (packLiteral 14 "File not found"#, text_plain, 404)
    where packLiteral l s = unsafePerformIO $ unsafePackAddressLen l s
          text_plain = packLiteral 10 "text/plain"#
NGX_EXPORT_UNSAFE_HANDLER (fromFile)
```

*Corresponding nginx location.*

```nginx
        location /static {
            haskell_unsafe_content fromFile $uri;
        }
```

The *unsafe* handler returns *3tuple(strictByteString,strictByteString,Int)*.
The two strict bytestrings in it must correspond to the *really* static data,
i.e. string literals like *"File not found"#*, *"text/plain"#* and those
embedded by the *Data.FileEmbed*, otherwise the nasty things may happen! Literal
strings that end with *hashes* (*#*) are actually addresses of compiled static
byte arrays that do not change during runtime. To enable the hash literals
option *-XMagicHash* must be added into the directive *haskell
ghc_extra_options*. Working on such a low level requires using functions
*unsafePackAddressLen* and *unsafePerformIO* from modules
*Data.ByteString.Unsafe* and *System.IO.Unsafe* respectively (in this example
*unsafePerformIO* can be safely replaced with the fastest and the unsafest *IO
unwrapper* *accursedUnutterablePerformIO* from module
*Data.ByteString.Internal*). Minimum requirements for using static byte arrays
in the module *Data.FileEmbed* are: *file-embed* version *0.0.7* and *Template
Haskell* version *2.5.0* (bundled with *ghc* since version *7.0.1*).

The unsafe content handler implementation from the above example can be found in
file [test/tsung/nginx-static.conf](test/tsung/nginx-static.conf).

<br><hr><a name="fnuch1"><sup>**1**</sup></a>&nbsp; Did you read the notice in
the beginning of the section? Yes, lazy bytestrings contents can be safely
passed to the C side directly, provided *stable pointers* (*StablePtr*) to them
are passed too. Creating a stable pointer to a bytestring makes it a *root
object* that is guaranteed not to be garbage collected while the pointer is not
freed. The bytestring *itself* can be relocated, but its buffers not! They are
stored in *pinned memory arrays* that are not moved while the bytestring is
alive.

Asynchronous tasks with side effects
------------------------------------

All variable handlers we met so far were *pure* haskell functions without side
effects. Inability to put side effects into pure functions has a great
significance in the sense that it gives strong guarantees about the time the
functions run. In haskell, functions that may produce side effects are normally
wrapped inside IO monad. They can do various non-deterministic IO *computations*
like reading or writing files, connecting to network servers etc., which, in
principle, may last unpredictably long or even eternally. Despite this, having
IO functions as nginx variable handlers are extremely tempting as it makes
possible to perform arbitrary IO tasks during an HTTP request. To eliminate
their non-probabilistic duration downside, they could be run *asynchronously* in
*green threads* provided by the haskell *RTS* library, and somehow signal the
nginx worker's main thread after their computations finish. This is exactly what
happens in special handler *NGX_EXPORT_ASYNC_IOY_Y*. Consider the following
example.

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    haskell compile threaded standalone /tmp/ngx_haskell.hs '

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Network.HTTP.Client
import           Control.Concurrent
import           Control.Exception
import           Safe

catchHttpException = (`catch` \e ->
        return $ C8L.pack $ "HTTP EXCEPTION: " ++ show (e :: HttpException))

getResponse (C8.unpack -> url) = fmap responseBody . (parseRequest url >>=)

getUrl url = do
    man <- newManager defaultManagerSettings
    catchHttpException $ getResponse url $ flip httpLbs man
NGX_EXPORT_ASYNC_IOY_Y (getUrl)

threadDelaySec = threadDelay . (* 10^6)

delay (readDef 0 . C8.unpack -> v) =
    threadDelaySec v >> return (C8L.pack $ show v)
NGX_EXPORT_ASYNC_IOY_Y (delay)

    ';

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        location / {
            haskell_run_async getUrl $hs_async_ya
                    "http://ya.ru";
            haskell_run_async getUrl $hs_async_httpbin
                    "http://httpbin.org";
            haskell_run_async getUrl $hs_async_hackage
                    "http://hackage.haskell.org";
            echo "------> YA.RU:\n\n$hs_async_ya\n";
            echo "------> HTTPBIN.ORG:\n\n$hs_async_httpbin\n";
            echo "------> HACKAGE.HASKELL.ORG:\n\n$hs_async_hackage";
        }

        location /rewrite {
            rewrite ^ / last;
        }

        location /delay {
            haskell_run_async delay $hs_async_elapsed $arg_a;
            echo "Elapsed $hs_async_elapsed seconds";
        }
    }
}
```

Notice that the haskell code was compiled with flag *threaded* which is
important for running asynchronous tasks. Function *getUrl* is an HTTP client
that returns the response body or a special message if an HTTP exception has
happened. Inside *location /* there are 3 directives *haskell_run_async* which
spawn 3 asynchronous tasks run by *getUrl*, and bind future results to 3
different variables accessed later by directives *echo* in the nginx *content
phase*. Async variable handlers are very special. In fact, the IO task gets
spawned even if the bound variable is not accessed anywhere. All the tasks are
spawned during early nginx *rewrite phase* (before all rewrite directives) or
late *rewrite phase* (when all location rewrites are done: this ensures that all
tasks in the final rewritten location will run). The request won't proceed to
later phases until all async tasks are done. Technically, an async task signals
the main nginx thread when it finishes by writing a byte into the *write-end*
file descriptor of a dedicated *self-pipe*. The *read-end* file descriptor of
the pipe are polled by the nginx event poller (normally *epoll* in Linux). When
a task is finished, the poller calls a special callback that checks if there are
more async tasks for this request and spawns the next one or finally finishes
the rewrite phase handler by returning *NGX_DECLINED*. Sequencing of tasks makes
it possible to use computed results of early tasks as input values in later
ones. *N.B.*: starting from version *1.1* the *self-pipe* technique is replaced
with more efficient *eventfd* channels if possible.

All types of exceptions are caught inside async handlers. If an exception has
happened, the async handler writes its message in the bound variable's data,
whereas the variable handler logs it when accessed. However, for better control,
you may want to catch exceptions inside your code like in the *getUrl*.

Let's do some tests.

```ShellSession
$ curl 'http://localhost:8010/'
```

Here you will see too long output with the 3 http sites content, I don't show it
here. Let's run 20 requests simultaneously.

```ShellSession
$ for i in {1..20} ; do curl -s 'http://localhost:8010/' & done
```

20 times longer output! Let's make a timer for 20 seconds from 20 parallel
requests.

```ShellSession
$ for i in {1..20} ; do curl -s "http://localhost:8010/delay?a=$i" & done
Elapsed 1 seconds
Elapsed 2 seconds
Elapsed 3 seconds
Elapsed 4 seconds
Elapsed 5 seconds
Elapsed 6 seconds
Elapsed 7 seconds
Elapsed 8 seconds
Elapsed 9 seconds
Elapsed 10 seconds
Elapsed 11 seconds
Elapsed 12 seconds
Elapsed 13 seconds
Elapsed 14 seconds
Elapsed 15 seconds
Elapsed 16 seconds
Elapsed 17 seconds
Elapsed 18 seconds
Elapsed 19 seconds
Elapsed 20 seconds
```

Make sure it prints out every one second: this marks that requests are processed
asynchronously!

In the second test we ran 20 HTTP requests simultaneously, but could run
hundreds and thousands! Some servers may reject so many requests at once
(despite the fact that the manager from the *Network.HTTP.Client* is so advanced
that it can share a single connection to the same host between all requests
provided it was defined at the top level like

```haskell
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getUrl url = catchHttpException $ getResponse url $ flip httpLbs httpManager
```

). Fortunately, we can limit number of simultaneous requests with *semaphores*.
Let's make a semaphore that allows only 1 task at once.

```haskell
sem1 = unsafePerformIO $ S.new 1
{-# NOINLINE sem1 #-}
```

Functions *unsafePerformIO* and *new* must be imported from modules
*System.IO.Unsafe* and *Control.Concurrent.MSem* (qualified as *S*)
respectively. This code looks ugly, nevertheless it is safe and will work as
expected in our new async handlers *getUrl1* and *delay1*.

```haskell
getUrl1 url = do
    man <- newManager defaultManagerSettings
    catchHttpException $ getResponse url $ S.with sem1 . flip httpLbs man
NGX_EXPORT_ASYNC_IOY_Y (getUrl1)

delay1 (readDef 0 . C8.unpack -> v) =
    S.with sem1 (threadDelaySec v) >> return (C8L.pack $ show v)
NGX_EXPORT_ASYNC_IOY_Y (delay1)
```

Put the new handlers in locations */* and */delay* and make the *20-requests*
tests again to see how they change the async behavior. For example, responses
from location */delay* must become so long as if they were not run
asynchronously, however they must be finishing not in order. Be aware that
*sem1* is shared between all async handlers that use it, this means that
simultaneous requests to locations */* and */delay* will probably wait for each
other: use different semaphores for different handlers when it is not desirable.

Asynchronous content handlers
-----------------------------

Effectful code in content handlers is not permitted because they are all *pure*
functions. We could emulate effects in a content handler by combining the latter
with an asynchronous task like in the following example.

```nginx
        location /async_content {
            haskell_run_async getUrl $hs_async_httpbin "http://httpbin.org";
            haskell_content echo $hs_async_httpbin;
        }
```

Here *echo* is a simple content handler that echoes its argument. This approach
has at least two deficiencies related to performance and memory usage. The
content may be huge and chunked, and its chunks could have been naturally used
in the content handler. However, here the chunks get collected by directive
*haskell_run_async* into a single chunk, and then passed to the content handler
*echo*. The other problem deals with *eagerness* of asynchronous tasks. Imagine
that we put in the location a rewrite to another location: handler *getUrl* will
run before redirection, but variable ``$hs_async_httpbin`` will never be used
because we'll get out from the current location.

Asynchronous content handlers may have two types:
*strictByteString-to-IO(3tuple(lazyByteString,strictByteString,Int))* and
*lazyByteString-then-strictByteString-to-IO(3tuple(lazyByteString,strictByteString,Int))*.
They are declared with directives *haskell_async_content* and
*haskell_async_content_on_request_body* respectively. The type of the first
variant corresponds to that of the normal content handler, except it runs in the
*IO Monad*, the second variant accepts additionally request body chunks in its
first argument. The task starts from the content handler asynchronously, and the
lazy bytestring from the *3tuple* &mdash; the contents &mdash; gets used in the
task as is, with all of its originally computed chunks.

The *echo*-example with an async content handler will look like the following.

```haskell
getUrlContent url = (, packLiteral 9 "text/html"#, 200) <$> getUrl url
    where packLiteral l s =
              accursedUnutterablePerformIO $ unsafePackAddressLen l s
NGX_EXPORT_ASYNC_HANDLER (getUrlContent)
```

```nginx
        location /async_content {
            haskell_async_content getUrlContent "http://httpbin.org";
        }
```

Asynchronous services
---------------------

Starting an async task that normally returns identical result on every new
request may be unnecessarily expensive. In the example from the previous
section, function *getUrl* must presumably return the same value during a long
period of time (days, months or even years). For this case there is another
handler *NGX_EXPORT_SERVICE_IOY_Y* that runs an async task as a service. Let's
put the following service function inside our haskell code.

```haskell
getUrlService url firstRun = do
    unless firstRun $ threadDelaySec 20
    getUrl url
NGX_EXPORT_SERVICE_IOY_Y (getUrlService)
```

(For function *unless* module *Control.Monad* must be additionally imported.)
Function *getUrlService* accepts two arguments, the second is a boolean value
that denotes whether the service runs for the first time: it is supposed to be
used to skip *threadDelay* on the first run. Using *threadDelay* in a service
task is very important, because without any delay nginx will restart it very
often.

Let's start *getUrlService*.

```nginx
    haskell_run_service getUrlService $hs_service_ya "http://ya.ru";
    haskell_run_service getUrlService $hs_service_httpbin "http://httpbin.org";
```

Directives *haskell_run_service* must locate in the *http* clause of the nginx
configuration after directive *haskell compile*. In contrast with other types of
handlers, service handlers cannot refer to variables in their arguments as soon
as nginx variable handlers always refer to a request which is not possible here.

Put locations for showing data collected by the services and we are done.

```nginx
        location /ya {
            echo $hs_service_ya;
        }

        location /httpbin {
            echo $hs_service_httpbin;
        }
```

Please notice, that service handlers can be interrupted at any point of their
execution when a worker exits. This means that they cannot be used when reliable
transactional semantics is required, e.g. when they have to write into a file
for persistency<sup>[1](#fnat1)</sup>. To work this around, a *callback
location* (see details about the service variable update callback approach in
[this
section](#service-variables-in-shared-memory-and-integration-with-other-nginx-modules))
with a *synchronous* IO handler (see the next paragraphs) can be declared: the
handler would write into the file reliably when it is called after a shared
memory segment, that corresponds to the shared service variable, gets updated.

Complex scenarios may require synchronous access to handlers with side effects.
For example it could be an ad-hoc *error_page* redirection loop: asynchronous
handlers do not suit here very well. For such cases another handler
*NGX_EXPORT_IOY_Y* may appear useful. Below is a toy example of a synchronous
handler declaration.

```haskell
getIOValue = const $ return $ C8L.pack "HELLO WORLD!"
NGX_EXPORT_IOY_Y (getIOValue)
```

You can find all the examples shown here in file
[test/tsung/nginx-async.conf](test/tsung/nginx-async.conf).

<br><hr><a name="fnat1"><sup>**1**</sup></a>&nbsp; Starting from version *1.4.3*
of the module you can use technique of catching *ThreadKilled* exception for
performing persistency and cleanup actions on a worker's exit. See details in
section [Termination of nginx worker and asynchronous exception
ThreadKilled](#termination-of-nginx-worker-and-asynchronous-exception-threadkilled).

Client request body handlers
----------------------------

There is another type of asynchronous handler declared with macro
*NGX_EXPORT_ASYNC_ON_REQ_BODY*. It accepts two bytestrings: the first, *lazy*
bytestring, is the client request's body buffers, the second, *strict*
bytestring, is the user parameter like in normal asynchronous handlers. The
request body handler returns its result in a *lazy* bytestring like normal
asynchronous handlers. It is possible to declare multiple request body handlers
and mix them with other asynchronous handlers within a whole hierarchy of levels
(formed by *server*, *location* and *location-if* clauses). Below is an example
from [test/tsung/nginx-async.conf](test/tsung/nginx-async.conf).

```haskell
reqBody = const . return
NGX_EXPORT_ASYNC_ON_REQ_BODY (reqBody)

reqHead a n = return $ C8L.concat $ take (readDef 0 $ C8.unpack n) $
    map (`C8L.append` C8L.pack "\\n") $ C8L.lines a
NGX_EXPORT_ASYNC_ON_REQ_BODY (reqHead)

reqFld a fld = return $ maybe C8L.empty C8L.tail $
    lookup (C8L.fromStrict fld) $ map (C8L.break (== \'=\')) $
    C8L.split \'&\' a
NGX_EXPORT_ASYNC_ON_REQ_BODY (reqFld)
```

```nginx
        location /rb {
            client_body_buffer_size 100k;
            haskell_run_async_on_request_body reqBody $hs_rb noarg;
            haskell_run_async_on_request_body reqFld $hs_rb_fld $arg_a;
            haskell_run_async_on_request_body reqHead $hs_rb_head $arg_a;
            echo ">>> BODY\n";
            echo $hs_rb;
            echo ">>> BODY HEAD $arg_a\n";
            echo $hs_rb_head;
            echo ">>> FIELD $arg_a\n";
            echo $hs_rb_fld;
        }
```

In location */rb* three request body handlers are declared: *reqBody*, *reqFld*
and *reqHead*. Handler *reqBody* returns the client request body as is, *reqFld*
extracts a field value from a posted form, and *reqHead* extracts a given number
of lines from the request body. This example is a bit artificial because after
extraction of the whole request body in the first async handler, there is little
sense in running other handlers in async way: calculation of head and extraction
of a form field can be carried out by synchronous *pure* handlers on the
calculated value of ``$hs_rb``.

Returning a large client request body in a handler like *reqBody* may have a
small overhead when converting original request buffers into a haskell
bytestring. When only the whole request body is needed, there is a more
practical way that makes use of a standard nginx variable ``$request_body`` and
a simple haskell handler that returns an empty string, while nevertheless
ensures that reading of the request body is triggered. Below is an example.

```haskell
reqBodyTouch = (return .) . const . return C8L.empty
NGX_EXPORT_ASYNC_ON_REQ_BODY (reqBodyTouch)
```

```nginx
        location /rb/touch {
            client_body_buffer_size 100k;
            haskell_run_async_on_request_body reqBodyTouch $hs_dummy noarg;
            if ($request_body) {
                echo $request_body;
                break;
            }
            echo Fail;
        }
```

This example shows as well that ``$request_body`` is available in the nginx
rewrite module directives such as *if* and *rewrite*.

Synchronous short circuit bang-handler
--------------------------------------

There is a special synchronous handler with predefined name *!*, which has no
definition in the haskell code because it does not run any haskell at all! It
merely sets its single argument's value to its variable's value. This
functionality is almost equal to what common nginx directive *set* does, except
the value gets cached after the first evaluation. This can be useful for caching
internal *no-cacheable* variables such as ``$args`` and ``$is_args``.

```nginx
    server {

        # ...

        haskell_run ! $hs_is_args $args;
        set $add_is_args '?';

        if ($hs_is_args) {
            set $add_is_args '&';
        }

        location /fallback {

            # ...

            proxy_pass http://$backend$request_uri${add_is_args}t=v;
        }
```

In this example, if *location /fallback* must trigger after *error_page*
redirection then, in case when clause *if* on the *server* level would have been
testing variable ``$is_args`` instead of ``$hs_is_args``, variable
``$add_is_args`` would erroneously be equal to *?* because internal redirection
resets variables ``$args`` and ``$is_args``.

To reliably cache a *no-cacheable* variable at the beginning of a request, the
*if* check against its haskell counterpart on the *server* level is a
must: this enforces strict evaluation of the variable. Empty *if* clauses are
permitted, so basically it should look like in the following example.

```nginx
    server {

        # ...

        # variable $request_method can be reset in redirection by error_page,
        # in variable $hs_request_method we want to store its original value
        haskell_run ! $hs_request_method $request_method;
        if ($hs_request_method) {}
```

The bang handler can also be used for short-circuit assignment of a variable
normally evaluated by a haskell handler on some other level of nginx
configuration hierarchy.

Miscellaneous nginx directives
------------------------------

- *haskell_var_nocacheable ``<list>``* &mdash; Makes variables in the
  *``<list>``* no cacheable between internal redirections but cacheable inside a
  single redirection. Accepts all types of variables, not only defined with
  directives from this module, however foreign variables are not guaranteed to
  use cache within a single redirection.

- *haskell_var_compensate_uri_changes ``<list>``* &mdash; Makes variables in the
  *``<list>``* compensate decrement of nginx internal *uri* counter on every
  internal redirection, thus making it possible to enjoy unlimited redirection
  loops. Accepts only variables defined with directive *haskell_run*.

The two directives above make internal redirections with *error_page*
*Turing-complete* computations by allowing potentially infinite location loops
with a no-cacheable condition test variable.

- *haskell_service_var_ignore_empty ``<list>``* &mdash; Makes variables in the
  *``<list>``* do not update when related services return empty strings. Accepts
  only variables defined with directive *haskell_run_service*.

- *haskell rts_options ``<list>``* &mdash; Passes options from the *``<list>``*
  to the haskell *RTS* when a worker starts.

- *haskell program_options ``<list>``* &mdash; Passes options from the
  *``<list>``* as program options. This is just another way to pass simple
  static data to the user haskell library. Inside the library data can be
  accessed with *cmdargs* or other tools that work with program options.

All the directives listed so far are allowed only in the *http* clause of the
nginx configuration.

- *haskell_request_body_read_temp_file ``<on/off>``* &mdash; Makes haskell
  handlers that require request body, read from a *temporary file* where the
  body has been buffered by nginx. Allowed in *server*, *location* and
  *location-if* clauses. If not set then buffered request bodies are not read in
  haskell handlers.

Service variables in shared memory and integration with other nginx modules
---------------------------------------------------------------------------

There are other two nginx directives that allow organizing communication between
the haskell module and other nginx modules by setting a special *callback*
location with a handler from the other module bound to a haskell service
variable. Here they are.

- *haskell_service_var_in_shm ``shm_name`` ``shm_size`` ``file_locks_path``
  ``<list>``* &mdash; Makes that variables in the *``<list>``* get stored in
  shared memory ``shm_name`` with size ``shm_size``. *File locks* for specific
  variables will be written in directory ``file_locks_path`` which must be
  accessible for writing from worker processes. Accepts only variables defined
  with directive *haskell_run_service*. This directive by itself can be used to
  build *shared services* when only one worker process runs the service while
  others wait.

- *haskell_service_var_update_callback ``service`` ``$var`` ``[value]``* &mdash;
  This directive is similar to *haskell_run_service* and accepts a haskell
  function ``service`` of the same type *NGX_EXPORT_SERVICE_IOY_Y*, however
  other arguments ``$var`` and ``value`` have different meanings. Variable
  ``$var`` must be listed in a directive *haskell_service_var_in_shm*. The
  function ``service`` will run every time when value of the ``$var`` which is
  allocated in a shared memory gets an update, ``service`` receives the new
  value of the ``$var`` or the ``value`` if it was defined.

This integration model requires that service variables are stored in a shared
memory because there could be multiple nginx worker processes and any of them
could receive a request for running the callback function.

See an example of using this approach in
[examples/dynamicUpstreams](examples/dynamicUpstreams).

This approach can also be used to provide reliable transactional semantics for
service handlers.

Shared services and global states
---------------------------------

Starting from version *1.4* of this module, all services in which corresponding
variables are listed in directive *haskell_service_var_in_shm* became *shared*.
This means that only one worker runs such a service while in other workers
identical services wait on file locks. This let drastically reduce involved
resources when many workers are running. However, this approach has downsides as
well. When all services run on all workers, they may have global states where
they can store data retrieved from the outer world. This data could be used in
processing client requests. Shared services cannot rely upon global states
because there is only one worker that continuously updates its global state.

Let's try to figure out how services on all workers could obtain the valid state
to process client requests at any time. The active worker could store the global
state in the service variable resided in shared memory and thus available in all
workers. When a client would come to a worker, it could pass the variable to the
haskell side in a handler. But this would be very inefficient. In this approach
the variable's contents must be copied from shared memory and interpreted in the
haskell handler on every request! Which is basically unnecessary because service
data do not change every millisecond. To mitigate this problem, special *update
variables* are created to accompany every service variable in shared memory.
They have names with prefix *\_upd\_\_* and evaluate to the corresponding
service variable value only when its data really changes. Being passed to
haskell handlers, their non-empty values may be used to update global states.
However, they require careful treatment. It is better to use a separate handler
that only updates the global state and does not do anything else, otherwise
uncaught exceptions from custom code may ruin the change of the state. This
handler must return an empty string in all cases. This returned empty value must
be *glued* to the value of the payload handler that requires the updated global
state.

Here is an example.

*Nginx configuration.*

```nginx
    haskell load /var/lib/nginx/haskell_lib.so;

    haskell_run_service getDataFromOuterWorld $hs_shared_data;

    haskell_service_var_in_shm outer_world 64k /tmp $hs_shared_data;

    # ...

        location /requires_valid_global_state {
            haskell_run updateGlobalState $_upd_ $_upd__hs_shared_data;
            haskell_run payloadProcess $hs_result "${_upd_}<payload if any>";
            echo $hs_result;
        }
```

*Code from the haskell library.*

```haskell
globalState :: IORef GlobalStateType
globalState = unsafePerformIO $ newIORef GlobalStateTypeCons
{-# NOINLINE globalState #-}

-- ...

getDataFromOuterWorld s firstRun = do
    -- retrieving data from outer world
    -- nginx will store it in $hs_shared_data variable in shared memory
ngxExportServiceIOYY 'getDataFromOuterWorld

updateGlobalState (B.null -> True) = return L.empty
updateGlobalState s = do
    maybe (return ()) (writeIORef globalState) $ decodeStrict s
    return L.empty
ngxExportIOYY 'updateGlobalState

payloadProcess s = do
    -- useful processing of s that requires globalState to be valid
ngxExportIOYY 'payloadProcess
```

In this example the global state is stored in some way in variable
``$hs_shared_data`` resided in shared memory. It is supposed that all requests
in location */requires_valid_global_state* depend on the valid global state.
This state gets updated before running handler *payloadProcess* in
*updateGlobalState* when *update variable* ``$_upd__hs_shared_data`` is not
empty. Handler *updateGlobalState* updates the global state and returns an empty
string in ``$_upd_``. Gluing its value to the payload data passed in
*payloadProcess* ensures that the global state has been updated.

Service hooks
-------------

Service hooks are special *synchronous* Haskell handlers of type
*strictByteString-to-IO(lazyByteString)* that can be used to interact with
running services (both *per-worker* and *shared*). This interaction solely
depends on implementation of the hook and the bound service, and may include
*stop* and *(re)start* of the service, changing its parameters or complete
replacement of its logic. Behind the scenes, declaration of a service hook
installs a *content handler* which signals all the workers when requested using
an event channel. Then in the event handler workers run the hook supplying it
with data, which was preliminary saved in the temporary storage, that had been
declared with directive *haskell_service_hooks_zone*. Hooks are supposed to
change some global state and immediately return. After this, workers interrupt
*active* services with an *asynchronous exception* *ServiceHookInterrupt*, that
makes them restart and read new contents from the global state.

Below is an example.

```nginx
    haskell_service_hooks_zone hooks 32k;

    # ...

        location /httpbin/url {
            allow 127.0.0.1;
            deny all;
            haskell_service_hook getUrlServiceHook $hs_service_httpbin $arg_v;
        }
```

Beware that setting shared zone for service hooks is not mandatory. It is only
needed when they pass data in global states. In this example service hook
*getUrlServiceHook* passes data found in variable ``$arg_v``.

```haskell
getUrlServiceLink :: IORef (Maybe ByteString)
getUrlServiceLink = unsafePerformIO $ newIORef Nothing
{-# NOINLINE getUrlServiceLink #-}

getUrlServiceLinkUpdated :: IORef Bool
getUrlServiceLinkUpdated = unsafePerformIO $ newIORef True
{-# NOINLINE getUrlServiceLinkUpdated #-}

getUrlService :: ByteString -> Bool -> IO L.ByteString
getUrlService url = const $ do
    url' <- fromMaybe url <$> readIORef getUrlServiceLink
    updated <- readIORef getUrlServiceLinkUpdated
    atomicWriteIORef getUrlServiceLinkUpdated False
    unless updated $ threadDelay $ 20 * 1000000
    getUrl url'
ngxExportServiceIOYY 'getUrlService

getUrlServiceHook :: ByteString -> IO L.ByteString
getUrlServiceHook url = do
    writeIORef getUrlServiceLink $ if B.null url
                                       then Nothing
                                       else Just url
    atomicWriteIORef getUrlServiceLinkUpdated True
    return $ if B.null url
                 then "getUrlService reset URL"
                 else L.fromChunks ["getUrlService set URL ", url]
ngxExportServiceHook 'getUrlServiceHook
```

Now we can change the URL for service *getUrlService* in runtime by sending a
simple request like

```ShellSession
$ curl 'http://127.0.0.1:8010/httpbin/url?v=http://example.com'
```

Starting from version *1.8.4*, service hooks can be used as an alternative
implementation of *update variables* for shared services. For this, directive
*haskell_service_update_hook* in the *http* clause of the nginx configuration
must be used. Here is an example.

```nginx
    haskell_service_update_hook getUrlServiceUpdateHook $hs_service_httpbin;
```

```haskell
getUrlServiceData :: IORef L.ByteString
getUrlServiceData = unsafePerformIO $ newIORef L.empty
{-# NOINLINE getUrlServiceData #-}

getUrlServiceUpdateHook :: ByteString -> IO L.ByteString
getUrlServiceUpdateHook v = do
    writeIORef getUrlServiceData $ L.fromStrict v
    return L.empty
ngxExportServiceHook 'getUrlServiceUpdateHook
```

Update hooks have at least 3 advantages over update variables.

1. No need for obscure treatment of update variables in configuration files.
2. No need for copying the original argument: its data is freed on the haskell
   side.
3. Nginx don't need to access shared memory on every single request for checking
   if the service data has been altered.

There is also a subtle difference with update variables. As soon as with update
hooks new service variable data is propagated to worker processes asynchronously
via an event channel, there always exists a very short transient period between
the moments when the service variable gets altered in shared memory and the
global state gets updated in a worker, during which events related to client
requests may occur.

Service update hooks can also be used to replace service *update callbacks*.
Indeed, being run *synchronously* from an event handler, a service hook could
safely call a C function which would acquire related to Nginx context from the
global Nginx variable *ngx_cycle* (which is accessible from the Haskell part via
function *ngxCyclePtr*) for doing a variety of low level actions. Notice that
unlike update callbacks, service hooks get triggered in all worker processes.

C plugins with low level access to the Nginx request object
-----------------------------------------------------------

Serialized pointer to the Nginx *request object* is accessible via a special
variable ``$_r_ptr``. Haskell handlers have no benefit from this because they do
not know how the request object is built. However they may run C code that is
compiled with this knowledge. The low level access to the Nginx request data
allows for making things not available without this. As soon as a C plugin can
do whatever a usual Nginx module can, using it from a Haskell handler must be
very cautious. All synchronous and asynchronous Haskell handlers can access the
Nginx request object and pass it to a C plugin. Using a C plugin in asynchronous
context has not been investigated and is probably dangerous in many aspects.
After all, an Nginx worker is a single-threaded process, and available Nginx
tools were not designed for using in multi-threaded environments. As such, using
C plugins in asynchronous Haskell handlers must be regarded as strictly
experimental!

Let's write a plugin that will insert into the request HTTP headers a header
*X-Powered-By*.

C header file *test_c_plugin.h*.

```c
#ifndef NGX_HTTP_HASKELL_TEST_C_PLUGIN_H
#define NGX_HTTP_HASKELL_TEST_C_PLUGIN_H

#include <ngx_core.h>
#include <ngx_http.h>

ngx_int_t ngx_http_haskell_test_c_plugin(ngx_http_request_t *r);

#endif
```

C source file *test_c_plugin.c*.

```c
#include "test_c_plugin.h"

static const ngx_str_t haskell_module = ngx_string("Nginx Haskell module");

ngx_int_t
ngx_http_haskell_test_c_plugin(ngx_http_request_t *r)
{
    ngx_table_elt_t  *x_powered_by;

    if (r == NULL) {
        return NGX_ERROR;
    }

    x_powered_by = ngx_list_push(&r->headers_out.headers);

    if (!x_powered_by) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "Unable to allocate memory to set X-Powered-By header");
        return NGX_ERROR;
    }

    x_powered_by->hash = 1;
    ngx_str_set(&x_powered_by->key, "X-Powered-By");
    x_powered_by->value = haskell_module;

    return NGX_OK;
}
```

Notice that the request object *r* gets checked in function
*ngx_http_haskell_test_c_plugin()* against *NULL* value. Normally in an Nginx C
code this check is redundant, however in our plugin this is important because
serialization of the request object may fail, and in this case the Nginx module
will serialize a null pointer.

Let's compile the C code. For this we need a directory where Nginx sources were
sometime compiled. Let's refer to it in an environment variable *NGX_HOME*.

```ShellSession
$ NGX_HOME=/path/to/nginx_sources
```

Here we are going to mimic the Nginx build process.

```ShellSession
$ gcc -O2 -fPIC -c -o test_c_plugin.o -I $NGX_HOME/src/core -I $NGX_HOME/src/http -I $NGX_HOME/src/http/modules -I $NGX_HOME/src/event -I $NGX_HOME/src/evwnt/modules -I $NGX_HOME/src/os/unix -I $NGX_HOME/objs test_c_plugin.c
```

Now we have an object file *test_c_plugin.o* to link with the Haskell code.
Below is the Haskell handler (in file *test.hs*).

```haskell
import           Data.Binary.Get
import           Foreign.C.Types
import           Foreign.Ptr

-- ...

foreign import ccall "test_c_plugin.h ngx_http_haskell_test_c_plugin"
    test_c_plugin :: Ptr () -> IO CIntPtr;

toRequestPtr :: ByteString -> Ptr ()
toRequestPtr = wordPtrToPtr . fromIntegral . runGet getWordhost . L.fromStrict

testCPlugin :: ByteString -> IO L.ByteString
testCPlugin v = do
    res <- test_c_plugin $ toRequestPtr v
    return $ if res == 0
                 then "Success!"
                 else "Failure!"
ngxExportIOYY 'testCPlugin
```

Handler *testCPlugin* runs function *ngx_http_haskell_test_c_plugin()* from the
C plugin and returns *Success!* or *Failure!* in cases when the C function
returns *NGX_OK* or *NGX_ERROR* respectively. When compiled with *ghc*, this
code has to be linked with *test_c_plugin.o*.


```ShellSession
$ ghc -O2 -dynamic -shared -fPIC -L$(ghc --print-libdir)/rts -lHSrts_thr-ghc$(ghc --numeric-version) test_c_plugin.o test.hs -o test.so
[1 of 1] Compiling NgxHaskellUserRuntime ( test.hs, test.o )
Linking test.so ...
```

If we add to the nginx configuration file a new location,

```nginx
        location /cplugin {
            haskell_run testCPlugin $hs_test_c_plugin $_r_ptr;
            echo "Test C plugin returned $hs_test_c_plugin";
        }
```

and run a curl test, then we'll get our header in the response.

```ShellSession
$ curl -D- 'http://localhost:8010/cplugin'
HTTP/1.1 200 OK
Server: nginx/1.12.1
Date: Thu, 08 Mar 2018 12:09:52 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive
X-Powered-By: Nginx Haskell module

Test C plugin returned Success!
```

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

Macro *NGX_EXPORT_S_S* and others are really *cpp* macros expanded by program
*cpphs* during *ghc* compilation stage. The code where these macros and other
auxiliary functions are defined wraps the user's haskell code around, thus
producing a single source file that contains a *standalone* module with name
*NgxHaskellUserRuntime*. This implementation imposes limitations on the user's
haskell code in the nginx configuration file, of which the most important is
inability to use haskell *file-header* pragmas like *LANGUAGE* and
*OPTIONS_GHC*. However this particular limitation can be worked around with
*-X...* options in directive *haskell ghc_extra_options*. Standalone module
wrapping approach also brings ghc extensions *ForeignFunctionInterface*,
*InterruptibleFFI*, *CPP*, *ViewPatterns* and *TupleSections* into scope of the
user's haskell code. Building the module from scratch for later loading with
directive *haskell load* is also difficult.

To address limitations of the standalone module approach, another *modular*
approach was introduced. In it, the wrapping haskell code must be built in a
separate haskell module *NgxExport* and installed in the system with *cabal*.
The source code of the module is located in directory
[haskell/ngx-export](haskell/ngx-export) of the project tree. You can also find
it on [hackage](http://hackage.haskell.org/package/ngx-export). The user's
haskell code in this approach must import the module *NgxExport*.

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
[test/tsung/nginx-static.conf](test/tsung/nginx-static.conf) into an independent
all-in-one shared library on *Fedora 23 x86_64* with *ghc 7.10.2* installed from
a [copr
repository](http://copr.fedorainfracloud.org/coprs/petersen/ghc-7.10.2/).

First of all, building of such a library is only possible from command-line as
soon as it requires tuning of some *ghc* options not available from directive
*haskell ghc_extra_options*. Therefore the haskell code from the configuration
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

Pull in the reins! Being unable to load static archives seems too restrictive,
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

Replace directive *haskell compile* in the configuration file with directive

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

Debugging and tracing of haskell code
-------------------------------------

For tracing run of the haskell code and further analyzing the event log in
*threadscope* or a similar tool, the user haskell library must be compiled with
flag *-eventlog* and linked against a *debug* variant of the haskell *RTS*
library. To accomplish this in the *standalone* approach, we must add flags
*debug* and *threaded* (if there are asynchronous tasks) in directive *haskell
compile*, add flag *-eventlog* in directive *haskell ghc_extra_options*, and set
haskell *RTS* option *-l* to signal the haskell *runtime* that it must collect
events in the event log.

```nginx
    haskell rts_options -l;
```

In the *modular* approach, assuming that we have haskell source code in file
*ngx_haskell.hs*, *ghc* command-line should look as follows.

```ShellSession
$ ghc -O2 -dynamic -shared -fPIC -lHSrts_thr_debug-ghc$(ghc --numeric-version) ngx_haskell.hs -o /tmp/ngx_haskell.so -fforce-recomp -eventlog
```

The *RTS* *-l* option must be set in the same way as in the *standalone*
approach.

Before running nginx we must make sure that nginx workers are allowed to write
the event log into *current working directory* as there is no option for setting
a specific path to it. Normally nginx worker's owner is set to be *nobody*. In
modern Linux distributions there is a good promiscuous directory which suits
well for *nobody*: */tmp*. Setting this working directory in the *main* clause
of the nginx configuration

```nginx
working_directory /tmp;
```

and making requests that involve haskell handlers will create event log files
*/tmp/NgxHaskellUserRuntime-&lt;PID&gt;.eventlog* for each nginx worker suitable
for processing by *threadscope*.

Some facts about efficiency
---------------------------

- Advantages

    + The haskell code gets compiled (in the standalone mode) in a library at
      the very start of nginx and later loaded with *dlopen()* in every nginx
      worker process.
    + Nginx strings are passed to haskell exported functions as strings with
      lengths, no extra allocations are needed.
    + *Template Haskell* extension makes it possible to read files into the
      haskell code during *ghc* compilation (see sections [Static content in
      HTTP responses](#static-content-in-http-responses) and [Optimized unsafe
      content handler](#optimized-unsafe-content-handler)).

- Pitfalls

    + (This does not refer to *bytestrings*.) Haskell strings are simple lists,
      they are not contiguously allocated (but on the other hand, they are lazy
      which usually means efficient).
    + Haskell exported functions of types *S_S*, *S_SS* and *S_LS* allocate new
      strings with *malloc()* which get freed upon the request termination.
      Strings returned by functions of types *Y_Y*, *IOY_Y* and all async
      handlers (i.e. by all functions that return lazy bytestrings except for
      content handlers) are copied into a single buffer, but only when
      underlying lazy bytestrings have more than one chunks.
    + Lifetime of handlers' arguments of bytestring type in all variable and
      content handlers does not extend beyond current request's lifetime. It
      means that the arguments or their parts must not be saved in global
      states. In particular, copying the original argument when it's not going
      to be deserialized or somehow consumed in-place, is an important step in
      the [*update variables approach*](#shared-services-and-global-states).
      Arguments of services and service hooks are not affected by this.
    + Haskell content handlers are not suspendable so you cannot use
      long-running haskell functions without hitting the overall nginx
      performance. Fortunately this does not refer to [asynchronous
      handlers](#asynchronous-tasks-with-side-effects).

Some facts about exceptions
---------------------------

(The following does not refer to exceptions in async and service handlers as
they catch them all. This also does not refer to all handlers starting from
version *1.2* of the module as they became exception safe.)

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

Starting from version *1.2* all synchronous variable and content handlers were
made exception safe. Now synchronous variable handlers return *NGX_ERROR*
(effectively, an empty string) on an exception, and log it with level *error*.
Content handlers log exceptions with level *error*, and return HTTP status
*500*.

Termination of nginx worker and asynchronous exception ThreadKilled
-------------------------------------------------------------------

When an nginx worker terminates, it calls function *cancelWith* from package
*async* with argument *ThreadKilled* for all asynchronous services. This
function sends asynchronous exception *ThreadKilled* to a corresponding haskell
async thread and waits until it exits. This means that nginx worker may block if
the service thread is blocked on *unsafe* blocking foreign function (see also
[the next section](#some-facts-about-foreign-functions-that-may-block)), or it
catches *ThreadKilled* with other exceptions and re-iterates some internal loop,
or it is masked from asynchronous exceptions. Imagine the following sketch of a
service.

```haskell
serviceWithALoop _ = const go
    where go = (do
                   -- wait for an event and return result
                   -- (do not use unsafe blocking foreign functions here!)
               )
               `catch` (const waitAndGo :: SomeException -> IO L.ByteString)
          waitAndGo = threadDelaySec 1 >> go
ngxExportServiceIOYY 'serviceWithALoop
```

This service will catch *ThreadKilled* along with other exceptions because all
exceptions match *SomeException*, and re-iterate the loop by calling *go*. When
the nginx worker calls *cancelWith* with *ThreadKilled*, the *ThreadKilled*
exception will be caught, the loop will re-iterate, and the worker will never
end. We could treat *ThreadKilled* specially...

```haskell
serviceWithALoop _ = const go
    where go = (do
                   -- wait for an event and return result
                   -- (do not use unsafe blocking foreign functions here!)
               )
               `catches`
               [Handler (\e -> if e == ThreadKilled
                                   then return L.empty
                                   else waitAndGo
                        )
               ,Handler (const waitAndGo :: SomeException -> IO L.ByteString)
               ]
          waitAndGo = threadDelaySec 1 >> go
ngxExportServiceIOYY 'serviceWithALoop
```

...but this won't help a lot: *catches* still makes tail call of the *go* body
for other exceptions, and after the first non-*ThreadKilled* exception caught it
masks *go* from asynchronous exceptions thus making the service unresponsive to
*ThreadKilled*. A good solution would be avoiding re-iterations upon *catch*.

```haskell
serviceWithALoop _ = const go
    where go = (do
                   -- wait for an event and return result
                   -- (do not use unsafe blocking foreign functions here!)
               )
               `catch` (waitAndThrow :: SomeException -> IO L.ByteString)
          waitAndGo = threadDelaySec 1 >> go
          waitAndThrow e = threadDelaySec 1 >> throw e
ngxExportServiceIOYY 'serviceWithALoop
```

Now when *any* exception gets caught, the service waits 1 second and re-throw it
without re-iteration of *go*. The exception will be caught inside the service
wrapper code and even kindly reported in nginx log! But we can still do better!
*ThreadKilled* can be used to perform service cleanup and persistency actions
such as saving data on a disk etc.

```haskell
serviceWithALoop _ = const go
    where go = (do
                   -- wait for an event and return result
                   -- (do not use unsafe blocking foreign functions here!)
               )
               `catches`
               [Handler (\e -> if e == ThreadKilled
                                   then
                                       -- make cleanup and persistency actions
                                       return L.empty
                                   else waitAndThrow e
                        )
               ,Handler (waitAndThrow :: SomeException -> IO L.ByteString)
               ]
          waitAndGo = threadDelaySec 1 >> go
          waitAndThrow e = threadDelaySec 1 >> throw e
ngxExportServiceIOYY 'serviceWithALoop
```

Some facts about foreign functions that may block
-------------------------------------------------

Foreign functions that were imported via FFI as *unsafe* and would block for a
long time should be avoided in *threaded* haskell RTS, because they would block
RTS while being blocked themselves. There was a good lesson learned from this
module when shared services were being implemented. Originally, inactive workers
were blocked on file locks using function *waitToSetLock* from package *unix*.
This function makes *unsafe* call to C function *fcntl()* to acquire an advisory
file lock and blocks until it finally gets a lock. This means that RTS is unable
to do any other asynchronous tasks while being blocked, which in most cases
corresponds to the whole lifetime of a blocked worker process! The issue was
fixed by reimplementation of *waitToSetLock* with *safe* call to *fcntl()*.

Troubleshooting
---------------

- _Haskell source code fails to compile with messages ``Not in scope: ‘<$>’``
  and ``Not in scope: ‘<*>’``_.

  This happens in the *standalone module approach* with *ghc* older than *7.10*
  and can be fixed by adding line

    ```haskell
  import Control.Applicative
    ```

  in the import list inside the haskell source code.

- _In nginx error log there are many messages of *INFO* level with
  ``epoll_wait() failed (4: Interrupted system call)``_.

  This happens with *threaded* *rts* in *ghc* versions *8.0.1* and earlier (must
  be fixed in version *8.2.1* with removal of *SIGVTALARM* signals in *rts*, see
  [issue #10840](http://ghc.haskell.org/trac/ghc/ticket/10840)) when nginx
  error log on the configuration top-level has severity *INFO* or less. This can
  be fixed by setting a higher severity value on the top-level: these messages
  are harmless and other messages on *http* configuration level or deeper are
  still configurable with any severity value.

- _Nginx worker processes do not start and nginx logs messages like_

    ```
  2018/10/17 16:12:11 [emerg] 7311#0: failed to load compiled haskell library: libHS...-ghc8.6.1.so: cannot open shared object file: No such file or directory
  2018/10/17 16:12:13 [alert] 7309#0: worker process 7310 exited with fatal code 2 and cannot be respawned
    ```

  Notice that normally nginx master process (which compiles custom haskell code
  in the standalone approach) and nginx worker processes (that load compiled or
  *pre*-compiled target library) run with different system privileges: *root*
  and *nginx* (or *nobody*) respectively. In the standalone approach, dependent
  haskell libraries must have been installed by *root*: if they were installed
  locally (e.g. without *cabal* flag *``--global``*) then they will probably not
  be accessible by unprivileged users. With pre-compiled libraries this does not
  differ a lot: the libraries can have been built by a regular user, but users
  *nginx* or *nobody* may still have no permission to access this user's local
  directories. To fix this, all dependent libraries must be installed in a
  directory which is accessible by *nginx* or *nobody* users. Flag
  *``--global``* is now deprecated in *cabal*. A better way is to create a
  dedicated directory (say */var/lib/nginx/x86_64-linux-ghc-8.6.1/*), grant
  public access to it, and then collect there all dependent libraries and patch
  the target library using utility [hslibdeps](utils/README.md). In the
  standalone approach, the target library gets compiled when nginx master
  process starts, and to give it access to the dependent haskell libraries, you
  can use directive *haskell ghc_extra_options*.

    ```nginx
  haskell ghc_extra_options
          -optl-Wl,-rpath,/var/lib/nginx/x86_64-linux-ghc-8.6.1;
    ```

- When running multiple instances of nginx with identical sets of file lock
  attributes for a shared service (i.e. the path to lock files and the service
  variable name), the shared service will only work at one of the instance! In
  other words, shared services are shared even between unrelated instances of
  nginx on the same machine. This is because file locks is a system-wide
  mechanism. Compare this with running two instances of nginx on the same
  network endpoint: one of them (the slower) will claim that *Address already in
  use*.

See also
--------

There are some articles about the module in my blog:

1. [*nginx module to enable haskell binding to nginx configuration
files*](http://lin-techdet.blogspot.com/2015/12/nginx-module-to-enable-haskell-binding.html).
2. [*nginx-haskell-module: labeled media routing
example*](http://lin-techdet.blogspot.com/2017/01/nginx-haskell-module-labeled-media.html).
3. [*Passing ByteString contents reliably into C
code*](http://lin-techdet.blogspot.com/2017/08/passing-bytestring-contents-reliably.html).
4. [*Signaling all worker processes in Nginx via an event
channel*](http://lin-techdet.blogspot.com/2018/03/signaling-all-worker-processes-in-nginx.html).

