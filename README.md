Nginx Haskell module
====================

This Nginx module allows compiling and running Haskell source code found in a
configuration file or an existing shared library.

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

    haskell compile /tmp/ngx_haskell.hs '

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

jSONListOfIntsTakeN x = encode $ take n $ fromMaybe [] $ jSONListOfInts y
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
##Do some JSON parsing

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
module. In several clauses of *doURLDecode* there are explicit characters
wrapped inside single quotes which are in turn escaped with backslashes to not
confuse nginx parser as the haskell code itself is wrapped inside single quotes.
Exported function *urlDecode* is defined via the string instance of
*doURLDecode*: if decoding fails it returns an empty string.

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
not have started! But in this case the code is OK and we are moving forward.

```ShellSession
# curl 'http://localhost:8010/?a=hello_world'
toUpper (hello_world) = HELLO_WORLD
# curl 'http://localhost:8010/?a=hello_world&b=4'
takeN (hello_world, 4) = hell
# curl 'http://localhost:8010/?a=hello_world&b=oops'
takeN (hello_world, oops) = 
# curl 'http://localhost:8010/?c=intelligence'
reverse (intelligence) = ecnegilletni
# curl 'http://localhost:8010/?d=intelligence&a=%5Ei'              # URL-encoded ^i
matches (intelligence, ^i) = 1
# curl 'http://localhost:8010/?d=intelligence&a=%5EI'              # URL-encoded ^I
matches (intelligence, ^I) = 0
# curl 'http://localhost:8010/?e=1&g=intelligence&a=smart'
firstNotEmpty (, intelligence, smart) = intelligence
# curl 'http://localhost:8010/?e=1&g=intelligence&f=smart'
firstNotEmpty (smart, intelligence, ) = smart
# curl 'http://localhost:8010/?e=1'
firstNotEmpty (, , ) = 
# curl 'http://localhost:8010/?l=1'
isInList (, <secret words>) = 0
# curl 'http://localhost:8010/?l=1&a=s'
isInList (s, <secret words>) = 0
# curl 'http://localhost:8010/?l=1&a=secret2'
isInList (secret2, <secret words>) = 1
# curl 'http://localhost:8010/?m=%5B1%2C2%2C3%5D'                  # URL-encoded [1,2,3]
isJSONListOfInts ([1,2,3]) = 1
# curl 'http://localhost:8010/?m=unknown'
isJSONListOfInts (unknown) = 0
# curl 'http://localhost:8010/?n=%5B10%2C20%2C30%2C40%5D&take=3'   # URL-encoded [10,20,30,40]
jSONListOfIntsTakeN ([10,20,30,40], 3) = [10,20,30]
# curl 'http://localhost:8010/?n=%5B10%2C20%2C30%2C40%5D&take=undefined'
jSONListOfIntsTakeN ([10,20,30,40], undefined) = []
```

Let's try location */content* (in a browser it looks great!)

```ShellSession
# curl -D- 'http://localhost:8010/content?n=%5B10%2C20%2C30%2C40%5D&take=3'
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

Static files contents in HTTP responses
---------------------------------------

Reading files in runtime inescapably drops nginx performance. Fortunately there
is a haskell module [file-embed](http://hackage.haskell.org/package/file-embed)
that makes it possible to embed files during *ghc* compilation time. Consider
the following haskell content handler

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
  *haskell_run* directive in some location.) The new workers will start but fail
  the module initialization and shut down, the old workers will shut down. No
  live workers will exist.

All errors will be logged, so the best way to find out if errors occurred during
reloading of the nginx configuration (and at the start of nginx too) is to refer
to the logs.

Some facts about efficiency
---------------------------

- Advantages

    + The haskell library gets compiled at the very start of nginx and later
      loaded with *dlopen()* in every nginx worker process.
    + Nginx strings are passed to haskell exported functions as strings with
      lengths, no extra allocations are needed.
    + *Template Haskell* extension makes it possible to read files into the
      haskell code during *ghc* compilation (see section [Static files contents
      in HTTP responses](#static-files-contents-in-http-responses)).

- Pitfalls

    + (This does not refer to *byte strings*.) Haskell strings are simple lists,
      they are not contiguously allocated (but on the other hand they are lazy,
      which usually means efficient).
    + Haskell exported functions of types *S_S*, *S_SS*, *S_LS* and *Y_Y* and
      content handlers allocate new strings with *malloc()* which get freed upon
      the request termination.
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
*execute* are required (they are exported from module *Text.Regex.PCRE.String*).
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
*safeMatch* gets unwrapped with *unsafePerformIO* (exported from
*System.IO.Unsafe*). There is nothing bad about that in this particular case:
internally higher level API regex functions like *(=~)* and *match* do all the
same.

See also
--------

[*An article*](http://lin-techdet.blogspot.com/2015/12/nginx-module-to-enable-haskell-binding.html)
about the module in my blog.

