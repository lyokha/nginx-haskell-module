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

    haskell ghc_extra_flags '-hide-package regex-pcre -XFlexibleInstances';

    haskell compile /tmp/ngx_haskell.hs '

import qualified Data.Char as C
import           Text.Regex.PCRE
import           Data.Aeson
import           Data.Maybe
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
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

isJSONListOfInts x = isJust
    (decode $ fromMaybe B.empty $ doURLDecode x :: Maybe [Int])
NGX_EXPORT_B_Y (isJSONListOfInts)

jSONListOfIntsTakeN x = encode $ take n $ fromMaybe []
    (decode $ fromMaybe B.empty $ doURLDecode y :: Maybe [Int])
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
                liftM2 ((:) . C.chr) (readMay [\'0\', \'x\', a, b]) $
                    doURLDecode xss
            _ -> Nothing
    doURLDecode (\'+\' : xs) = liftM (\' \' :) $ doURLDecode xs
    doURLDecode (x : xs) = liftM (x :) $ doURLDecode xs

instance UrlDecodable ByteString where
    -- adopted for ByteString arguments from
    -- http://www.rosettacode.org/wiki/URL_decoding#Haskell
    doURLDecode (B.null -> True) = Just B.empty
    doURLDecode (B.uncons -> Just (37, xs))
        | B.length xs > 1 =
            liftM2 B.cons (readMay $ \'0\' : \'x\' : C8.unpack (B.take 2 xs)) $
                doURLDecode $ B.drop 2 xs
        | otherwise = Nothing
    doURLDecode (B.uncons -> Just (43, xs)) =
        liftM (32 `B.cons`) $ doURLDecode xs
    doURLDecode (B.uncons -> Just (x, xs)) =
        liftM (x `B.cons`) $ doURLDecode xs

urlDecode = fromMaybe "" . doURLDecode
NGX_EXPORT_S_S (urlDecode)

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
two macros that deal with *lazy byte strings*: *NGX_EXPORT_Y_Y* and
*NGX_EXPORT_B_Y* (*Y* stands for *bYte*). Effectively this means that only those
functions are supported that return strings, byte strings or booleans and accept
one, two or more (only *S_LS* and *B_LS*) string arguments or one byte string.

In this example nine custom haskell functions are exported: *toUpper*, *takeN*,
*reverse* (which is normal *reverse* imported from *Prelude*), *matches*
(which requires module *Text.Regex.PCRE*), *firstNotEmpty*, *isInList*,
*isJSONListOfInts*, *jSONListOfIntsTakeN* and *urlDecode*. As soon as probably
this code won't compile due to ambiguity involved by presence of the two
packages *regex-pcre* and *regex-pcre-builtin*, I had to add an extra *ghc*
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

What about doing some tests? Let's first start nginx.

```
nginx -p. -c./nginx.conf
[1 of 1] Compiling NgxHaskellUserRuntime ( /tmp/ngx_haskell.hs, /tmp/ngx_haskell.o )
Linking /tmp/ngx_haskell.so ...
```

Nginx compiles haskell code at its start. Had compilation failed and nginx would
not have started! But in this case the code is OK and we are moving forward.

```
%> curl 'http://localhost:8010/?a=hello_world'
toUpper (hello_world) = HELLO_WORLD
%> curl 'http://localhost:8010/?a=hello_world&b=4'
takeN (hello_world, 4) = hell
%> curl 'http://localhost:8010/?a=hello_world&b=oops'
takeN (hello_world, oops) = 
%> curl 'http://localhost:8010/?c=intelligence'
reverse (intelligence) = ecnegilletni
%> curl 'http://localhost:8010/?d=intelligence&a=%5Ei'              # URL-encoded ^i
matches (intelligence, ^i) = 1
%> curl 'http://localhost:8010/?d=intelligence&a=%5EI'              # URL-encoded ^I
matches (intelligence, ^I) = 0
%> curl 'http://localhost:8010/?e=1&g=intelligence&a=smart'
firstNotEmpty (, intelligence, smart) = intelligence
%> curl 'http://localhost:8010/?e=1&g=intelligence&f=smart'
firstNotEmpty (smart, intelligence, ) = smart
%> curl 'http://localhost:8010/?e=1'
firstNotEmpty (, , ) = 
%> curl 'http://localhost:8010/?l=1'
isInList (, <secret words>) = 0
%> curl 'http://localhost:8010/?l=1&a=s'
isInList (s, <secret words>) = 0
%> curl 'http://localhost:8010/?l=1&a=secret2'
isInList (secret2, <secret words>) = 1
%> curl 'http://localhost:8010/?m=%5B1%2C2%2C3%5D'                  # URL-encoded [1,2,3]
isJSONListOfInts ([1,2,3]) = 1
%> curl 'http://localhost:8010/?m=unknown'
isJSONListOfInts (unknown) = 0
%> curl 'http://localhost:8010/?n=%5B10%2C20%2C30%2C40%5D&take=3'   # URL-encoded [10,20,30,40]
jSONListOfIntsTakeN ([10,20,30,40], 3) = [10,20,30]
%> curl 'http://localhost:8010/?n=%5B10%2C20%2C30%2C40%5D&take=undefined'
jSONListOfIntsTakeN ([10,20,30,40], undefined) = []
```

Some facts about efficiency
---------------------------

- Advantages

    + The haskell library gets compiled at the very start of nginx and later
      loaded with *dlopen()* in every nginx worker process.
    + Nginx strings are passed to haskell exported functions as strings with
      lengths, no extra allocations are needed.

- Pitfalls

    + Haskell strings are simple lists, they are not contiguously allocated
      (but on the other hand they are lazy, which usually means efficient).
    + Haskell exported functions of types *S_S*, *S_SS*, *S_LS* and *Y_Y*
      allocate new strings with *malloc()* which get freed upon the request
      termination.

Some facts about exceptions
---------------------------

Haskell source code must be preferably pure and safe as soon as C code is known
to be unfamiliar with catching Haskell exceptions. That is why I used functions
from the *Safe* module in the above example.

See also
--------

[*An article*](http://lin-techdet.blogspot.com/2015/12/nginx-module-to-enable-haskell-binding.html)
about the module in my blog.

