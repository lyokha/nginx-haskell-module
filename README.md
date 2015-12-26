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

    haskell ghc_extra_flags '-hide-package regex-pcre';

    haskell compile /tmp/ngx_haskell.hs '

import qualified Data.Char as C
import           Text.Regex.PCRE
import           Safe

toUpper = map C.toUpper
NGX_EXPORT_S_S (toUpper)

takeN x y = take (readDef 0 y) x
NGX_EXPORT_S_SS (takeN)

NGX_EXPORT_S_S (reverse)

isMatch :: String -> String -> String
isMatch a b = show $ (not . null) $ headDef "" $ getAllTextMatches $ a =~ b
NGX_EXPORT_S_SS (isMatch)

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
                haskell_run takeN $hs_a $arg_a $arg_b;
                echo "takeN ($arg_a, $arg_b) = $hs_a";
                break;
            }
            if ($arg_c) {
                haskell_run reverse $hs_a $arg_c;
                echo "reverse ($arg_c) = $hs_a";
                break;
            }
            if ($arg_d) {
                haskell_run isMatch $hs_a $arg_d $arg_a;
                echo "isMatch ($arg_d, $arg_a) = $hs_a";
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
accessible from the nginx that are exported with special macros *NGX_EXPORT_S_S*
and *NGX_EXPORT_S_SS* (here *S_S* and *S_SS* stand for mnemonical types
*returns-String-accepts-String* and *returns-String-accepts-String-String*).
Effectively this means that only *string* functions are supported that return
strings and accept one or two string arguments.

In this example four custom haskell functions are exported: *toUpper*, *takeN*,
*reverse* (which is normal *reverse* imported from *Prelude*) and *isMatch*
(which requires module *Text.Regex.PCRE*). As soon as normally this code won't
compile due to ambiguity involved by presence of the two packages *regex-pcre*
and *regex-pcre-builtin*, I had to add an extra *ghc* compilation flag using
directive *haskell ghc_extra_flags*.

Let's look inside the *server* clause, in *location /* where the exported
haskell functions are used. Directive *haskell_run* takes three or four
arguments: it depends on the type of the exported function (*S_S* or *S_SS*).
The first argument of the directive is the name of an exported haskell function,
the second argument is a custom variable where the function return value will be
stored, and the remaining (one or two) arguments are complex values (in the
nginx notion, it means that they may contain arbitrary number of variables and
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
# curl 'http://localhost:8010/?a=hello_world'
toUpper (hello_world) = HELLO_WORLD
# curl 'http://localhost:8010/?a=hello_world&b=4'
takeN (hello_world, 4) = hell
# curl 'http://localhost:8010/?a=hello_world&b=oops'
takeN (hello_world, oops) = 
# curl 'http://localhost:8010/?c=intelligence'
reverse (intelligence) = ecnegilletni
# curl 'http://localhost:8010/?d=intelligence&a=^i'
isMatch (intelligence, ^i) = True
# curl 'http://localhost:8010/?d=intelligence&a=^I'
isMatch (intelligence, ^I) = False
```

(of course using plain character ``^`` in an URL is not welcome, but otherwise I
would have to write another haskell function to decode URLs)

Some facts about efficiency
---------------------------

- Advantages

    + The haskell library gets compiled at the very start of nginx and later
      loaded with *dlopen()* in every nginx worker process.
    + Nginx strings are passed to haskell exported functions as strings with
      lengths, no extra allocations are needed.

- Pitfalls

    + Haskell strings are simple lists, they are not contiguously allocated
      (but on the other hand they are lazy, which usually means efficient)
    + Haskell exported functions allocate new strings which later get copied 
      to the nginx request context's pool and freed.

Some facts about exceptions
---------------------------

Haskell source code must be preferably pure and safe as soon as C code is known
to be unfamiliar with catching Haskell exceptions. That is why I used functions
from the *Safe* module in the above example.

See also
--------

[*An article*](http://lin-techdet.blogspot.com/2015/12/nginx-module-to-enable-haskell-binding.html)
about the module in my blog.

