Nginx Haskell module
====================

This Nginx module allows compiling and running Haskell source code found in a
configuration file or an existing shared library. Below is a motivational
example.

```nginx
user                nobody;
worker_processes    2;

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

