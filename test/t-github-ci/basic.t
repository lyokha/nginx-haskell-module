# vi:filetype=

use Test::Nginx::Socket;

repeat_each(2);
plan tests => repeat_each() * (2 * blocks());

no_shuffle();
run_tests();

__DATA__

=== TEST 1: toUpper
--- http_config
    haskell load /var/lib/nginx/test_github_ci.so;
--- config
    location /toUpper {
        haskell_run toUpper $hs_a $arg_a;
        echo "toUpper ($arg_a) = $hs_a";
    }
    location /takeN {
        haskell_run takeN $hs_a $arg_b $arg_a;
        echo "takeN ($arg_a, $arg_b) = $hs_a";
    }
    location /reverse {
        haskell_run reverse $hs_a $arg_a;
        echo "reverse ($arg_a) = $hs_a";
    }
    location /matches {
        haskell_run matches $hs_a $arg_b $arg_a;
        haskell_run urlDecode $hs_b $arg_a;
        echo "matches ($arg_b, $hs_b) = $hs_a";
    }
    location /firstNotEmpty {
        haskell_run firstNotEmpty $hs_a $arg_b $arg_c $arg_a;
        echo "firstNotEmpty ($arg_b, $arg_c, $arg_a) = $hs_a";
    }
    location /isInList {
        haskell_run isInList $hs_a $arg_a secret1 secret2 secret3;
        echo "isInList ($arg_a, <secret words>) = $hs_a";
    }
    location /isJSONListOfInts {
        haskell_run isJSONListOfInts $hs_a $arg_a;
        haskell_run urlDecode $hs_b $arg_a;
        echo "isJSONListOfInts ($hs_b) = $hs_a";
    }
    location /jSONListOfIntsTakeN {
        haskell_run jSONListOfIntsTakeN $hs_a $arg_take|$arg_a;
        haskell_run urlDecode $hs_b $arg_a;
        echo "jSONListOfIntsTakeN ($hs_b, $arg_take) = $hs_a";
    }
--- request
    GET /toUpper?a=hello_world
--- response_body
toUpper (hello_world) = HELLO_WORLD
--- error_code: 200

=== TEST 2: takeN
--- request
    GET /takeN?a=hello_world&b=4
--- response_body
takeN (hello_world, 4) = hell
--- error_code: 200

=== TEST 3: takeN (bad argument)
--- request
    GET /takeN?a=hello_world&b=oops
--- response_body
takeN (hello_world, oops) = 
--- error_code: 200

=== TEST 4: reverse
--- request
    GET /reverse?a=intelligence
--- response_body
reverse (intelligence) = ecnegilletni
--- error_code: 200

=== TEST 5: matches
--- request
    GET /matches?b=intelligence&a=%5Ei
--- response_body
matches (intelligence, ^i) = 1
--- error_code: 200

=== TEST 6: matches (does not match)
--- request
    GET /matches?b=intelligence&a=%5EI
--- response_body
matches (intelligence, ^I) = 0
--- error_code: 200

=== TEST 7: firstNotEmpty
--- request
    GET /firstNotEmpty?d=1&c=intelligence&a=smart
--- response_body
firstNotEmpty (, intelligence, smart) = intelligence
--- error_code: 200

=== TEST 8: firstNotEmpty 2
--- request
    GET /firstNotEmpty?d=1&c=intelligence&b=smart
--- response_body
firstNotEmpty (smart, intelligence, ) = smart
--- error_code: 200

=== TEST 9: firstNotEmpty (empty list)
--- request
    GET /firstNotEmpty?d=1
--- response_body
firstNotEmpty (, , ) = 
--- error_code: 200

=== TEST 10: isInList (no key)
--- request
    GET /isInList?d=1
--- response_body
isInList (, <secret words>) = 0
--- error_code: 200

=== TEST 11: isInList (not found)
--- request
    GET /isInList?d=1&a=s
--- response_body
isInList (s, <secret words>) = 0
--- error_code: 200

=== TEST 12: isInList (found)
--- request
    GET /isInList?d=1&a=secret2
--- response_body
isInList (secret2, <secret words>) = 1
--- error_code: 200

=== TEST 13: isJSONListOfInts
--- request
    GET /isJSONListOfInts?a=%5B1%2C2%2C3%5D
--- response_body
isJSONListOfInts ([1,2,3]) = 1
--- error_code: 200

=== TEST 14: isJSONListOfInts (not a JSON)
--- request
    GET /isJSONListOfInts?a=unknown
--- response_body
isJSONListOfInts (unknown) = 0
--- error_code: 200

=== TEST 15: jSONListOfIntsTakeN
--- request
    GET /jSONListOfIntsTakeN?a=%5B10%2C20%2C30%2C40%5D&take=3
--- response_body
jSONListOfIntsTakeN ([10,20,30,40], 3) = [10,20,30]
--- error_code: 200

=== TEST 16: jSONListOfIntsTakeN (bad take argument)
--- request
    GET /jSONListOfIntsTakeN?a=%5B10%2C20%2C30%2C40%5D&take=undefined
--- response_body
jSONListOfIntsTakeN ([10,20,30,40], undefined) = []
--- error_code: 200

