##### Aliases

This module declares handy aliases for common patterns built of directives from
Nginx Haskell module. The alias directives and their translations are listed in
the following table.

| Aliases (in examples)             | Translations                            |
| --------------------------------- | --------------------------------------- |
| `cache_set $cached_arg_a $arg_a;` | `haskell_run ! <!$cached_arg_a $arg_a;` |
| `lazy_set $lazy_arg_a $arg_a;`    | `haskell_run ! $lazy_arg_a $arg_a;`     |

The reason for using *cache_set* and *lazy_set* is inability of directive *set*
from the standard Nginx *rewrite module* for setting and caching variables right
before the *rewrite phase* as well as setting them lazily.

##### Build

To build the module, add option
`--add-module=/path/to/nginx-haskell-module/aliases` after option
`--add-module=/path/to/nginx-haskell-module` when configuring Nginx build.

