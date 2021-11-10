##### Aliases

This module declares handy aliases for common patterns built of directives from
Nginx Haskell module. Currently, it contains only one alias directive
*cache_variable* that gets translated as shown in the following table.

| Aliases (in examples)                  | Translations                            |
| -------------------------------------- | --------------------------------------- |
| `cache_variable $cached_arg_a $arg_a;` | `haskell_run ! <!$cached_arg_a $arg_a;` |

##### Build

To build the module, add option
`--add-module=/path/to/nginx-haskell-module/aliases` after option
`--add-module=/path/to/nginx-haskell-module` when configuring Nginx build.

