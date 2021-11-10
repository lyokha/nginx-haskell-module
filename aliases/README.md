This module declares handy configuration alias directives for common directive
patterns met when using Nginx Haskell module. Currently, it contains only one
alias directive *cache_variable* that gets translated as in the following table.

| Aliases (in examples)              | Translations                        |
| ---------------------------------- | ----------------------------------- |
| `cache_variable $hs_arg_a $arg_a;` | `haskell_run ! <!$hs_arg_a $arg_a;` |

To build the module, add option
`--add-module=/home/lyokha/devel/nginx-haskell-module/aliases` when configuring
Nginx build.

