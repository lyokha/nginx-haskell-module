/* Compile:
 *      NGX_HOME=/path/to/nginx_sources
 *      gcc -fPIC -c -o test_c_plugin.o \
 *          -I $NGX_HOME/src/core \
 *          -I $NGX_HOME/src/http \
 *          -I $NGX_HOME/src/http/modules \
 *          -I $NGX_HOME/src/event \
 *          -I $NGX_HOME/src/evwnt/modules \
 *          -I $NGX_HOME/src/os/unix \
 *          -I $NGX_HOME/objs test_c_plugin.c
 */

#ifndef NGX_HTTP_HASKELL_TEST_C_PLUGIN_H
#define NGX_HTTP_HASKELL_TEST_C_PLUGIN_H

#include <ngx_core.h>
#include <ngx_http.h>

ngx_int_t ngx_http_haskell_test_c_plugin(ngx_http_request_t *r);

#endif

