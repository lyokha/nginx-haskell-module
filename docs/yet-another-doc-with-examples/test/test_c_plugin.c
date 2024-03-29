/* Compile:
 *      NGX_HOME=/path/to/nginx_sources
 *      gcc -fPIC -c -o test_c_plugin.o \
 *          -I $NGX_HOME/src/core \
 *          -I $NGX_HOME/src/http \
 *          -I $NGX_HOME/src/http/modules \
 *          -I $NGX_HOME/src/event \
 *          -I $NGX_HOME/src/event/modules \
 *          -I $NGX_HOME/src/os/unix \
 *          -I $NGX_HOME/objs test_c_plugin.c
 */

#include <ngx_core.h>
#include <ngx_http.h>

static const ngx_str_t haskell_module = ngx_string("Nginx Haskell module");

ngx_int_t
ngx_http_haskell_test_c_plugin(ngx_http_request_t *r)
{
    ngx_table_elt_t  *x_powered_by;

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

