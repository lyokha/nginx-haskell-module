#ifndef NGX_UPCONF_ROUND_ROBIN_H
#define NGX_UPCONF_ROUND_ROBIN_H

#include <ngx_config.h>
#include <ngx_core.h>


ngx_int_t ngx_http_upconf_init_round_robin(ngx_conf_t *cf,
    ngx_http_upstream_srv_conf_t *us);

#endif /* NGX_UPCONF_ROUND_ROBIN_H */

