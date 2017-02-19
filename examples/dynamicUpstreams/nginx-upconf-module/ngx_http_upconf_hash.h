#ifndef NGX_UPCONF_HASH_H
#define NGX_UPCONF_HASH_H

#include <ngx_config.h>
#include <ngx_core.h>


typedef struct {
    ngx_http_complex_value_t                key;
    struct ngx_http_upconf_chash_points_s  *points;
} ngx_http_upconf_hash_conf_t;


ngx_int_t ngx_http_upconf_init_hash(ngx_conf_t *cf,
    ngx_http_upstream_srv_conf_t *us);
ngx_int_t ngx_http_upconf_init_chash(ngx_conf_t *cf,
    ngx_http_upstream_srv_conf_t *us);
ngx_int_t ngx_http_upconf_init_chash_common(ngx_log_t *log,
    ngx_http_upstream_srv_conf_t *us, ngx_uint_t check);

#endif /* NGX_UPCONF_HASH_H */

