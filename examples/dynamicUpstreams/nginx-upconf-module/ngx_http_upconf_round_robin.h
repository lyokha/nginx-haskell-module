#ifndef NGX_UPCONF_ROUND_ROBIN_H
#define NGX_UPCONF_ROUND_ROBIN_H

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>


ngx_int_t ngx_http_upconf_init_round_robin(ngx_conf_t *cf,
    ngx_http_upstream_srv_conf_t *us);
ngx_int_t ngx_http_upconf_get_round_robin_peer(ngx_peer_connection_t *pc,
    void *data);
void ngx_http_upconf_free_round_robin_peer(ngx_peer_connection_t *pc,
    void *data, ngx_uint_t state);

#if (NGX_HTTP_SSL)
ngx_int_t ngx_http_upconf_set_round_robin_peer_session(
    ngx_peer_connection_t *pc, void *data);
void ngx_http_upconf_save_round_robin_peer_session(
    ngx_peer_connection_t *pc, void *data);
#endif

#endif /* NGX_UPCONF_ROUND_ROBIN_H */

