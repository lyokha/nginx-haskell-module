/*
 * these original functions are brought from
 * nginx/src/http/ngx_http_upstream_round_robin.c
 */

/*
 * Copyright (C) Igor Sysoev
 * Copyright (C) Nginx, Inc.
 */


#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

#include "ngx_http_upconf_round_robin.h"
#include "ngx_http_upconf_module.h"


typedef struct {
    ngx_http_upstream_rr_peer_data_t  *rrp;
    struct sockaddr                    sockaddr;
} ngx_http_upconf_peer_data_t;


static ngx_int_t ngx_http_upconf_init_round_robin_peer(ngx_http_request_t *r,
    ngx_http_upstream_srv_conf_t *us);
static ngx_int_t ngx_http_upconf_get_round_robin_peer(ngx_peer_connection_t *pc,
    void *data);
static ngx_http_upstream_rr_peer_t * ngx_http_upconf_get_peer(
    ngx_http_upstream_rr_peer_data_t *rrp);
static void ngx_http_upconf_free_round_robin_peer(ngx_peer_connection_t *pc,
    void *data, ngx_uint_t state);

#if (NGX_HTTP_SSL)
static ngx_int_t ngx_http_upconf_set_round_robin_peer_session(
    ngx_peer_connection_t *pc, void *data);
static void ngx_http_upconf_save_round_robin_peer_session(
    ngx_peer_connection_t *pc, void *data);
#endif


ngx_int_t
ngx_http_upconf_init_round_robin(ngx_conf_t *cf,
                                 ngx_http_upstream_srv_conf_t *us)
{
    if (ngx_http_upstream_init_round_robin(cf, us) != NGX_OK) {
        return NGX_ERROR;
    }

    us->peer.init = ngx_http_upconf_init_round_robin_peer;

    return NGX_OK;
}


static ngx_int_t
ngx_http_upconf_init_round_robin_peer(ngx_http_request_t *r,
    ngx_http_upstream_srv_conf_t *us)
{
    ngx_uint_t                    rc;
    ngx_http_upconf_peer_data_t  *peer_data;

    rc = ngx_http_upstream_init_round_robin_peer(r, us);
    if (rc != NGX_OK) {
        return rc;
    }

    peer_data = ngx_palloc(r->pool, sizeof(ngx_http_upconf_peer_data_t));
    if (peer_data == NULL) {
        return NGX_ERROR;
    }

    peer_data->rrp = r->upstream->peer.data;
    r->upstream->peer.data = peer_data;

    r->upstream->peer.get = ngx_http_upconf_get_round_robin_peer;
    r->upstream->peer.free = ngx_http_upconf_free_round_robin_peer;
#if (NGX_HTTP_SSL)
    r->upstream->peer.set_session =
                               ngx_http_upconf_set_round_robin_peer_session;
    r->upstream->peer.save_session =
                               ngx_http_upconf_save_round_robin_peer_session;
#endif

    return NGX_OK;
}


static ngx_int_t
ngx_http_upconf_get_round_robin_peer(ngx_peer_connection_t *pc, void *data)
{
    ngx_http_upconf_peer_data_t       *peer_data = data;
    ngx_http_upstream_rr_peer_data_t  *rrp;

    ngx_int_t                      rc;
    ngx_uint_t                     i, n;
    ngx_http_upstream_rr_peer_t   *peer;
    ngx_http_upstream_rr_peers_t  *peers;

    ngx_log_debug1(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                   "get rr peer, try: %ui", pc->tries);

    pc->cached = 0;
    pc->connection = NULL;

    rrp = peer_data->rrp;
    peers = rrp->peers;

    pc->name = peers->name;

    ngx_http_upstream_rr_peers_wlock(peers);

    if (peers->single || peers->peer) {
        /* BEWARE: misusing both rrp->config and peers->peer->max_conns! */
        rrp->config = peers->peer->max_conns;
    }

    if (peers->single) {
        peer = peers->peer;

        if (peer->down) {
            goto failed;
        }

        if (peer->max_conns && peer->conns >= peer->max_conns) {
            goto failed;
        }

        rrp->current = peer;

    } else {

        /* there are several peers */

        peer = ngx_http_upconf_get_peer(rrp);

        if (peer == NULL) {
            goto failed;
        }

        ngx_log_debug2(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                       "get rr peer, current: %p %i",
                       peer, peer->current_weight);
    }

    peer_data->sockaddr = *peer->sockaddr;

    pc->sockaddr = &peer_data->sockaddr;
    pc->socklen = peer->socklen;

    peer->conns++;

    ngx_http_upstream_rr_peers_unlock(peers);

    return NGX_OK;

failed:

    if (peers->next) {

        ngx_log_debug0(NGX_LOG_DEBUG_HTTP, pc->log, 0, "backup servers");

        rrp->peers = peers->next;

        n = (rrp->peers->number + (8 * sizeof(uintptr_t) - 1))
                / (8 * sizeof(uintptr_t));

        for (i = 0; i < n; i++) {
            rrp->tried[i] = 0;
        }

        ngx_http_upstream_rr_peers_unlock(peers);

        rc = ngx_http_upstream_get_round_robin_peer(pc, rrp);

        if (rc != NGX_BUSY) {
            return rc;
        }

        ngx_http_upstream_rr_peers_wlock(peers);
    }

    ngx_http_upstream_rr_peers_unlock(peers);

    return NGX_BUSY;
}


static ngx_http_upstream_rr_peer_t *
ngx_http_upconf_get_peer(ngx_http_upstream_rr_peer_data_t *rrp)
{
    time_t                        now;
    uintptr_t                     m;
    ngx_int_t                     total;
    ngx_uint_t                    i, n, p;
    ngx_http_upstream_rr_peer_t  *peer, *best;

    now = ngx_time();

    best = NULL;
    total = 0;

#if (NGX_SUPPRESS_WARN)
    p = 0;
#endif

    for (peer = rrp->peers->peer, i = 0;
         peer;
         peer = peer->next, i++)
    {
        n = i / (8 * sizeof(uintptr_t));
        m = (uintptr_t) 1 << i % (8 * sizeof(uintptr_t));

        if (rrp->tried[n] & m) {
            continue;
        }

        if (peer->down) {
            continue;
        }

        if (peer->max_fails
            && peer->fails >= peer->max_fails
            && now - peer->checked <= peer->fail_timeout)
        {
            continue;
        }

        if (peer->max_conns && peer->conns >= peer->max_conns) {
            continue;
        }

        peer->current_weight += peer->effective_weight;
        total += peer->effective_weight;

        if (peer->effective_weight < peer->weight) {
            peer->effective_weight++;
        }

        if (best == NULL || peer->current_weight > best->current_weight) {
            best = peer;
            p = i;
        }
    }

    if (best == NULL) {
        return NULL;
    }

    rrp->current = best;

    n = p / (8 * sizeof(uintptr_t));
    m = (uintptr_t) 1 << p % (8 * sizeof(uintptr_t));

    rrp->tried[n] |= m;

    best->current_weight -= total;

    if (now - best->checked > best->fail_timeout) {
        best->checked = now;
    }

    return best;
}


static void
ngx_http_upconf_free_round_robin_peer(ngx_peer_connection_t *pc, void *data,
    ngx_uint_t state)
{
    ngx_http_upconf_peer_data_t       *peer_data = data;
    ngx_http_upstream_rr_peer_data_t  *rrp;

    time_t                         now;
    ngx_http_upstream_rr_peer_t   *peer;
    ngx_http_upstream_rr_peers_t  *peers;

    ngx_log_debug2(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                   "free rr peer %ui %ui", pc->tries, state);

    /* TODO: NGX_PEER_KEEPALIVE */

    rrp = peer_data->rrp;

    peer = rrp->current;

    peers = rrp->peers;

    ngx_http_upstream_rr_peers_rlock(peers);

    /* BEWARE: misusing both rrp->config and peers->peer->max_conns! */
    if (!peers->peer || rrp->config != peers->peer->max_conns) {
        ngx_http_upstream_rr_peers_unlock(peers);
        return;
    }

    ngx_http_upstream_rr_peer_lock(peers, peer);

    if (peers->single) {

        peer->conns--;

        ngx_http_upstream_rr_peer_unlock(peers, peer);
        ngx_http_upstream_rr_peers_unlock(peers);

        pc->tries = 0;
        return;
    }

    if (state & NGX_PEER_FAILED) {
        now = ngx_time();

        peer->fails++;
        peer->accessed = now;
        peer->checked = now;

        if (peer->max_fails) {
            peer->effective_weight -= peer->weight / peer->max_fails;

            if (peer->fails >= peer->max_fails) {
                ngx_log_error(NGX_LOG_WARN, pc->log, 0,
                              "upstream server temporarily disabled");
            }
        }

        ngx_log_debug2(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                       "free rr peer failed: %p %i",
                       peer, peer->effective_weight);

        if (peer->effective_weight < 0) {
            peer->effective_weight = 0;
        }

    } else {

        /* mark peer live if check passed */

        if (peer->accessed < peer->checked) {
            peer->fails = 0;
        }
    }

    peer->conns--;

    ngx_http_upstream_rr_peer_unlock(peers, peer);
    ngx_http_upstream_rr_peers_unlock(peers);

    if (pc->tries) {
        pc->tries--;
    }
}


#if (NGX_HTTP_SSL)

static ngx_int_t
ngx_http_upconf_set_round_robin_peer_session(ngx_peer_connection_t *pc,
    void *data)
{
    ngx_http_upconf_peer_data_t       *peer_data = data;
    ngx_http_upstream_rr_peer_data_t  *rrp;

    rrp = peer_data->rrp;

    return ngx_http_upstream_set_round_robin_peer_session(pc, rrp);
}


static void
ngx_http_upconf_save_round_robin_peer_session(ngx_peer_connection_t *pc,
    void *data)
{
    ngx_http_upconf_peer_data_t       *peer_data = data;
    ngx_http_upstream_rr_peer_data_t  *rrp;

    rrp = peer_data->rrp;

    ngx_http_upstream_save_round_robin_peer_session(pc, rrp);
}

#endif

