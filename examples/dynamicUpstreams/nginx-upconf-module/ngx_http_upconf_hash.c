/*
 * these original functions are brought from
 * nginx/src/http/modules/ngx_http_upstream_hash_module.c
 */

/*
 * Copyright (C) Roman Arutyunyan
 * Copyright (C) Nginx, Inc.
 */

#include "ngx_http_upconf_hash.h"
#include "ngx_http_upconf_module.h"
#include "ngx_http_upconf_round_robin.h"


typedef struct {
    uint32_t                            hash;
    ngx_str_t                          *server;
} ngx_http_upconf_chash_point_t;


struct ngx_http_upconf_chash_points_s {
    ngx_uint_t                          number;
    ngx_http_upconf_chash_point_t       point[1];
};

typedef struct ngx_http_upconf_chash_points_s ngx_http_upconf_chash_points_t;


typedef struct {
    /* the round robin data must be first */
    ngx_http_upstream_rr_peer_data_t    rrp;
    ngx_http_upconf_hash_conf_t        *conf;
    ngx_str_t                           key;
    ngx_uint_t                          tries;
    ngx_uint_t                          rehash;
    uint32_t                            hash;
    ngx_event_get_peer_pt               get_rr_peer;
} ngx_http_upstream_hash_peer_data_t;


typedef struct {
    /* the round robin data, number and sockaddr must be first */
    ngx_http_upstream_rr_peer_data_t    *rrp;
    ngx_uint_t                           number;
    struct sockaddr                      sockaddr;
    ngx_http_upstream_hash_peer_data_t  *hp;
} ngx_http_upconf_hash_peer_data_t;


static ngx_int_t ngx_http_upstream_init_hash_peer(ngx_http_request_t *r,
    ngx_http_upstream_srv_conf_t *us);
static ngx_int_t ngx_http_upstream_get_hash_peer(ngx_peer_connection_t *pc,
    void *data);

static int ngx_libc_cdecl
    ngx_http_upstream_chash_cmp_points(const void *one, const void *two);
static ngx_uint_t ngx_http_upstream_find_chash_point(
    ngx_http_upconf_chash_points_t *points, uint32_t hash);
static ngx_int_t ngx_http_upstream_init_chash_peer(ngx_http_request_t *r,
    ngx_http_upstream_srv_conf_t *us);
static ngx_int_t ngx_http_upstream_get_chash_peer(ngx_peer_connection_t *pc,
    void *data);


ngx_int_t
ngx_http_upconf_init_hash(ngx_conf_t *cf, ngx_http_upstream_srv_conf_t *us)
{
    if (ngx_http_upstream_init_round_robin(cf, us) != NGX_OK) {
        return NGX_ERROR;
    }

    us->peer.init = ngx_http_upstream_init_hash_peer;

    return NGX_OK;
}


static ngx_int_t
ngx_http_upstream_init_hash_peer(ngx_http_request_t *r,
    ngx_http_upstream_srv_conf_t *us)
{
    ngx_http_upconf_srv_conf_t          *scf;
    ngx_http_upconf_hash_conf_t         *hcf;
    ngx_http_upstream_hash_peer_data_t  *hp;

    hp = ngx_palloc(r->pool, sizeof(ngx_http_upstream_hash_peer_data_t));
    if (hp == NULL) {
        return NGX_ERROR;
    }

    r->upstream->peer.data = &hp->rrp;

    if (ngx_http_upstream_init_round_robin_peer(r, us) != NGX_OK) {
        return NGX_ERROR;
    }

    r->upstream->peer.get = ngx_http_upstream_get_hash_peer;

    scf = ngx_http_conf_upstream_srv_conf(us, ngx_http_upconf_module);
    hcf = &scf->hash;

    if (ngx_http_complex_value(r, &hcf->key, &hp->key) != NGX_OK) {
        return NGX_ERROR;
    }

    ngx_log_debug1(NGX_LOG_DEBUG_HTTP, r->connection->log, 0,
                   "upstream hash key:\"%V\"", &hp->key);

    hp->conf = hcf;
    hp->tries = 0;
    hp->rehash = 0;
    hp->hash = 0;
    hp->get_rr_peer = ngx_http_upstream_get_round_robin_peer;

    return NGX_OK;
}


static ngx_int_t
ngx_http_upstream_get_hash_peer(ngx_peer_connection_t *pc, void *data)
{
    ngx_http_upstream_hash_peer_data_t  *hp = data;

    time_t                        now;
    u_char                        buf[NGX_INT_T_LEN];
    size_t                        size;
    uint32_t                      hash;
    ngx_int_t                     w;
    uintptr_t                     m;
    ngx_uint_t                    n, p;
    ngx_http_upstream_rr_peer_t  *peer;

    ngx_log_debug1(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                   "get hash peer, try: %ui", pc->tries);

    ngx_http_upstream_rr_peers_wlock(hp->rrp.peers);

    if (hp->tries > 20 || hp->rrp.peers->single) {
        ngx_http_upstream_rr_peers_unlock(hp->rrp.peers);
        return hp->get_rr_peer(pc, &hp->rrp);
    }

    now = ngx_time();

    pc->cached = 0;
    pc->connection = NULL;

    for ( ;; ) {

        /*
         * Hash expression is compatible with Cache::Memcached:
         * ((crc32([REHASH] KEY) >> 16) & 0x7fff) + PREV_HASH
         * with REHASH omitted at the first iteration.
         */

        ngx_crc32_init(hash);

        if (hp->rehash > 0) {
            size = ngx_sprintf(buf, "%ui", hp->rehash) - buf;
            ngx_crc32_update(&hash, buf, size);
        }

        ngx_crc32_update(&hash, hp->key.data, hp->key.len);
        ngx_crc32_final(hash);

        hash = (hash >> 16) & 0x7fff;

        hp->hash += hash;
        hp->rehash++;

        w = hp->hash % hp->rrp.peers->total_weight;
        peer = hp->rrp.peers->peer;
        p = 0;

        while (w >= peer->weight) {
            w -= peer->weight;
            peer = peer->next;
            p++;
        }

        n = p / (8 * sizeof(uintptr_t));
        m = (uintptr_t) 1 << p % (8 * sizeof(uintptr_t));

        if (hp->rrp.tried[n] & m) {
            goto next;
        }

        ngx_log_debug2(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                       "get hash peer, value:%uD, peer:%ui", hp->hash, p);

        if (peer->down) {
            goto next;
        }

        if (peer->max_fails
            && peer->fails >= peer->max_fails
            && now - peer->checked <= peer->fail_timeout)
        {
            goto next;
        }

        break;

    next:

        if (++hp->tries > 20) {
            ngx_http_upstream_rr_peers_unlock(hp->rrp.peers);
            return hp->get_rr_peer(pc, &hp->rrp);
        }
    }

    hp->rrp.current = peer;

    pc->sockaddr = peer->sockaddr;
    pc->socklen = peer->socklen;
    pc->name = &peer->name;

    peer->conns++;

    if (now - peer->checked > peer->fail_timeout) {
        peer->checked = now;
    }

    ngx_http_upstream_rr_peers_unlock(hp->rrp.peers);

    hp->rrp.tried[n] |= m;

    return NGX_OK;
}


ngx_int_t
ngx_http_upconf_init_chash(ngx_conf_t *cf, ngx_http_upstream_srv_conf_t *us)
{
    if (ngx_http_upconf_init_round_robin(cf, us) != NGX_OK) {
        return NGX_ERROR;
    }

    us->peer.init = ngx_http_upstream_init_chash_peer;

    return ngx_http_upconf_init_chash_common(cf->log, us, 0);
}


ngx_int_t
ngx_http_upconf_init_chash_common(ngx_log_t *log,
    ngx_http_upstream_srv_conf_t *us, ngx_uint_t check)
{
    u_char                               *host, *port, c;
    size_t                                host_len, port_len, size;
    uint32_t                              hash, base_hash;
    ngx_str_t                            *server;
    ngx_uint_t                            npoints, i, j;
    ngx_http_upstream_rr_peer_t          *peer;
    ngx_http_upstream_rr_peers_t         *peers;
    ngx_http_upconf_chash_points_t       *points;
    ngx_http_upconf_hash_conf_t          *hcf;
    union {
        uint32_t                          value;
        u_char                            byte[4];
    } prev_hash;

    ngx_http_upconf_srv_conf_t           *scf;
    ngx_http_upconf_hash_checker_data_t  *checkers;
    ngx_slab_pool_t                      *shpool;
    time_t                               *last_update;
    time_t                                now;
    ngx_int_t                             found_idx = NGX_ERROR;

    peers = us->peer.data;
    npoints = peers->total_weight * 160;

    size = sizeof(ngx_http_upconf_chash_points_t)
           + sizeof(ngx_http_upconf_chash_point_t) * (npoints - 1);

    points = ngx_alloc(size, log);
    if (points == NULL) {
        return NGX_ERROR;
    }

    scf = ngx_http_conf_upstream_srv_conf(us, ngx_http_upconf_module);
    hcf = &scf->hash;

    ngx_free(hcf->points);

    points->number = 0;

    for (peer = peers->peer; peer; peer = peer->next) {
        server = &peer->server;

        /*
         * Hash expression is compatible with Cache::Memcached::Fast:
         * crc32(HOST \0 PORT PREV_HASH).
         */

        if (server->len >= 5
            && ngx_strncasecmp(server->data, (u_char *) "unix:", 5) == 0)
        {
            host = server->data + 5;
            host_len = server->len - 5;
            port = NULL;
            port_len = 0;
            goto done;
        }

        for (j = 0; j < server->len; j++) {
            c = server->data[server->len - j - 1];

            if (c == ':') {
                host = server->data;
                host_len = server->len - j - 1;
                port = server->data + server->len - j;
                port_len = j;
                goto done;
            }

            if (c < '0' || c > '9') {
                break;
            }
        }

        host = server->data;
        host_len = server->len;
        port = NULL;
        port_len = 0;

    done:

        ngx_crc32_init(base_hash);
        ngx_crc32_update(&base_hash, host, host_len);
        ngx_crc32_update(&base_hash, (u_char *) "", 1);
        ngx_crc32_update(&base_hash, port, port_len);

        prev_hash.value = 0;
        npoints = peer->weight * 160;

        for (j = 0; j < npoints; j++) {
            hash = base_hash;

            ngx_crc32_update(&hash, prev_hash.byte, 4);
            ngx_crc32_final(hash);

            points->point[points->number].hash = hash;
            points->point[points->number].server = server;
            points->number++;

#if (NGX_HAVE_LITTLE_ENDIAN)
            prev_hash.value = hash;
#else
            prev_hash.byte[0] = (u_char) (hash & 0xff);
            prev_hash.byte[1] = (u_char) ((hash >> 8) & 0xff);
            prev_hash.byte[2] = (u_char) ((hash >> 16) & 0xff);
            prev_hash.byte[3] = (u_char) ((hash >> 24) & 0xff);
#endif
        }
    }

    ngx_qsort(points->point,
              points->number,
              sizeof(ngx_http_upconf_chash_point_t),
              ngx_http_upstream_chash_cmp_points);

    for (i = 0, j = 1; j < points->number; j++) {
        if (points->point[i].hash != points->point[j].hash) {
            points->point[++i] = points->point[j];
        }
    }

    points->number = i + 1;

    hcf->points = points;

    if (!check) {
        return NGX_OK;
    }

    checkers = scf->hash_checkers->elts;

    for (i = 0; i < scf->hash_checkers->nelts; i++) {
        if (checkers[i].name.len == us->shm_zone->shm.name.len
            && ngx_strncmp(checkers[i].name.data, us->shm_zone->shm.name.data,
                           checkers[i].name.len) == 0)
        {
            found_idx = i;
            break;
        }
    }

    if (found_idx == NGX_ERROR) {
        return NGX_OK;
    }

    now = ngx_time();

    shpool = (ngx_slab_pool_t *) checkers[found_idx].shm_zone->shm.addr;

    ngx_shmtx_lock(&shpool->mutex);

    last_update = checkers[found_idx].shm_zone->data;
    *last_update = now;

    ngx_shmtx_unlock(&shpool->mutex);

    checkers[found_idx].last_update = now;

    return NGX_OK;
}


static int ngx_libc_cdecl
ngx_http_upstream_chash_cmp_points(const void *one, const void *two)
{
    ngx_http_upconf_chash_point_t *first =
                                       (ngx_http_upconf_chash_point_t *) one;
    ngx_http_upconf_chash_point_t *second =
                                       (ngx_http_upconf_chash_point_t *) two;

    if (first->hash < second->hash) {
        return -1;

    } else if (first->hash > second->hash) {
        return 1;

    } else {
        return 0;
    }
}


static ngx_uint_t
ngx_http_upstream_find_chash_point(ngx_http_upconf_chash_points_t *points,
    uint32_t hash)
{
    ngx_uint_t                        i, j, k;
    ngx_http_upconf_chash_point_t    *point;

    /* find first point >= hash */

    point = &points->point[0];

    i = 0;
    j = points->number;

    while (i < j) {
        k = (i + j) / 2;

        if (hash > point[k].hash) {
            i = k + 1;

        } else if (hash < point[k].hash) {
            j = k;

        } else {
            return k;
        }
    }

    return i;
}


static ngx_int_t
ngx_http_upstream_init_chash_peer(ngx_http_request_t *r,
    ngx_http_upstream_srv_conf_t *us)
{
    uint32_t                              hash;
    ngx_http_upconf_hash_conf_t          *hcf;
    ngx_http_upstream_hash_peer_data_t   *hp;

    ngx_uint_t                            i;
    ngx_http_upconf_srv_conf_t           *scf;
    ngx_http_upconf_hash_checker_data_t  *checkers;
    ngx_slab_pool_t                      *shpool;
    time_t                               *last_update;
    ngx_http_upconf_hash_peer_data_t     *peer_data;
    ngx_int_t                             found_idx = NGX_ERROR;
    ngx_uint_t                            update_points = 0;

    if (ngx_http_upstream_init_hash_peer(r, us) != NGX_OK) {
        return NGX_ERROR;
    }

    hp = r->upstream->peer.data;

    hp->get_rr_peer = ngx_http_upconf_get_round_robin_peer;

    peer_data = ngx_palloc(r->pool, sizeof(ngx_http_upconf_hash_peer_data_t));
    if (peer_data == NULL) {
        return NGX_ERROR;
    }

    peer_data->rrp = &hp->rrp;
    peer_data->hp = hp;

    r->upstream->peer.data = peer_data;

    r->upstream->peer.get = ngx_http_upstream_get_chash_peer;
    r->upstream->peer.free = ngx_http_upconf_free_round_robin_peer;
#if (NGX_HTTP_SSL)
    r->upstream->peer.set_session =
                               ngx_http_upconf_set_round_robin_peer_session;
    r->upstream->peer.save_session =
                               ngx_http_upconf_save_round_robin_peer_session;
#endif

    scf = ngx_http_conf_upstream_srv_conf(us, ngx_http_upconf_module);
    hcf = &scf->hash;

    hash = ngx_crc32_long(hp->key.data, hp->key.len);

    checkers = scf->hash_checkers->elts;

    for (i = 0; i < scf->hash_checkers->nelts; i++) {
        if (checkers[i].name.len == us->shm_zone->shm.name.len
            && ngx_strncmp(checkers[i].name.data, us->shm_zone->shm.name.data,
                           checkers[i].name.len) == 0)
        {
            found_idx = i;
            break;
        }
    }

    if (found_idx != NGX_ERROR) {
        shpool = (ngx_slab_pool_t *) checkers[found_idx].shm_zone->shm.addr;

        ngx_shmtx_lock(&shpool->mutex);

        last_update = checkers[found_idx].shm_zone->data;

        if (*last_update > checkers[found_idx].last_update) {
            checkers[found_idx].last_update = *last_update;
            update_points = 1;
        }

        ngx_shmtx_unlock(&shpool->mutex);
    }

    ngx_http_upstream_rr_peers_rlock(hp->rrp.peers);

    peer_data->number = peer_data->rrp->peers->number;

    if (update_points) {
        ngx_http_upconf_init_chash_common(r->connection->log, us, 0);
    }

    hp->hash = ngx_http_upstream_find_chash_point(hcf->points, hash);

    ngx_http_upstream_rr_peers_unlock(hp->rrp.peers);

    return NGX_OK;
}


static ngx_int_t
ngx_http_upstream_get_chash_peer(ngx_peer_connection_t *pc, void *data)
{
    ngx_http_upconf_hash_peer_data_t    *peer_data = data;
    ngx_http_upstream_hash_peer_data_t  *hp;

    time_t                              now;
    intptr_t                            m;
    ngx_str_t                          *server;
    ngx_int_t                           total;
    ngx_uint_t                          i, n, best_i;
    ngx_http_upstream_rr_peer_t        *peer, *best;
    ngx_http_upstream_rr_peers_t       *peers;
    ngx_http_upconf_chash_point_t      *point;
    ngx_http_upconf_chash_points_t     *points;
    ngx_http_upconf_hash_conf_t        *hcf;

    ngx_log_debug1(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                   "get consistent hash peer, try: %ui", pc->tries);

    pc->cached = 0;
    pc->connection = NULL;

    hp = peer_data->hp;
    peers = hp->rrp.peers;

    ngx_http_upstream_rr_peers_wlock(peers);

    if (peers->number > peer_data->number && peers->number > 8) {
        /* hp->rrp.tried storage capacity is not big enough for new servers! */
        ngx_http_upstream_rr_peers_unlock(peers);
        return NGX_BUSY;
    }

    now = ngx_time();
    hcf = hp->conf;

    points = hcf->points;
    point = &points->point[0];

    for ( ;; ) {
        server = point[hp->hash % points->number].server;

        ngx_log_debug2(NGX_LOG_DEBUG_HTTP, pc->log, 0,
                       "consistent hash peer:%uD, server:\"%V\"",
                       hp->hash, server);

        best = NULL;
        best_i = 0;
        total = 0;

        for (peer = peers->peer, i = 0;
             peer;
             peer = peer->next, i++)
        {

            n = i / (8 * sizeof(uintptr_t));
            m = (uintptr_t) 1 << i % (8 * sizeof(uintptr_t));

            if (hp->rrp.tried[n] & m) {
                continue;
            }

            if (peer->down) {
                continue;
            }

            if (peer->server.len != server->len
                || ngx_strncmp(peer->server.data, server->data, server->len)
                   != 0)
            {
                continue;
            }

            if (peer->max_fails
                && peer->fails >= peer->max_fails
                && now - peer->checked <= peer->fail_timeout)
            {
                continue;
            }

            peer->current_weight += peer->effective_weight;
            total += peer->effective_weight;

            if (peer->effective_weight < peer->weight) {
                peer->effective_weight++;
            }

            if (best == NULL || peer->current_weight > best->current_weight) {
                best = peer;
                best_i = i;
            }
        }

        if (best) {
            best->current_weight -= total;
            goto found;
        }

        hp->hash++;
        hp->tries++;

        if (hp->tries >= points->number) {
            ngx_http_upstream_rr_peers_unlock(peers);
            return NGX_BUSY;
        }
    }

found:

    hp->rrp.current = best;

    peer_data->sockaddr = *best->sockaddr;

    pc->sockaddr = &peer_data->sockaddr;
    pc->socklen = best->socklen;
    pc->name = &best->name;

    best->conns++;

    if (now - best->checked > best->fail_timeout) {
        best->checked = now;
    }

    ngx_http_upstream_rr_peers_unlock(peers);

    n = best_i / (8 * sizeof(uintptr_t));
    m = (uintptr_t) 1 << best_i % (8 * sizeof(uintptr_t));

    hp->rrp.tried[n] |= m;

    return NGX_OK;
}

