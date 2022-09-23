/*
 * =============================================================================
 *
 *       Filename:  ngx_http_upconf_module.c
 *
 *    Description:  nginx module for dynamic configuration of upstream peers
 *                  from a variable
 *
 *        Version:  1.0
 *        Created:  10.01.2017 13:30:00
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>


#if (NGX_HTTP_UPSTREAM_ZONE)

#define JSMN_STATIC
#include <jsmn.h>

#include "ngx_inet_slab.h"
#include "ngx_http_upconf_module.h"


typedef struct {
    ngx_array_t                   checkers;
    ngx_array_t                   hash_checkers;
} ngx_http_upconf_main_conf_t;


typedef struct {
    ngx_int_t                     index;
} ngx_http_upconf_loc_conf_t;


typedef struct {
    ngx_uint_t                    checked;
} ngx_http_upconf_ctx_t;


typedef struct {
    ngx_array_t                   servers;
    ngx_int_t                     total_weight;
    ngx_uint_t                    weighted;
} ngx_http_upconf_upstream_data_t;


typedef struct {
    ngx_str_t                     addr;
    ngx_int_t                     weight;
    ngx_int_t                     max_fails;
    ngx_int_t                     fail_timeout;
} ngx_http_upconf_server_data_t;


typedef struct {
    ngx_int_t                     index;
    ngx_shm_zone_t               *shm_zone;
    ngx_uint_t                    checked;
} ngx_http_upconf_checker_data_t;


typedef struct {
    u_char                       *url;
    struct sockaddr              *sockaddr;
    ngx_http_upstream_rr_peer_t  *peer;
} ngx_http_upconf_shm_cleanup_data_t;


static ngx_int_t ngx_http_upconf_init(ngx_conf_t *cf);
static void *ngx_http_upconf_create_main_conf(ngx_conf_t *cf);
static void *ngx_http_upconf_create_srv_conf(ngx_conf_t *cf);
static void *ngx_http_upconf_create_loc_conf(ngx_conf_t *cf);
static char *ngx_http_upconf_merge_loc_conf(ngx_conf_t *cf, void *parent,
    void *child);
static ngx_int_t ngx_http_upconf_access_phase_handler(ngx_http_request_t *r);
static ngx_int_t ngx_http_upconf_checker_init_zone(ngx_shm_zone_t *shm_zone,
    void *data);
static char *ngx_http_upconf(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static ngx_int_t ngx_http_upconf_content_handler(ngx_http_request_t *r);
static ngx_http_upstream_srv_conf_t *ngx_http_upconf_get_shm_zone(
    ngx_http_request_t *r, ngx_str_t *upstream);
static ngx_int_t ngx_http_upconf_update(ngx_http_request_t *r, ngx_int_t idx,
    ngx_uint_t init);
static ngx_int_t ngx_http_upconf_update_shm_zone(ngx_http_request_t *r,
    ngx_str_t *zone_name, ngx_http_upconf_upstream_data_t *upstream_data);
static ngx_uint_t ngx_http_upconf_is_shpool_range(ngx_slab_pool_t *shpool,
    void *p);
static char * ngx_http_upconf_round_robin(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char * ngx_http_upconf_hash(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static ngx_int_t ngx_http_upconf_hash_init_zone(ngx_shm_zone_t *shm_zone,
    void *data);


static ngx_command_t ngx_http_upconf_commands[] = {

    { ngx_string("upconf"),
      NGX_HTTP_LOC_CONF|NGX_CONF_TAKE1,
      ngx_http_upconf,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("upconf_round_robin"),
      NGX_HTTP_UPS_CONF|NGX_CONF_NOARGS,
      ngx_http_upconf_round_robin,
      NGX_HTTP_SRV_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("upconf_hash"),
      NGX_HTTP_UPS_CONF|NGX_CONF_TAKE12,
      ngx_http_upconf_hash,
      NGX_HTTP_SRV_CONF_OFFSET,
      0,
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_upconf_module_ctx = {
    NULL,                                    /* preconfiguration */
    ngx_http_upconf_init,                    /* postconfiguration */

    ngx_http_upconf_create_main_conf,        /* create main configuration */
    NULL,                                    /* init main configuration */

    ngx_http_upconf_create_srv_conf,         /* create server configuration */
    NULL,                                    /* merge server configuration */

    ngx_http_upconf_create_loc_conf,         /* create location configuration */
    ngx_http_upconf_merge_loc_conf,          /* merge location configuration */
};


ngx_module_t  ngx_http_upconf_module = {
    NGX_MODULE_V1,
    &ngx_http_upconf_module_ctx,             /* module context */
    ngx_http_upconf_commands,                /* module directives */
    NGX_HTTP_MODULE,                         /* module type */
    NULL,                                    /* init master */
    NULL,                                    /* init module */
    NULL,                                    /* init process */
    NULL,                                    /* init thread */
    NULL,                                    /* exit thread */
    NULL,                                    /* exit process */
    NULL,                                    /* exit master */
    NGX_MODULE_V1_PADDING
};


static ngx_str_t upconf_hash_shm_prefix = ngx_string("upconf_hash");


static ngx_int_t ngx_http_upconf_init(ngx_conf_t *cf)
{
    ngx_http_core_main_conf_t     *cmcf;
    ngx_http_handler_pt           *h;

    cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);

    h = ngx_array_push(&cmcf->phases[NGX_HTTP_ACCESS_PHASE].handlers);
    if (h == NULL) {
        return NGX_ERROR;
    }

    *h = ngx_http_upconf_access_phase_handler;

    return NGX_OK;
}


static void *
ngx_http_upconf_create_main_conf(ngx_conf_t *cf)
{
    ngx_http_upconf_main_conf_t  *mcf;

    mcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_upconf_main_conf_t));
    if (mcf == NULL) {
        return NULL;
    }

    if (ngx_array_init(&mcf->checkers, cf->pool, 1,
                       sizeof(ngx_http_upconf_checker_data_t)) != NGX_OK)
    {
        return NULL;
    }


    if (ngx_array_init(&mcf->hash_checkers, cf->pool, 1,
                       sizeof(ngx_http_upconf_hash_checker_data_t)) != NGX_OK)
    {
        return NULL;
    }

    return mcf;
}


static void *
ngx_http_upconf_create_srv_conf(ngx_conf_t *cf)
{
    ngx_http_upconf_srv_conf_t   *scf;

    ngx_http_upconf_main_conf_t  *mcf;

    scf = ngx_pcalloc(cf->pool, sizeof(ngx_http_upconf_srv_conf_t));
    if (scf == NULL) {
        return NULL;
    }

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_upconf_module);
    scf->hash_checkers = &mcf->hash_checkers;

    return scf;
}


static void *
ngx_http_upconf_create_loc_conf(ngx_conf_t *cf)
{
    ngx_http_upconf_loc_conf_t  *lcf;

    lcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_upconf_loc_conf_t));
    if (lcf == NULL) {
        return NULL;
    }

    lcf->index = NGX_CONF_UNSET;

    return lcf;
}


static char *
ngx_http_upconf_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child)
{
    ngx_http_upconf_loc_conf_t  *prev = parent;
    ngx_http_upconf_loc_conf_t  *conf = child;

    ngx_conf_merge_value(conf->index, prev->index, NGX_CONF_UNSET);

    return NGX_CONF_OK;
}


static
ngx_int_t ngx_http_upconf_access_phase_handler(ngx_http_request_t *r)
{
    ngx_uint_t                       i;
    ngx_http_upconf_main_conf_t     *mcf;
    ngx_http_upconf_loc_conf_t      *lcf;
    ngx_http_core_loc_conf_t        *clcf;
    ngx_http_upconf_ctx_t           *ctx;
    ngx_http_upconf_checker_data_t  *checker;
    ngx_slab_pool_t                 *shpool;
    ngx_uint_t                      *check;
    ngx_int_t                        rc;

    mcf = ngx_http_get_module_main_conf(r, ngx_http_upconf_module);
    checker = mcf->checkers.elts;

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_upconf_module);
    clcf = ngx_http_get_module_loc_conf(r, ngx_http_core_module);

    ctx = ngx_http_get_module_ctx(r, ngx_http_upconf_module);

    for (i = 0; i < mcf->checkers.nelts; i++) {
        if (checker[i].checked) {
            continue;
        }

        shpool = (ngx_slab_pool_t *) checker[i].shm_zone->shm.addr;

        ngx_shmtx_lock(&shpool->mutex);

        checker[i].checked = *((ngx_uint_t *) checker[i].shm_zone->data);

        if (checker[i].checked) {
            ngx_shmtx_unlock(&shpool->mutex);
            continue;
        }

        rc = ngx_http_upconf_update(r, checker[i].index, 1);

        if (rc == NGX_AGAIN) {
            if (clcf->handler != ngx_http_upconf_content_handler) {
                ngx_log_error(NGX_LOG_NOTICE, r->connection->log, 0,
                              "upconf variable \"%V\" not initialized",
                              &checker[i].shm_zone->shm.name);
            }
            ngx_shmtx_unlock(&shpool->mutex);
            continue;
        }

        if (rc != NGX_OK) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to update upconf variable \"%V\"",
                          &checker[i].shm_zone->shm.name);
            ngx_shmtx_unlock(&shpool->mutex);
            continue;
        }

        if (ctx == NULL) {
            ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_upconf_ctx_t));
            if (ctx == NULL) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "failed to create request context");
                ngx_shmtx_unlock(&shpool->mutex);
                continue;
            }
            ngx_http_set_ctx(r, ctx, ngx_http_upconf_module);
        }

        if (checker[i].index == lcf->index) {
            ctx->checked = 1;
        }

        checker[i].checked = 1;
        check = checker[i].shm_zone->data;
        *check = 1;

        ngx_shmtx_unlock(&shpool->mutex);
    }

    return NGX_DECLINED;
}


static
ngx_int_t ngx_http_upconf_checker_init_zone(ngx_shm_zone_t *shm_zone,
                                            void *data)
{
    ngx_slab_pool_t                 *shpool;
    ngx_uint_t                      *check;

    if (shm_zone->shm.exists) {
        return NGX_OK;
    }

    shpool = (ngx_slab_pool_t *) shm_zone->shm.addr;

    check = ngx_slab_calloc(shpool, sizeof(ngx_uint_t));
    if (check == NULL) {
        return NGX_ERROR;
    }

    shpool->data = check;
    shm_zone->data = check;

    return NGX_OK;
}


static char *
ngx_http_upconf(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_upconf_loc_conf_t      *lcf = conf;

    ngx_uint_t                       i;
    ngx_http_upconf_main_conf_t     *mcf;
    ngx_http_core_loc_conf_t        *clcf;
    ngx_str_t                       *value;
    ngx_http_upconf_checker_data_t  *checker;
    ngx_shm_zone_t                  *shm_zone;
    ngx_int_t                        found_idx = NGX_ERROR;

    value = cf->args->elts;

    if (value[1].len < 2 || value[1].data[0] != '$') {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid variable name \"%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }
    value[1].len--;
    value[1].data++;

    lcf->index = ngx_http_get_variable_index(cf, &value[1]);
    if (lcf->index == NGX_ERROR) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "failed to get index for variable \"$%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_upconf_module);
    checker = mcf->checkers.elts;

    for (i = 0; i < mcf->checkers.nelts; ++i)
    {
        if (checker[i].index == lcf->index) {
            found_idx = 1;
            break;
        }
    }

    if (found_idx == NGX_ERROR) {
        shm_zone = ngx_shared_memory_add(cf, &value[1], 2 * ngx_pagesize,
                                              &ngx_http_upconf_module);
        if (shm_zone == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to add checker for variable \"$%V\"", &value[1]);
            return NGX_CONF_ERROR;
        }

        shm_zone->init = ngx_http_upconf_checker_init_zone;
        shm_zone->noreuse = 1;

        checker = ngx_array_push(&mcf->checkers);
        if (checker == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to add checker for variable \"$%V\"", &value[1]);
            return NGX_CONF_ERROR;
        }

        checker->index = lcf->index;
        checker->shm_zone = shm_zone;
        checker->checked = 0;
    }

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_upconf_content_handler;

    return NGX_CONF_OK;
}


static ngx_int_t
ngx_http_upconf_content_handler(ngx_http_request_t *r)
{
    ngx_http_upconf_loc_conf_t      *lcf;
    ngx_http_core_main_conf_t       *cmcf;
    ngx_http_variable_t             *cmvars;
    ngx_http_upconf_ctx_t           *ctx;
    ngx_chain_t                     *out = NULL;
    ngx_buf_t                       *b;
    u_char                          *res;
    ngx_http_variable_value_t       *var;
    ngx_int_t                        rc;

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_upconf_module);

    var = ngx_http_get_indexed_variable(r, lcf->index);
    if (var == NULL || !var->valid || var->not_found) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    ctx = ngx_http_get_module_ctx(r, ngx_http_upconf_module);

    if (ctx == NULL || !ctx->checked) {
        rc = ngx_http_upconf_update(r, lcf->index, 0);
        if (rc != NGX_OK) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to update upconf variable \"%V\"",
                          &cmvars[lcf->index].name);
            return rc;
        }
    }

    r->headers_out.status = NGX_HTTP_OK;
    r->headers_out.content_length_n = 0;

    if (var->len > 0) {
        out = ngx_pcalloc(r->pool, sizeof(ngx_chain_t));
        if (out == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
        if (b == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        res = ngx_pnalloc(r->pool, var->len);
        if (res == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        ngx_memcpy(res, var->data, var->len);

        b->pos = b->start = res;
        b->last = b->end = res + var->len;
        b->memory = 1;
        b->last_buf = (r == r->main) ? 1 : 0;
        b->last_in_chain = 1;
        r->headers_out.content_length_n = var->len;

        out->buf = b;
    }

    r->header_only = r->headers_out.content_length_n == 0 ? 1 : 0;

    rc = ngx_http_send_header(r);
    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    return ngx_http_output_filter(r, out);
}


static ngx_http_upstream_srv_conf_t *
ngx_http_upconf_get_shm_zone(ngx_http_request_t *r, ngx_str_t *upstream)
{
    ngx_uint_t                      i;
    ngx_http_upstream_srv_conf_t   *uscf, **uscfp;
    ngx_http_upstream_main_conf_t  *umcf;

    umcf  = ngx_http_get_module_main_conf(r, ngx_http_upstream_module);
    uscfp = umcf->upstreams.elts;

    for (i = 0; i < umcf->upstreams.nelts; i++) {
        uscf = uscfp[i];
        if (uscf->shm_zone != NULL
            && uscf->shm_zone->shm.name.len == upstream->len
            && ngx_strncmp(uscf->shm_zone->shm.name.data,
                           upstream->data, upstream->len) == 0
            && (uscf->peer.init_upstream == ngx_http_upconf_init_round_robin
                || uscf->peer.init_upstream == ngx_http_upconf_init_chash))
        {
            return uscf;
        }
    }

    return NULL;
}


static ngx_int_t
ngx_http_upconf_update(ngx_http_request_t *r, ngx_int_t idx, ngx_uint_t init)
{
    ngx_int_t                            i, j, k, l, acc;
    ngx_http_variable_value_t           *var;
    jsmn_parser                          jparse;
    jsmntok_t                           *jtok;
    int                                  jrc, jsz, sz, szs;
    ngx_http_upconf_upstream_data_t      upstream_data;
    ngx_http_upconf_server_data_t       *server_data;
    ngx_str_t                            zone_name;
    ngx_str_t                            field, field_value;
    ngx_int_t                            value;

    var = ngx_http_get_indexed_variable(r, idx);

    if (var->len == 0) {
        return init ? NGX_AGAIN : NGX_OK;
    }

    jsmn_init(&jparse);

    jrc = jsmn_parse(&jparse, (char *) var->data, var->len, NULL, 0);
    if (jrc < 0) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "JSON parse error: %d", jrc);
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    jsz = jrc;
    jtok = ngx_palloc(r->pool, sizeof(jsmntok_t) * jsz);
    if (jtok == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate memory to parse JSON data");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    jsmn_init(&jparse);

    jrc = jsmn_parse(&jparse, (char *) var->data, var->len, jtok, jsz);
    if (jrc < 0) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "JSON parse error: %d", jrc);
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

#if 0
    for (i = 0; i < jsz; i++) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "JTOK: %d, %d", jtok[i].type, jtok[i].size);
    }
#endif

    if (jtok[0].type != JSMN_OBJECT) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "unexpected structure of JSON data: "
                      "the whole data is not an object");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    i = 1;
    for ( ;; ) {
        if (i >= jsz) {
            break;
        }

        if (jtok[i].type != JSMN_STRING) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "unexpected structure of JSON data: "
                          "key is not a string");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        if (i + 1 >= jsz || jtok[i + 1].type != JSMN_ARRAY) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "unexpected structure of JSON data: "
                          "server list is not an array");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        sz = jtok[i + 1].size;
        if (sz == 0) {
            ngx_log_error(NGX_LOG_NOTICE, r->connection->log, 0,
                          "unexpected structure of JSON data: "
                          "server list is empty, ignored");
            i += 2;
            continue;
        }

        if (ngx_array_init(&upstream_data.servers, r->pool, sz,
                           sizeof(ngx_http_upconf_server_data_t)) != NGX_OK
            || ngx_array_push_n(&upstream_data.servers, sz) == NULL)
        {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to allocate memory for upstream data");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        server_data = upstream_data.servers.elts;
        upstream_data.total_weight = 0;
        upstream_data.weighted = 0;

        acc = 0;
        for (k = 0; k < sz; k++) {
            if (jtok[acc + i + 2].type != JSMN_OBJECT) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "unexpected structure of JSON data: "
                              "server data is not an object");
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }
            szs = jtok[acc + i + 2].size;
            if (szs == 0) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "unexpected structure of JSON data: "
                              "server data is empty");
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }

            ngx_str_null(&server_data[k].addr);
            server_data[k].weight = 1;
            server_data[k].max_fails = 1;
            server_data[k].fail_timeout = 10;

            ++acc;
            j = acc + i + 2;
            for (l = 0; l < szs; l++) {
                if (jtok[j].type != JSMN_STRING) {
                    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                  "unexpected structure of JSON data: "
                                  "key is not a string");
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                if (jtok[j + 1].type != JSMN_STRING
                    && jtok[j + 1].type != JSMN_PRIMITIVE)
                {
                    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                  "unexpected structure of JSON data: "
                                  "server option is neither a string nor "
                                  "a primitive");
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                field.len = jtok[j].end - jtok[j].start;
                field.data = &var->data[jtok[j].start];
                if (field.len == 4 && ngx_strncmp(field.data, "addr", 4) == 0)
                {
                    if (jtok[j + 1].type != JSMN_STRING) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "server address is not a string");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    field_value.len = jtok[j + 1].end - jtok[j + 1].start;
                    field_value.data = &var->data[jtok[j + 1].start];
                    server_data[k].addr = field_value;
                } else if (field.len == 6
                           && ngx_strncmp(field.data, "weight", 6) == 0)
                {
                    if (jtok[j + 1].type != JSMN_PRIMITIVE) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "weight is not a primitive");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    field_value.len = jtok[j + 1].end - jtok[j + 1].start;
                    field_value.data = &var->data[jtok[j + 1].start];
                    value = ngx_atoi(field_value.data, field_value.len);
                    if (value == NGX_ERROR) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "weight is not a number");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    server_data[k].weight = value;
                    upstream_data.weighted = 1;
                } else if (field.len == 9
                           && ngx_strncmp(field.data, "max_fails", 9) == 0)
                {
                    if (jtok[j + 1].type != JSMN_PRIMITIVE) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "max_fails is not a primitive");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    field_value.len = jtok[j + 1].end - jtok[j + 1].start;
                    field_value.data = &var->data[jtok[j + 1].start];
                    value = ngx_atoi(field_value.data, field_value.len);
                    if (value == NGX_ERROR) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "max_fails is not a number");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    server_data[k].max_fails = value;
                } else if (field.len == 12
                           && ngx_strncmp(field.data, "fail_timeout", 12) == 0)
                {
                    if (jtok[j + 1].type != JSMN_PRIMITIVE) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "fail_timeout is not a primitive");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    field_value.len = jtok[j + 1].end - jtok[j + 1].start;
                    field_value.data = &var->data[jtok[j + 1].start];
                    value = ngx_atoi(field_value.data, field_value.len);
                    if (value == NGX_ERROR) {
                        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                      "unexpected structure of JSON data: "
                                      "fail_timeout is not a number");
                        return NGX_HTTP_INTERNAL_SERVER_ERROR;
                    }
                    server_data[k].fail_timeout = value;
                } else {
                    ngx_log_error(NGX_LOG_NOTICE, r->connection->log, 0,
                                  "unexpected structure of JSON data: "
                                  "unknown field \"%V\", ignored", &field);
                }
                acc += 2;
                j += 2;
            }

            if (server_data[k].addr.len == 0) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "unexpected structure of JSON data: "
                              "server address was not found");
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }

            upstream_data.total_weight += server_data[k].weight;
        }

        zone_name.len = jtok[i].end - jtok[i].start;
        zone_name.data = &var->data[jtok[i].start];

        if (ngx_http_upconf_update_shm_zone(r, &zone_name, &upstream_data)
            != NGX_OK)
        {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }

        i += 2 + acc;
    }

    return NGX_OK;
}


static ngx_int_t
ngx_http_upconf_update_shm_zone(ngx_http_request_t *r, ngx_str_t *zone_name,
                                ngx_http_upconf_upstream_data_t *upstream_data)
{
    ngx_uint_t                           i, size;
    ngx_uint_t                           seqn = 0, seqn_done = 0;
    ngx_slab_pool_t                     *shpool;
    ngx_http_upstream_rr_peer_t         *peer, *existing, *prev, *next;
    ngx_http_upstream_rr_peer_t         *new = NULL;
    ngx_http_upstream_rr_peers_t        *peers;
    ngx_http_upstream_srv_conf_t        *uscf;
    ngx_url_t                            u;
    struct sockaddr                     *sockaddr;
    socklen_t                            socklen;
    ngx_http_upconf_server_data_t       *server_data;
    ngx_array_t                          shm_cleanup_data;
    ngx_http_upconf_shm_cleanup_data_t  *shm_cleanup_data_elts;

    size = upstream_data->servers.nelts;

    uscf = ngx_http_upconf_get_shm_zone(r, zone_name);

    if (uscf == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "upstream \"%V\" not found or not tagged as upconf",
                      zone_name);
        return NGX_OK;
    }

    if (ngx_array_init(&shm_cleanup_data, r->pool, size,
                       sizeof(ngx_http_upconf_shm_cleanup_data_t)) != NGX_OK
        || ngx_array_push_n(&shm_cleanup_data, size) == NULL)
    {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    shm_cleanup_data_elts = shm_cleanup_data.elts;
    ngx_memzero(shm_cleanup_data_elts,
                sizeof(ngx_http_upconf_shm_cleanup_data_t) * size);

    ngx_log_error(NGX_LOG_INFO, r->connection->log, 0,
                  "updating upstream \"%V\"", zone_name);

    shpool = (ngx_slab_pool_t *) uscf->shm_zone->shm.addr;

    peers = uscf->peer.data;

    ngx_http_upstream_rr_peers_wlock(peers);

    ngx_shmtx_lock(&shpool->mutex);

    prev = NULL;

    server_data = upstream_data->servers.elts;

    for (i = 0; i < size; i++) {
        existing = NULL;

        for (peer = peers->peer; peer != NULL; peer = peer->next) {
            if (!seqn_done) {
                seqn = peer->max_conns;
                seqn_done = 1;
            }
            if (server_data[i].addr.len == peer->name.len
                && ngx_strncmp(server_data[i].addr.data, peer->name.data,
                               peer->name.len) == 0)
            {
                existing = peer;
                break;
            }
        }

        ngx_memzero(&u, sizeof(ngx_url_t));
        u.url.data = ngx_slab_alloc_locked(shpool, server_data[i].addr.len);
        if (u.url.data == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to allocate memory for url data");
            goto error_cleanup;
        }
        shm_cleanup_data_elts[i].url = u.url.data;

        ngx_memcpy(u.url.data,
                   server_data[i].addr.data, server_data[i].addr.len);
        u.url.len = server_data[i].addr.len;
        u.default_port = 80;

        if (existing == NULL) {
            if (ngx_parse_url_slab(shpool, &u) != NGX_OK) {
                if (u.err) {
                    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                                  "%s in upstream \"%V\"", u.err, &u.url);
                }
                goto error_cleanup;
            }
            sockaddr = u.addrs[0].sockaddr;
            socklen = u.addrs[0].socklen;
        } else {
            sockaddr = ngx_slab_alloc_locked(shpool, sizeof(struct sockaddr));
            if (sockaddr == NULL) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "failed to allocate memory for sockaddr");
                goto error_cleanup;
            }
            *sockaddr = *existing->sockaddr;
            socklen = existing->socklen;
        }
        shm_cleanup_data_elts[i].sockaddr = sockaddr;

        peer = ngx_slab_calloc_locked(shpool,
                                      sizeof(ngx_http_upstream_rr_peer_t));
        if (peer == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to allocate memory for a new peer");
            goto error_cleanup;
        }
        shm_cleanup_data_elts[i].peer = peer;

        peer->name = u.url;
        peer->server = u.url;
        peer->sockaddr = sockaddr;
        peer->socklen = socklen;
        peer->weight = server_data[i].weight;
        peer->effective_weight = server_data[i].weight;
        peer->current_weight = 0;
        peer->max_fails = server_data[i].max_fails;
        peer->fail_timeout = server_data[i].fail_timeout;

        if (existing) {
            peer->conns = existing->conns;
            peer->fails = existing->fails;
            peer->accessed = existing->accessed;
            peer->checked = existing->checked;
            peer->down = existing->down;
        }

        peer->next = NULL;
        if (prev) {
            prev->next = peer;
        }
        prev = peer;
        if (i == 0) {
            /* BEWARE: misusing peer->max_conns to hold seq number! */
            peer->max_conns = seqn + 1;
            new = peer;
        }
    }

    for (peer = peers->peer; peer != NULL; ) {
        next = peer->next;
#if (NGX_HTTP_SSL)
        if (peer->ssl_session != NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "peer \"%V\" has SSL session established, this is "
                          "not supported in upconf module, SSL data may leak!",
                          &peer->name);
        }
#endif
        if (peer->name.data != peer->server.data) {
            if (ngx_http_upconf_is_shpool_range(shpool, peer->name.data)) {
                ngx_slab_free_locked(shpool, peer->name.data);
            }
        }
        if (ngx_http_upconf_is_shpool_range(shpool, peer->server.data)) {
            ngx_slab_free_locked(shpool, peer->server.data);
        }
        if (ngx_http_upconf_is_shpool_range(shpool, peer->sockaddr)) {
            ngx_slab_free_locked(shpool, peer->sockaddr);
        }
        ngx_slab_free_locked(shpool, peer);
        peer = next;
    }

    peers->peer = new;
    peers->number = size;
    peers->total_weight = upstream_data->total_weight;
    peers->single = size == 1 ? 1 : 0;
    peers->weighted = upstream_data->weighted;

#if nginx_version >= 1019006
    peers->tries = size;
#endif

    if (uscf->peer.init_upstream == ngx_http_upconf_init_chash
        && ngx_http_upconf_init_chash_common(r->connection->log, uscf, 1)
        != NGX_OK)
    {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "upstream \"%V\" failed to update hash", zone_name);
    }

    ngx_shmtx_unlock(&shpool->mutex);

    ngx_http_upstream_rr_peers_unlock(peers);

    return NGX_OK;

error_cleanup:

    for (i = 0; i < size; i++) {
        if (shm_cleanup_data_elts[i].url != NULL) {
            ngx_slab_free_locked(shpool, shm_cleanup_data_elts[i].url);
        }
        if (shm_cleanup_data_elts[i].sockaddr != NULL) {
            ngx_slab_free_locked(shpool, shm_cleanup_data_elts[i].sockaddr);
        }
        if (shm_cleanup_data_elts[i].peer != NULL) {
            ngx_slab_free_locked(shpool, shm_cleanup_data_elts[i].peer);
        }
    }

    ngx_shmtx_unlock(&shpool->mutex);

    ngx_http_upstream_rr_peers_unlock(peers);

    return NGX_HTTP_INTERNAL_SERVER_ERROR;
}


static ngx_uint_t
ngx_http_upconf_is_shpool_range(ngx_slab_pool_t *shpool, void *p)
{
    if ((u_char *) p < shpool->start || (u_char *) p > shpool->end) {
        return 0;
    }

    return 1;
}


static char *
ngx_http_upconf_round_robin(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_str_t                            *value;
    ngx_http_upstream_srv_conf_t         *uscf;

    value = cf->args->elts;

    uscf = ngx_http_conf_get_module_srv_conf(cf, ngx_http_upstream_module);

    if (uscf->shm_zone == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "directive \"%V\" must be defined after \"zone\"",
                           &value[0]);
        return NGX_CONF_ERROR;
    }

    if (uscf->host.len != uscf->shm_zone->shm.name.len
        || ngx_strncmp(uscf->host.data, uscf->shm_zone->shm.name.data,
                       uscf->host.len) != 0)
    {
        ngx_conf_log_error(NGX_LOG_WARN, cf, 0,
                           "names of upstream \"%V\" and zone \"%V\" differ, "
                           "note that upconf finds upstreams by zone name!",
                           &uscf->host, &uscf->shm_zone->shm.name);
    }

    if (uscf->peer.init_upstream) {
        ngx_conf_log_error(NGX_LOG_WARN, cf, 0,
                           "load balancing method redefined");
    }

    uscf->flags = NGX_HTTP_UPSTREAM_CREATE
                  |NGX_HTTP_UPSTREAM_WEIGHT
                  |NGX_HTTP_UPSTREAM_MAX_FAILS
                  |NGX_HTTP_UPSTREAM_FAIL_TIMEOUT
                  |NGX_HTTP_UPSTREAM_DOWN;

    uscf->peer.init_upstream = ngx_http_upconf_init_round_robin;

    return NGX_CONF_OK;
}


static char *
ngx_http_upconf_hash(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_upconf_srv_conf_t           *scf = conf;

    ngx_uint_t                            i;
    ngx_str_t                            *value;
    ngx_http_upconf_main_conf_t          *mcf;
    ngx_http_upstream_srv_conf_t         *uscf;
    ngx_http_compile_complex_value_t      ccv;
    ngx_http_upconf_hash_checker_data_t  *checker;
    ngx_shm_zone_t                       *shm_zone;
    ngx_str_t                             zone_name = ngx_null_string;

    value = cf->args->elts;

    ngx_memzero(&ccv, sizeof(ngx_http_compile_complex_value_t));

    ccv.cf = cf;
    ccv.value = &value[1];
    ccv.complex_value = &scf->hash.key;

    if (ngx_http_compile_complex_value(&ccv) != NGX_OK) {
        return NGX_CONF_ERROR;
    }

    uscf = ngx_http_conf_get_module_srv_conf(cf, ngx_http_upstream_module);

    if (uscf->shm_zone == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "directive \"%V\" must be defined after \"zone\"",
                           &value[0]);
        return NGX_CONF_ERROR;
    }

    if (uscf->host.len != uscf->shm_zone->shm.name.len
        || ngx_strncmp(uscf->host.data, uscf->shm_zone->shm.name.data,
                       uscf->host.len) != 0)
    {
        ngx_conf_log_error(NGX_LOG_WARN, cf, 0,
                           "names of upstream \"%V\" and zone \"%V\" differ, "
                           "note that upconf finds upstreams by zone name!",
                           &uscf->host, &uscf->shm_zone->shm.name);
    }

    if (uscf->peer.init_upstream) {
        ngx_conf_log_error(NGX_LOG_WARN, cf, 0,
                           "load balancing method redefined");
    }

    uscf->flags = NGX_HTTP_UPSTREAM_CREATE
                  |NGX_HTTP_UPSTREAM_WEIGHT
                  |NGX_HTTP_UPSTREAM_MAX_FAILS
                  |NGX_HTTP_UPSTREAM_FAIL_TIMEOUT
                  |NGX_HTTP_UPSTREAM_DOWN;

    if (cf->args->nelts == 2) {
        /*uscf->peer.init_upstream = ngx_http_upconf_init_hash;*/
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "only consistent upconf hash is supported");
        return NGX_CONF_ERROR;

    } else if (ngx_strcmp(value[2].data, "consistent") == 0) {
        uscf->peer.init_upstream = ngx_http_upconf_init_chash;

    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid parameter \"%V\"", &value[2]);
        return NGX_CONF_ERROR;
    }

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_upconf_module);
    checker = mcf->hash_checkers.elts;

    for (i = 0; i < mcf->hash_checkers.nelts; ++i)
    {
        if (checker[i].name.len == uscf->shm_zone->shm.name.len
            && ngx_strncmp(checker[i].name.data, uscf->shm_zone->shm.name.data,
                           checker[i].name.len) == 0)
        {
            return "is duplicate";
        }
    }

    zone_name.len = upconf_hash_shm_prefix.len + uscf->shm_zone->shm.name.len;
    zone_name.data = ngx_pnalloc(cf->pool, zone_name.len);
    if (zone_name.data == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to add upconf hash in zone \"%V\"",
                           &uscf->shm_zone->shm.name);
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(zone_name.data,
               upconf_hash_shm_prefix.data, upconf_hash_shm_prefix.len);
    ngx_memcpy(zone_name.data + upconf_hash_shm_prefix.len,
               uscf->shm_zone->shm.name.data, uscf->shm_zone->shm.name.len);

    shm_zone = ngx_shared_memory_add(cf, &zone_name,
                                     2 * ngx_pagesize, &ngx_http_upconf_module);
    if (shm_zone == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to add upconf hash in zone \"%V\"",
                           &uscf->shm_zone->shm.name);
        return NGX_CONF_ERROR;
    }

    shm_zone->init = ngx_http_upconf_hash_init_zone;
    shm_zone->noreuse = 1;

    checker = ngx_array_push(&mcf->hash_checkers);
    if (checker == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to add upconf hash in zone \"%V\"",
                           &uscf->shm_zone->shm.name);
        return NGX_CONF_ERROR;
    }

    checker->shm_zone = shm_zone;
    checker->name = uscf->shm_zone->shm.name;
    checker->last_update = 0;

    return NGX_CONF_OK;
}


static
ngx_int_t ngx_http_upconf_hash_init_zone(ngx_shm_zone_t *shm_zone, void *data)
{
    ngx_slab_pool_t                 *shpool;
    time_t                          *check;

    if (shm_zone->shm.exists) {
        return NGX_OK;
    }

    shpool = (ngx_slab_pool_t *) shm_zone->shm.addr;

    check = ngx_slab_calloc(shpool, sizeof(time_t));
    if (check == NULL) {
        return NGX_ERROR;
    }

    shpool->data = check;
    shm_zone->data = check;

    return NGX_OK;
}


#endif

