/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_async_handler.c
 *
 *    Description:  Haskell async handlers
 *
 *        Version:  2.0
 *        Created:  05.02.2018 14:49:55
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_async_handler.h"
#include "ngx_http_haskell_content_handler.h"
#include "ngx_http_haskell_util.h"


static ngx_int_t ngx_http_haskell_create_async_task(ngx_http_request_t *r,
    ngx_fd_t fd[2], ngx_event_handler_pt handler, ngx_uint_t *complete,
    CWord *error, ngx_int_t index);
static void ngx_http_haskell_delete_async_task(void *data);
static void ngx_http_haskell_post_handler(ngx_http_request_t *r);
static ngx_int_t ngx_http_haskell_read_request_body(ngx_http_request_t *r,
    ngx_http_haskell_loc_conf_t *lcf, ngx_http_haskell_ctx_t *ctx);
static void ngx_http_haskell_async_event(ngx_event_t *ev);
static ngx_int_t ngx_http_haskell_async_finalize_request(ngx_http_request_t *r,
    ngx_uint_t index, CWord status, ngx_uint_t styled);
static void ngx_http_haskell_async_content_handler_event(ngx_event_t *ev);
static void ngx_http_haskell_async_content_handler_cleanup(void *data);

static ngx_event_t  dummy_write_event;


ngx_int_t
ngx_http_haskell_rewrite_phase_handler(ngx_http_request_t *r)
{
    ngx_uint_t                         i, j;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_haskell_code_var_data_t  *code_vars;
    ngx_http_haskell_ctx_t            *ctx;
    ngx_http_haskell_async_data_t     *async_data, *async_data_elts;
    ngx_http_haskell_var_handle_t     *var_nocacheable;
    ngx_http_haskell_var_cache_t      *var_nocacheable_cache;
    ngx_int_t                          found_idx;
    ngx_uint_t                         task_complete;
    ngx_http_complex_value_t          *args;
    ngx_str_t                          arg1;
    ngx_fd_t                           fd[2];
    ngx_pool_cleanup_t                *cln;
    ngx_uint_t                         rb, rb_skip;
    HsStablePtr                        res;

    static CWord                       dummy_active;

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;
    code_vars = lcf->code_vars.elts;

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (mcf->var_nocacheable.nelts > 0 && ctx == NULL) {
        ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_ctx_t));
        if (ctx == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to create request context, "
                          "declining phase handler");
            return NGX_DECLINED;
        }
        if (ngx_array_init(&ctx->var_nocacheable_cache, r->pool,
                           mcf->var_nocacheable.nelts,
                           sizeof(ngx_http_haskell_var_cache_t)) != NGX_OK
            || ngx_array_push_n(&ctx->var_nocacheable_cache,
                                mcf->var_nocacheable.nelts) == NULL)
        {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to create variable cache, "
                          "declining phase handler");
            return NGX_DECLINED;
        }
        var_nocacheable = mcf->var_nocacheable.elts;
        var_nocacheable_cache = ctx->var_nocacheable_cache.elts;
        for (i = 0; i < ctx->var_nocacheable_cache.nelts; i++) {
            var_nocacheable_cache[i].index = var_nocacheable[i].index;
            var_nocacheable_cache[i].checked = 0;
            ngx_str_null(&var_nocacheable_cache[i].value);
        }
        ngx_http_set_ctx(r, ctx, ngx_http_haskell_module);
    }

    for (i = 0; i < lcf->code_vars.nelts; i++) {
        rb = handlers[code_vars[i].handler].role
                == ngx_http_haskell_handler_role_async_variable_rb;
        if (!rb
            && handlers[code_vars[i].handler].role
            != ngx_http_haskell_handler_role_async_variable)
        {
            continue;
        }
        if (ctx == NULL) {
            ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_ctx_t));
            if (ctx == NULL) {
                goto decline_phase_handler;
            }
            ngx_http_set_ctx(r, ctx, ngx_http_haskell_module);
        }
        if (ctx->async_data.nalloc == 0
            && ngx_array_init(&ctx->async_data, r->pool, 1,
                              sizeof(ngx_http_haskell_async_data_t)) != NGX_OK)
        {
            goto decline_phase_handler;
        }

        if (ctx->waiting_more_request_body) {
            return NGX_DONE;
        }

        if (rb
            && ngx_http_haskell_read_request_body(r, lcf, ctx) == NGX_AGAIN)
        {
            return NGX_DONE;
        }

        rb_skip = ctx->read_request_body_error || ctx->no_request_body;

        found_idx = NGX_ERROR;
        task_complete = 0;
        async_data_elts = ctx->async_data.elts;
        for (j = 0; j < ctx->async_data.nelts; j++) {
            if (async_data_elts[j].index == code_vars[i].index) {
                found_idx = code_vars[i].index;
                if (rb && rb_skip && async_data_elts[j].complete == 0) {
                    async_data_elts[j].complete = 1;
                }
                task_complete = async_data_elts[j].complete;
                break;
            }
        }
        if (found_idx != NGX_ERROR) {
            if (!task_complete) {
                return NGX_DONE;
            }
            continue;
        }

        async_data = ngx_array_push(&ctx->async_data);
        if (async_data == NULL) {
            goto decline_phase_handler;
        }
        async_data->index = code_vars[i].index;
        ngx_str_null(&async_data->result.data);
        async_data->result.complete = 0;
        async_data->yy_cleanup_data.bufs = &async_data->result.data;
        async_data->yy_cleanup_data.n_bufs = 0;
        async_data->yy_cleanup_data.hs_free_stable_ptr =
                                            mcf->hs_free_stable_ptr;
        async_data->yy_cleanup_data.locked_bytestring = NULL;
        async_data->yy_cleanup_data.free_single_buffer = 0;
        async_data->error = 0;
        async_data->ref_count = 0;
        async_data->complete = 0;

        args = code_vars[i].args.elts;
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to compile complex value for "
                          "future async result, skipping IO task");
            async_data->complete = 1;
            continue;
        }

        if (ngx_http_haskell_create_async_task(r, fd,
                                               ngx_http_haskell_async_event,
                                               &async_data->complete,
                                               &async_data->error,
                                               async_data->index)
            != NGX_OK)
        {
            async_data->complete = 1;
            continue;
        }

        cln = ngx_pool_cleanup_add(r->pool, 0);
        if (cln == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to register cleanup handler for "
                          "future async result, skipping IO task");
            async_data->complete = 1;
            continue;
        }

        if (rb) {
            res = ((ngx_http_haskell_handler_async_ioy_yy)
                   handlers[code_vars[i].handler].self)
                    (r->request_body && r->request_body->temp_file
                     && lcf->request_body_read_temp_file ?
                        &r->request_body->temp_file->file.name : NULL,
                     ctx->request_body.elts, ctx->request_body.nelts,
                     arg1.data, arg1.len, fd[1],
                     ngx_http_haskell_module_use_eventfd_channel,
                     &async_data->yy_cleanup_data.bufs,
                     &async_data->yy_cleanup_data.n_bufs, &async_data->error,
                     &async_data->yy_cleanup_data.locked_bytestring);
        } else {
            res = ((ngx_http_haskell_handler_async_ioy_y)
                   handlers[code_vars[i].handler].self)
                    (arg1.data, arg1.len, fd[1], -1, &dummy_active,
                     ngx_http_haskell_module_use_eventfd_channel, 0,
                     &async_data->yy_cleanup_data.bufs,
                     &async_data->yy_cleanup_data.n_bufs, &async_data->error,
                     &async_data->yy_cleanup_data.locked_bytestring);
        }
        mcf->hs_free_stable_ptr(res);

        cln->handler = ngx_http_haskell_yy_handler_cleanup;
        cln->data = &async_data->yy_cleanup_data;

        return NGX_DONE;
    }

    return NGX_DECLINED;

decline_phase_handler:

    ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                  "failed to create an async task, declining phase handler");

    return NGX_DECLINED;
}


ngx_int_t
ngx_http_haskell_run_async_content_handler(ngx_http_request_t *r)
{
    ngx_http_haskell_main_conf_t             *mcf;
    ngx_http_haskell_loc_conf_t              *lcf;
    ngx_http_haskell_handler_t               *handlers;
    ngx_http_haskell_ctx_t                   *ctx;
    ngx_http_complex_value_t                 *args;
    ngx_str_t                                 arg = ngx_null_string;
    ngx_fd_t                                  fd[2];
    ngx_pool_cleanup_t                       *cln;
    ngx_http_haskell_content_handler_data_t  *clnd = NULL;
    ngx_uint_t                                rb;
    HsStablePtr                               res;
    ngx_int_t                                 rc = NGX_OK;

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    if (lcf->content_handler == NULL) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "impossible branch while running "
                      "haskell async content handler");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    handlers = mcf->handlers.elts;
    rb = handlers[lcf->content_handler->handler].type
            == ngx_http_haskell_handler_type_ach_rb;

    if (handlers[lcf->content_handler->handler].type
        != ngx_http_haskell_handler_type_ach
        && !rb)
    {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "impossible branch while running "
                      "haskell async content handler");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (ctx == NULL) {
        ctx = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_ctx_t));
        if (ctx == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to create an async task for content handler, "
                          "declining async content handler");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        ngx_http_set_ctx(r, ctx, ngx_http_haskell_module);
    }

    if (ctx->waiting_more_request_body) {
        return NGX_DONE;
    }

    if (rb) {
        rc = ngx_http_haskell_read_request_body(r, lcf, ctx);
        if (rc == NGX_AGAIN) {
            return NGX_DONE;
        }
        if (rc >= NGX_HTTP_SPECIAL_RESPONSE) {
            return rc;
        }
    }

    clnd = ctx->content_handler_data;
    if (clnd == NULL) {
        clnd = ngx_pcalloc(r->pool,
                           sizeof(ngx_http_haskell_content_handler_data_t));
        if (clnd == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to create an async task for content handler, "
                          "declining phase handler");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        ctx->content_handler_data = clnd;
    }

    if (clnd->complete) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "impossible branch while running "
                      "haskell async content handler: "
                      "async task is already complete");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    args = lcf->content_handler->args;

    if (args && ngx_http_complex_value(r, args, &arg) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to compile complex value for future "
                      "content handler result, skipping IO task");
        clnd->complete = 1;
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (ngx_http_haskell_create_async_task(r, fd,
                                ngx_http_haskell_async_content_handler_event,
                                &clnd->complete, NULL, NGX_ERROR)
        != NGX_OK)
    {
        clnd->complete = 1;
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    cln = ngx_pool_cleanup_add(r->pool, 0);
    if (cln == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to register cleanup handler for future "
                      "content handler result, skipping IO task");
        clnd->complete = 1;
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    clnd->yy_cleanup_data.bufs = ngx_pnalloc(r->pool, sizeof(ngx_str_t));
    if (clnd->yy_cleanup_data.bufs == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate initial buffer for future "
                      "content handler result, skipping IO task");
        clnd->complete = 1;
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }
    clnd->yy_cleanup_data.n_bufs = 0;
    clnd->yy_cleanup_data.hs_free_stable_ptr = mcf->hs_free_stable_ptr;
    clnd->yy_cleanup_data.locked_bytestring = NULL;
    clnd->yy_cleanup_data.free_single_buffer = 0;

    clnd->headers_cleanup_data.bufs = NULL;
    clnd->headers_cleanup_data.n_bufs = 0;
    clnd->headers_cleanup_data.hs_free_stable_ptr = mcf->hs_free_stable_ptr;
    clnd->headers_cleanup_data.locked_bytestring = NULL;
    clnd->headers_cleanup_data.free_single_buffer = 1;

    if (rb) {
        res = ((ngx_http_haskell_handler_ach_rb)
               handlers[lcf->content_handler->handler].self)
                (r->request_body && r->request_body->temp_file
                     && lcf->request_body_read_temp_file ?
                        &r->request_body->temp_file->file.name : NULL,
                 ctx->request_body.elts, ctx->request_body.nelts,
                 arg.data, arg.len, fd[1],
                 ngx_http_haskell_module_use_eventfd_channel,
                 &clnd->content_type.data, &clnd->content_type.len,
                 &clnd->locked_ct, &clnd->status,
                 &clnd->headers_cleanup_data.bufs,
                 &clnd->headers_cleanup_data.n_bufs,
                 &clnd->headers_cleanup_data.locked_bytestring,
                 &clnd->yy_cleanup_data.bufs, &clnd->yy_cleanup_data.n_bufs,
                 &clnd->error, &clnd->yy_cleanup_data.locked_bytestring);
    } else {
        res = ((ngx_http_haskell_handler_ach)
               handlers[lcf->content_handler->handler].self)
                (arg.data, arg.len, fd[1],
                 ngx_http_haskell_module_use_eventfd_channel,
                 &clnd->content_type.data, &clnd->content_type.len,
                 &clnd->locked_ct, &clnd->status,
                 &clnd->headers_cleanup_data.bufs,
                 &clnd->headers_cleanup_data.n_bufs,
                 &clnd->headers_cleanup_data.locked_bytestring,
                 &clnd->yy_cleanup_data.bufs, &clnd->yy_cleanup_data.n_bufs,
                 &clnd->error, &clnd->yy_cleanup_data.locked_bytestring);
    }
    mcf->hs_free_stable_ptr(res);

    cln->handler = ngx_http_haskell_async_content_handler_cleanup;
    cln->data = clnd;

    return NGX_DONE;
}


static ngx_int_t
ngx_http_haskell_create_async_task(ngx_http_request_t *r, ngx_fd_t fd[2],
                                   ngx_event_handler_pt handler,
                                   ngx_uint_t *complete, CWord *error,
                                   ngx_int_t index)
{
    ngx_http_haskell_async_event_t    *hev;
    ngx_event_t                       *event;
    ngx_pool_cleanup_t                *cln;

    if (ngx_http_haskell_open_async_event_channel(fd) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, ngx_errno,
                      "failed to create async event channel for future "
                      "async result, skipping IO task");
        return NGX_ERROR;
    }

    hev = ngx_pcalloc(r->pool, sizeof(ngx_http_haskell_async_event_t));
    event = ngx_pcalloc(r->pool, sizeof(ngx_event_t));
    if (hev == NULL || event == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate memory for future async result, "
                      "skipping IO task");
        ngx_http_haskell_close_async_event_channel(r->connection->log, fd);
        return NGX_ERROR;
    }

    event->data = hev;
    event->handler = handler;
    event->log = r->connection->log;

    hev->s.fd = fd[0];
    hev->r = r;
    hev->complete = complete;
    hev->error = error;
    hev->index = index;

    hev->s.read = event;
    hev->s.write = &dummy_write_event;

    cln = ngx_pool_cleanup_add(r->pool, 0);
    if (cln == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to register cleanup handler for "
                      "future async result, skipping IO task");
        ngx_http_haskell_close_async_event_channel(r->connection->log, fd);
        return NGX_ERROR;
    }

    if (ngx_add_event(event, NGX_READ_EVENT, 0) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to add event for future async result, "
                      "skipping IO task");
        ngx_http_haskell_close_async_event_channel(r->connection->log, fd);
        return NGX_ERROR;
    }

    cln->handler = ngx_http_haskell_delete_async_task;
    cln->data = hev;

    return NGX_OK;
}


static void
ngx_http_haskell_delete_async_task(void *data)
{
    ngx_http_haskell_async_event_t  *hev = data;
    ngx_event_t                     *ev = hev->s.read;

    if (*hev->complete > 1) {
        return;
    }

    if (*hev->complete == 0 && !(ngx_terminate || ngx_exiting)) {
        ngx_log_error(NGX_LOG_CRIT, hev->r->connection->log, 0,
                      "async task is still not finished at request "
                      "termination, its data will probably leak!");
    }

    ev->active = 0;
    if (ngx_del_event(ev, NGX_READ_EVENT, 0) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, hev->r->connection->log, 0,
                      "failed to delete event after async task was finished");
    }

    if (close(hev->s.fd) == -1) {
        ngx_log_error(NGX_LOG_CRIT, hev->r->connection->log, ngx_errno,
                      "failed to close async event channel "
                      "after async task was finished");
    }

    *hev->complete = 2;
}


static void
ngx_http_haskell_post_handler(ngx_http_request_t *r)
{
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_http_haskell_ctx_t            *ctx;
    ngx_chain_t                       *cl;
    ngx_str_t                         *rb;
    ngx_uint_t                         n = 0;

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (ctx == NULL) {
        return;
    }

    if (r->request_body != NULL && r->request_body->temp_file) {
        lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);
        if (!lcf->request_body_read_temp_file
            && ctx->request_body_read_cycle == 0)
        {
            ngx_log_error(NGX_LOG_ALERT, r->connection->log, 0,
                          "request body is being saved in a temporary file, "
                          "exiting from haskell POST handler");
        }
        goto request_body_done;
    }

    if (r->request_body == NULL || r->request_body->bufs == NULL) {
        ctx->no_request_body = 1;
    }

    if (ctx->request_body.nalloc != 0 || ctx->no_request_body) {
        goto request_body_done;
    }

    for (cl = r->request_body->bufs; cl != NULL; cl = cl->next) {
        n++;
    }

    if (ngx_array_init(&ctx->request_body, r->pool, n, sizeof(ngx_str_t))
        != NGX_OK || ngx_array_push_n(&ctx->request_body, n) == NULL)
    {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "failed to allocate memory for haskell POST handler");
        ctx->no_request_body = 1;
        goto request_body_done;
    }

    rb = ctx->request_body.elts;

    n = 0;
    for (cl = r->request_body->bufs; cl != NULL; cl = cl->next) {
        rb[n].len = cl->buf == NULL ? 0 : cl->buf->last - cl->buf->pos;
        rb[n].data = cl->buf == NULL ? NULL : cl->buf->pos;
        n++;
    }

request_body_done:

    ++ctx->request_body_read_cycle;

    if (ctx->waiting_more_request_body) {
        ctx->waiting_more_request_body = 0;
        ngx_http_core_run_phases(r);
    }
}


static ngx_int_t
ngx_http_haskell_read_request_body(ngx_http_request_t *r,
                                   ngx_http_haskell_loc_conf_t *lcf,
                                   ngx_http_haskell_ctx_t *ctx)
{
    ngx_uint_t  rb_skip;
    ngx_uint_t  r_main_count;
    ngx_int_t   rc;

    rb_skip = ctx->read_request_body_error || ctx->no_request_body;

    if (rb_skip || ctx->request_body.nalloc > 0) {
        return NGX_OK;
    }

    if (lcf->request_body_read_temp_file) {
        r->request_body_in_persistent_file = 1;
        r->request_body_in_clean_file = 1;
    }

    r_main_count = r->main->count;
    rc = ngx_http_read_client_request_body(r, ngx_http_haskell_post_handler);
    r->main->count = r_main_count;

    if (rc == NGX_ERROR || rc >= NGX_HTTP_SPECIAL_RESPONSE) {
        ctx->read_request_body_error = 1;
    }

    if (rc == NGX_AGAIN) {
        ctx->waiting_more_request_body = 1;
        return NGX_AGAIN;
    }

    return NGX_OK;
}


static void
ngx_http_haskell_async_event(ngx_event_t *ev)
{
    ngx_http_haskell_async_event_t    *hev = ev->data;

    *hev->complete = 1;
    ngx_http_haskell_delete_async_task(hev);
    *hev->complete = 3;

    if (*hev->error & 0x80000000) {
        ngx_http_finalize_request(hev->r,
                ngx_http_haskell_async_finalize_request(hev->r, hev->index,
                *hev->error & ~0xC0000000, (*hev->error & 0x40000000) != 0));
        return;
    }

    ngx_http_core_run_phases(hev->r);
}


ngx_int_t
ngx_http_haskell_async_finalize_request(ngx_http_request_t *r, ngx_uint_t index,
                                        CWord status, ngx_uint_t styled)
{
    ngx_http_variable_value_t         *value;
    ngx_str_t                          ct = ngx_string("text/plain");
    CInt                               len = 0;
    ngx_chain_t                       *out;
    ngx_buf_t                         *b;
    ngx_int_t                          rc;

    if (ngx_http_discard_request_body(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    value = ngx_http_get_indexed_variable(r, index);
    if (value == NULL || !value->valid) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (status < 100 || status > 999) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "invalid HTTP status %uD", status);
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (styled) {
        return ngx_http_special_response_handler(r, status);
    }

    r->headers_out.status = status;
    r->headers_out.content_length_n = 0;

    len = value->len;
    out = NULL;

    if (len > 0) {
        out = ngx_pcalloc(r->pool, sizeof(ngx_chain_t));
        if (out == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
        if (b == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }

        b->pos = b->start = value->data;
        b->last = b->end = b->pos + len;
        b->memory = 1;
        b->last_buf = (r == r->main) ? 1 : 0;
        b->last_in_chain = 1;
        r->headers_out.content_length_n = len;

        out->buf = b;
        out->next = NULL;

        r->headers_out.content_type = ct;
        r->headers_out.content_type_len = ct.len;
        r->headers_out.content_type_lowcase = NULL;
    }

    r->header_only = r->headers_out.content_length_n == 0 ? 1 : 0;

    rc = ngx_http_send_header(r);

    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    return ngx_http_output_filter(r, out);
}


static void
ngx_http_haskell_async_content_handler_event(ngx_event_t *ev)
{
    ngx_http_haskell_async_event_t    *hev = ev->data;

    *hev->complete = 1;
    ngx_http_haskell_delete_async_task(hev);
    *hev->complete = 3;

    ngx_http_finalize_request(hev->r, ngx_http_haskell_content_handler(hev->r));
}


static void
ngx_http_haskell_async_content_handler_cleanup(void *data)
{
    ngx_http_haskell_content_handler_data_t  *clnd = data;

    if (clnd->content_type.len > 0) {
        clnd->yy_cleanup_data.hs_free_stable_ptr(clnd->locked_ct);
        clnd->content_type.len = 0;
    }
    ngx_http_haskell_yy_handler_cleanup(&clnd->headers_cleanup_data);
    ngx_http_haskell_yy_handler_cleanup(&clnd->yy_cleanup_data);
}


ngx_int_t
ngx_http_haskell_run_async_handler(ngx_http_request_t *r,
                                   ngx_http_variable_value_t *v,
                                   uintptr_t data)
{
    ngx_int_t                         *index = (ngx_int_t *) data;

    ngx_uint_t                         i;
    ngx_http_core_main_conf_t         *cmcf;
    ngx_http_variable_t               *cmvars;
    ngx_http_haskell_ctx_t            *ctx;
    ngx_http_haskell_async_data_t     *async_data_elts;
    ngx_int_t                          found_idx = NGX_ERROR;
    ngx_int_t                          rc;

    if (index == NULL) {
        return NGX_ERROR;
    }

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    if (ctx == NULL) {
        return NGX_ERROR;
    }
    async_data_elts = ctx->async_data.elts;

    for (i = 0; i < ctx->async_data.nelts; i++) {
        if (*index == async_data_elts[i].index) {
            found_idx = i;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    if (async_data_elts[found_idx].yy_cleanup_data.n_bufs == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting "
                      "value of variable \"%V\" asynchronously",
                      &cmvars[*index].name);
        return NGX_ERROR;
    }

    if (!async_data_elts[found_idx].result.complete) {
        rc = ngx_http_haskell_yy_handler_result(r->connection->log, r->pool,
                async_data_elts[found_idx].yy_cleanup_data.bufs,
                async_data_elts[found_idx].yy_cleanup_data.n_bufs,
                &async_data_elts[found_idx].result.data,
                async_data_elts[found_idx].yy_cleanup_data.hs_free_stable_ptr,
                async_data_elts[found_idx].yy_cleanup_data.locked_bytestring,
                &cmvars[*index], 0, 0);
        async_data_elts[found_idx].result.complete = 1;
        if (rc == NGX_ERROR) {
            return NGX_ERROR;
        }
    }

    if (async_data_elts[found_idx].error) {
        ngx_uint_t  finalizing =
                        (async_data_elts[found_idx].error & 0x80000000) != 0;
        CWord       status = async_data_elts[found_idx].error & ~0xC0000000;
        ngx_uint_t  log_level = finalizing ?
                        (status >= NGX_HTTP_SPECIAL_RESPONSE ? NGX_LOG_ALERT
                            : NGX_LOG_INFO) : NGX_LOG_ERR;
        ngx_str_t   event_msg = ngx_string("an exception was caught");

        if (finalizing) {
            event_msg.data = ngx_pnalloc(r->pool,
                    sizeof("HTTP request finalization was requested (status )")
                    + NGX_INT32_LEN);
            if (event_msg.data == NULL) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "failed to allocate memory for logging HTTP "
                              "request finalization");
                return NGX_ERROR;
            }
            event_msg.len = ngx_sprintf(event_msg.data,
                    "HTTP request finalization was requested (status %uD)",
                    status) - event_msg.data;
        }

        ngx_log_error(log_level, r->connection->log, 0,
                      "%V while getting value of variable \"%V\" "
                      "asynchronously: \"%V\"", &event_msg,
                      &cmvars[*index].name,
                      &async_data_elts[found_idx].result);
        /* BEWARE: return the value of the exception (to avoid returning the
         * exception's message, wrap the haskell handler in an exception
         * handler) */
    }

    v->len = async_data_elts[found_idx].result.data.len;
    v->data = async_data_elts[found_idx].result.data.data;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}

