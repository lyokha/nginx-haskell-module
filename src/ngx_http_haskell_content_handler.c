/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_content_handler.c
 *
 *    Description:  Haskell content handlers
 *
 *        Version:  2.0
 *        Created:  05.02.2018 16:46:41
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_content_handler.h"
#include "ngx_http_haskell_async_handler.h"
#include "ngx_http_haskell_service.h"
#include "ngx_http_haskell_util.h"


static void ngx_http_haskell_content_handler_cleanup(void *data);


ngx_int_t
ngx_http_haskell_content_handler(ngx_http_request_t *r)
{
    ngx_int_t                                 i;
    ngx_http_haskell_main_conf_t             *mcf;
    ngx_http_haskell_loc_conf_t              *lcf;
    ngx_http_haskell_ctx_t                   *ctx;
    ngx_http_haskell_handler_t               *handlers;
    ngx_http_complex_value_t                 *args;
    ngx_str_t                                 arg = ngx_null_string;
    ngx_str_t                                 ct = ngx_string("text/plain");
    HsInt32                                   len = 0, st = NGX_HTTP_OK;
    HsWord32                                  err;
    size_t                                    slen;
    ngx_str_t                                *res, buf;
    u_char                                   *sres = NULL;
    char                                     *eres = NULL;
    ngx_int_t                                 elen = 0;
    ngx_str_t                                 ereslen;
    ngx_chain_t                              *out, *out_cur;
    ngx_buf_t                                *b;
    ngx_pool_cleanup_t                       *cln;
    ngx_http_haskell_content_handler_data_t  *clnd = NULL;
    ngx_pool_t                               *pool;
    HsStablePtr                               locked_bytestring = NULL;
    HsStablePtr                               locked_ct = NULL;
    ngx_uint_t                                def_handler;
    ngx_uint_t                                unsafe_handler;
    ngx_uint_t                                async_handler;
    ngx_int_t                                 rc;

    if (ngx_http_discard_request_body(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;

    def_handler    = handlers[lcf->content_handler->handler].type
                                == ngx_http_haskell_handler_type_y_y ? 1 : 0;
    unsafe_handler = handlers[lcf->content_handler->handler].type
                                == ngx_http_haskell_handler_type_uch ? 1 : 0;
    async_handler  = handlers[lcf->content_handler->handler].type
                                == ngx_http_haskell_handler_type_ach
                    || handlers[lcf->content_handler->handler].type
                                == ngx_http_haskell_handler_type_ach_rb ? 1 : 0;

    if (lcf->static_content && lcf->content_handler_data != NULL) {
        res = lcf->content_handler_data->yy_cleanup_data.bufs;
        len = lcf->content_handler_data->yy_cleanup_data.n_bufs;
        if (!def_handler) {
            ct = lcf->content_handler_data->content_type;
        }
        st = lcf->content_handler_data->status;
        goto send_response;
    }

    args = lcf->content_handler->args;

    if (args && ngx_http_complex_value(r, args, &arg) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    res = &buf;

    switch (handlers[lcf->content_handler->handler].type) {
    case ngx_http_haskell_handler_type_y_y:
        err = ((ngx_http_haskell_handler_dch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &res, &len, &eres,
                     &locked_bytestring);
        elen = len;
        break;
    case ngx_http_haskell_handler_type_ch:
        err = ((ngx_http_haskell_handler_ch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &res, &len, &ct.data, &ct.len,
                     &locked_ct, &st, &locked_bytestring);
        elen = st;
        eres = (char *) ct.data;
        break;
    case ngx_http_haskell_handler_type_uch:
        err = ((ngx_http_haskell_handler_uch)
               handlers[lcf->content_handler->handler].self)
                    (arg.data, arg.len, &sres, &slen, &ct.data, &ct.len, &st);
        len = slen;
        elen = st;
        eres = (char *) ct.data;
        break;
    case ngx_http_haskell_handler_type_ach:
    case ngx_http_haskell_handler_type_ach_rb:
        ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
        if (ctx == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "no request context while running "
                          "haskell async content handler");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        if (ctx->content_handler_data == NULL) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "no content handler data, "
                          "probably it was cleared by internal redirect");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        if (!ctx->content_handler_data->complete) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "content handler data is not ready");
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        err = ctx->content_handler_data->error;
        res = ctx->content_handler_data->yy_cleanup_data.bufs;
        len = ctx->content_handler_data->yy_cleanup_data.n_bufs;
        ct = ctx->content_handler_data->content_type;
        st = ctx->content_handler_data->status;
        if (err) {
            rc = ngx_http_haskell_yy_handler_result(r->connection->log, r->pool,
                    res, len, &ereslen,
                    ctx->content_handler_data->
                        yy_cleanup_data.hs_free_stable_ptr,
                    ctx->content_handler_data->
                        yy_cleanup_data.locked_bytestring,
                    NULL, 0, 0);
            if (rc == NGX_ERROR) {
                ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                              "failed to allocate memory for error message");
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }
            elen = ereslen.len;
            eres = (char *) ereslen.data;
        }
        break;
    default:
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (err) {
        len = 0;
    }

    if (elen == -1 || len == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while running "
                      "haskell content handler");
        if (unsafe_handler) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        goto cleanup;
    }

    if (err) {
        if (eres == NULL) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                          "impossible branch while running "
                          "haskell content handler");
        } else {
            ereslen.len = elen;
            ereslen.data = (u_char *) eres;
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while running "
                          "haskell content handler: \"%V\"", &ereslen);
            if (def_handler) {
                ngx_free(eres);
            }
        }
        if (unsafe_handler) {
            ngx_free(eres);
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }
        goto cleanup;
    }

    if (unsafe_handler || async_handler) {
        goto send_response;
    }

    if (res == NULL && len != 0) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "impossible branch while running "
                      "haskell content handler");
        goto cleanup;
    }

    if (!lcf->static_content || lcf->content_handler_data == NULL) {
        pool = lcf->static_content ? lcf->pool : r->pool;
        cln = ngx_pool_cleanup_add(pool, 0);
        clnd = ngx_pcalloc(pool,
                           sizeof(ngx_http_haskell_content_handler_data_t));
        if (cln == NULL || clnd == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to register cleanup handler for "
                          "content handler data");
            mcf->hs_free_stable_ptr(locked_bytestring);
            goto cleanup;
        }
        /* do not let hs_free_stable_ptr() after hs_exit() for static content */
        clnd->yy_cleanup_data.hs_free_stable_ptr =
                        lcf->static_content ? NULL : mcf->hs_free_stable_ptr;
        clnd->yy_cleanup_data.locked_bytestring =
                        lcf->static_content ? NULL : locked_bytestring;
        clnd->yy_cleanup_data.bufs = res;
        clnd->yy_cleanup_data.n_bufs = len;
        if (res == &buf) {
            if (len != 1) {
                ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                              "impossible branch while running "
                              "haskell content handler");
                goto cleanup;
            }
            clnd->yy_cleanup_data.bufs = ngx_palloc(pool, sizeof(ngx_str_t));
            if (clnd->yy_cleanup_data.bufs == NULL) {
                ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                              "failed to allocate data buffer for "
                              "content handler data");
                mcf->hs_free_stable_ptr(locked_bytestring);
                goto cleanup;
            }
            *clnd->yy_cleanup_data.bufs = buf;
        }
        clnd->content_type.len = def_handler ? 0 : ct.len;
        clnd->content_type.data = def_handler ? NULL : ct.data;
        clnd->locked_ct = def_handler ? NULL : locked_ct;
        clnd->has_locked_ct = def_handler || ct.len == 0 ? 0 : 1;
        clnd->status = st;
        cln->handler = ngx_http_haskell_content_handler_cleanup;
        cln->data = clnd;
    }

    if (lcf->static_content && lcf->content_handler_data == NULL) {
        lcf->content_handler_data = clnd;
    }

send_response:

    r->headers_out.status = st;
    r->headers_out.content_length_n = 0;

    out = NULL;

    if (len > 0) {
        if (unsafe_handler) {
            out = ngx_pcalloc(r->pool, sizeof(ngx_chain_t));
            if (out == NULL) {
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }
            b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
            if (b == NULL) {
                return NGX_HTTP_INTERNAL_SERVER_ERROR;
            }

            b->pos = b->start = sres;
            b->last = b->end = sres + len;
            b->memory = 1;
            b->last_buf = (r == r->main) ? 1 : 0;
            b->last_in_chain = 1;
            r->headers_out.content_length_n = len;

            out->buf = b;
            out->next = NULL;
        } else {
            for (i = len - 1; i >= 0; i--) {
                out_cur = out;
                out = ngx_pcalloc(r->pool, sizeof(ngx_chain_t));
                if (out == NULL) {
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }
                b = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
                if (b == NULL) {
                    return NGX_HTTP_INTERNAL_SERVER_ERROR;
                }

                b->pos = b->start = res[i].data;
                b->last = b->end = res[i].data + res[i].len;
                b->memory = 1;
                if (out_cur == NULL) {
                    b->last_buf = (r == r->main) ? 1 : 0;
                    b->last_in_chain = 1;
                }
                r->headers_out.content_length_n += res[i].len;

                out->buf = b;
                out->next = out_cur;
            }
        }
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

cleanup:

    if (!async_handler) {
        if (len > 1) {
            ngx_free(res);
        }

        if (!def_handler) {
            if (handlers[lcf->content_handler->handler].type
                == ngx_http_haskell_handler_type_ch && !err)
            {
                if (ct.len > 0) {
                    mcf->hs_free_stable_ptr(locked_ct);
                }
            } else {
                ngx_free(ct.data);
            }
        }
    }

    return NGX_HTTP_INTERNAL_SERVER_ERROR;
}


ngx_int_t
ngx_http_haskell_async_content_handler(ngx_http_request_t *r)
{
    ngx_int_t                                 rc;

    /* start the async task */
    rc = ngx_http_haskell_run_async_content_handler(r);
    if (rc >= NGX_HTTP_SPECIAL_RESPONSE) {
        return rc;
    }

    /* postpone the response until the async task finishes and finalizes
     * the request with ngx_http_haskell_content_handler() */
    r->main->count++;

    return NGX_DONE;
}


ngx_int_t
ngx_http_haskell_service_hook(ngx_http_request_t *r)
{
    ngx_http_haskell_main_conf_t             *mcf;
    ngx_http_haskell_loc_conf_t              *lcf;
    ngx_http_complex_value_t                 *args;
    ngx_str_t                                 arg = ngx_null_string;
    ngx_http_haskell_service_hook_t          *service_hooks;

    if (ngx_http_discard_request_body(r) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);
    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);

    if (lcf->service_hook_index == NGX_ERROR
        || mcf->service_hooks.nelts < (ngx_uint_t) lcf->service_hook_index)
    {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "unexpected service hook index %ui",
                      lcf->service_hook_index);
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    service_hooks = mcf->service_hooks.elts;

    if (service_hooks[lcf->service_hook_index].service_code_var_index
        == NGX_DECLINED)
    {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                      "service hook was disabled because of inappropriate "
                      "variable handler");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (service_hooks[lcf->service_hook_index].service_code_var_index
        == NGX_AGAIN)
    {
        return NGX_HTTP_SERVICE_UNAVAILABLE;
    }

    args = lcf->content_handler->args;

    if (args && ngx_http_complex_value(r, args, &arg) != NGX_OK) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (arg.len > 0 && mcf->service_hooks_shm_zone == NULL) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "service hook provides data, but service hooks shm zone "
                      "was not initialized");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (mcf->service_hooks_shm_zone != NULL
        && ngx_http_haskell_update_service_hook_data(r, lcf->service_hook_index,
                                                     arg)
        != NGX_OK)
    {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "service hook data failed to update");
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    if (ngx_http_haskell_consume_from_async_event_channel(
                    service_hooks[lcf->service_hook_index].event_channel[0])
        == -1)
    {
        if (ngx_errno != NGX_EAGAIN) {
            ngx_log_error(NGX_LOG_CRIT, r->connection->log, ngx_errno,
                          "failed to read from service hook event channel");
        }
    }

    if (ngx_http_haskell_signal_async_event_channel(
                    service_hooks[lcf->service_hook_index].event_channel[1])
        == -1)
    {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, ngx_errno,
                      "failed to write to service hook event channel");
    }

    r->header_only = 1;
    r->headers_out.content_type_lowcase = NULL;
    r->headers_out.status = NGX_HTTP_OK;
    r->headers_out.content_length_n = 0;

    return ngx_http_send_header(r);
}


static void
ngx_http_haskell_content_handler_cleanup(void *data)
{
    ngx_http_haskell_content_handler_data_t  *clnd = data;

    if (clnd->has_locked_ct) {
        clnd->yy_cleanup_data.hs_free_stable_ptr(clnd->locked_ct);
    }
    ngx_http_haskell_yy_handler_cleanup(&clnd->yy_cleanup_data);
}

