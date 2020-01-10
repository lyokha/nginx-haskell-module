/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_handler.c
 *
 *    Description:  Haskell synchronous handlers
 *
 *        Version:  2.0
 *        Created:  05.02.2018 16:39:06
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_handler.h"
#include "ngx_http_haskell_util.h"


ngx_int_t
ngx_http_haskell_log_phase_handler(ngx_http_request_t *r)
{
    ngx_uint_t                         i;
    ngx_http_haskell_loc_conf_t       *lcf;
    ngx_http_core_main_conf_t         *cmcf;
    ngx_http_variable_t               *cmvars;
    ngx_http_variable_value_t         *value;
    ngx_http_haskell_code_var_data_t  *code_vars;

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    if (!lcf->check_strict) {
        return NGX_DECLINED;
    }

    code_vars = lcf->code_vars.elts;

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    for (i = 0; i < lcf->code_vars.nelts; i++) {
        if (!code_vars[i].strict) {
            continue;
        }

        /* BEWARE: a strict variable can be asked for evaluation more than
         * once during this for-loop pass if its multiple declarations were
         * merged in location configuration hierarchy, however this should
         * not be a problem because all variables (including nocacheable)
         * cache after the first evaluation and should return the cache on
         * the next evaluations */
        value = ngx_http_get_indexed_variable(r, code_vars[i].index);
        if (value == NULL || !value->valid) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to evaluate strict variable \"%V\"",
                          &cmvars[code_vars[i].index].name);
        }
    }

    return NGX_DECLINED;
}


ngx_int_t
ngx_http_haskell_run_handler(ngx_http_request_t *r,
                             ngx_http_variable_value_t *v, uintptr_t data)
{
    ngx_int_t                           *index = (ngx_int_t *) data;

    ngx_uint_t                           i;
    ngx_http_haskell_main_conf_t        *mcf;
    ngx_http_haskell_loc_conf_t         *lcf;
    ngx_http_core_main_conf_t           *cmcf;
    ngx_http_variable_t                 *cmvars;
    ngx_http_haskell_ctx_t              *ctx;
    ngx_http_haskell_wrap_ctx_t         *wctx;
    ngx_int_t                            j, found_idx = NGX_ERROR;
    ngx_http_haskell_var_handle_t       *vars;
    ngx_http_haskell_var_cache_t        *var_nocacheable_cache;
    ngx_http_haskell_handler_t          *handlers;
    ngx_http_haskell_code_var_data_t    *code_vars;
    ngx_http_complex_value_t            *args;
    ngx_str_t                            arg1, arg2, *argn = NULL;
    char                                *res = NULL;
    ngx_str_t                           *res_yy = NULL, buf_yy;
    HsStablePtr                          locked_bytestring = NULL;
    CInt                                 len;
    CUInt                                err;
    ngx_str_t                            reslen;
    ngx_pool_cleanup_t                  *cln;

    if (index == NULL) {
        return NGX_ERROR;
    }

    ctx = ngx_http_get_module_ctx(r, ngx_http_haskell_module);
    wctx = (ngx_http_haskell_wrap_ctx_t *) ctx;
    if (wctx != NULL) {
        var_nocacheable_cache = wctx->var_nocacheable_cache.elts;
        for (i = 0; i < wctx->var_nocacheable_cache.nelts; i++) {
            if (var_nocacheable_cache[i].index == *index
                && var_nocacheable_cache[i].checked)
            {
                v->len = var_nocacheable_cache[i].value.len;
                v->data = var_nocacheable_cache[i].value.data;
                v->valid = 1;
                v->no_cacheable = 0;
                v->not_found = 0;

                return NGX_OK;
            }
        }
    }

    lcf = ngx_http_get_module_loc_conf(r, ngx_http_haskell_module);

    code_vars = lcf->code_vars.elts;

    /* code vars must be traced in reverse order here because location
     * configuration merge algorithm has placed them in such a way in favour
     * of correct processing of async tasks hierarchies */
    for (j = lcf->code_vars.nelts - 1; j >= 0; j--) {
        if (*index == code_vars[j].index) {
            found_idx = j;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    handlers = mcf->handlers.elts;
    args = code_vars[found_idx].args.elts;

    /* this is a bang handler */
    if (code_vars[found_idx].handler == NGX_ERROR) {
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            return NGX_ERROR;
        }

        v->len = arg1.len;
        v->data = arg1.data;
        v->valid = 1;
        v->no_cacheable = 0;
        v->not_found = 0;

        return NGX_OK;
    }

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_s_ss:
    case ngx_http_haskell_handler_type_b_ss:
        if (ngx_http_complex_value(r, &args[1], &arg2) != NGX_OK) {
            return NGX_ERROR;
        }
        /* fall through */
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_ioy_y:
        res_yy = &buf_yy;
        /* fall through */
    case ngx_http_haskell_handler_type_s_s:
    case ngx_http_haskell_handler_type_b_s:
    case ngx_http_haskell_handler_type_b_y:
        if (ngx_http_complex_value(r, &args[0], &arg1) != NGX_OK) {
            return NGX_ERROR;
        }
        break;
    case ngx_http_haskell_handler_type_s_ls:
    case ngx_http_haskell_handler_type_b_ls:
        argn = ngx_palloc(r->pool,
                          sizeof(ngx_str_t) * code_vars[found_idx].args.nelts);
        if (argn == NULL) {
            return NGX_ERROR;
        }
        for (i = 0; i < code_vars[found_idx].args.nelts; i++) {
            if (ngx_http_complex_value(r, &args[i], &argn[i]) != NGX_OK) {
                return NGX_ERROR;
            }
        }
        break;
    default:
        return NGX_ERROR;
    }

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_s_s:
        err = ((ngx_http_haskell_handler_s_s)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_s_ss:
        err = ((ngx_http_haskell_handler_s_ss)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, arg2.data, arg2.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_s_ls:
        err = ((ngx_http_haskell_handler_s_ls)
               handlers[code_vars[found_idx].handler].self)
                    (argn, code_vars[found_idx].args.nelts, &res, &len);
        break;
    case ngx_http_haskell_handler_type_b_s:
        err = ((ngx_http_haskell_handler_b_s)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_b_ss:
        err = ((ngx_http_haskell_handler_b_ss)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, arg2.data, arg2.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_b_ls:
        err = ((ngx_http_haskell_handler_b_ls)
               handlers[code_vars[found_idx].handler].self)
                    (argn, code_vars[found_idx].args.nelts, &res, &len);
        break;
    case ngx_http_haskell_handler_type_y_y:
        err = ((ngx_http_haskell_handler_y_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res_yy, &len, &locked_bytestring);
        break;
    case ngx_http_haskell_handler_type_b_y:
        err = ((ngx_http_haskell_handler_b_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res, &len);
        break;
    case ngx_http_haskell_handler_type_ioy_y:
        err = ((ngx_http_haskell_handler_ioy_y)
               handlers[code_vars[found_idx].handler].self)
                    (arg1.data, arg1.len, &res_yy, &len, &locked_bytestring);
        break;
    default:
        return NGX_ERROR;
    }

    cmcf = ngx_http_get_module_main_conf(r, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    if (len == -1) {
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while running haskell handler");
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "memory allocation error while getting value of "
                      "variable \"%V\"", &cmvars[*index].name);
        return NGX_ERROR;
    }

    reslen.len = len;
    reslen.data = (u_char *) res;

    switch (handlers[code_vars[found_idx].handler].type) {
    case ngx_http_haskell_handler_type_s_s:
    case ngx_http_haskell_handler_type_s_ss:
    case ngx_http_haskell_handler_type_s_ls:
        if (err) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting value of "
                          "variable \"%V\": \"%V\"",
                          &cmvars[*index].name, &reslen);
            ngx_free(res);
            return NGX_ERROR;
        }
        if (res == NULL) {
            if (len == 0) {
                res = "";
            } else {
                ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                              "impossible branch while running "
                              "haskell handler");
                return NGX_ERROR;
            }
        } else {
            cln = ngx_pool_cleanup_add(r->pool, 0);
            if (cln == NULL) {
                ngx_free(res);
                return NGX_ERROR;
            }
            cln->handler = ngx_free;
            cln->data = res;
        }
        break;
    case ngx_http_haskell_handler_type_y_y:
    case ngx_http_haskell_handler_type_ioy_y:
        if (ngx_http_haskell_yy_handler_result(r->connection->log, r->pool,
                                               res_yy, len, &reslen,
                                               mcf->hs_free_stable_ptr,
                                               locked_bytestring,
                                               &cmvars[*index], 1, 0)
            == NGX_ERROR)
        {
            return NGX_ERROR;
        }
        len = reslen.len;
        res = (char *) reslen.data;
        if (err) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting value of "
                          "variable \"%V\": \"%V\"",
                          &cmvars[*index].name, &reslen);
            if (handlers[code_vars[found_idx].handler].type
                != ngx_http_haskell_handler_type_ioy_y)
            {
                return NGX_ERROR;
            }
            /* BEWARE: return the value of the exception in case of the
             * effectful IO handler ngx_http_haskell_handler_type_ioy_y (to
             * avoid returning the exception's message in this case, wrap the
             * haskell handler in an exception handler or add the corresponding
             * variable to the list of directive haskell_var_empty_on_error) */
            vars = mcf->var_empty_on_error.elts;
            for (i = 0; i < mcf->var_empty_on_error.nelts; i++) {
                if (vars[i].index == *index) {
                    return NGX_ERROR;
                }
            }
        }
        break;
    case ngx_http_haskell_handler_type_b_s:
    case ngx_http_haskell_handler_type_b_ss:
    case ngx_http_haskell_handler_type_b_ls:
    case ngx_http_haskell_handler_type_b_y:
        if (res != NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "an exception was caught while getting value of "
                          "variable \"%V\": \"%V\"",
                          &cmvars[*index].name, &reslen);
            ngx_free(res);
            return NGX_ERROR;
        }
        res = err ? "1" : "0";
        len = 1;
        break;
    default:
        return NGX_ERROR;
    }

    if (r->internal) {
        vars = mcf->var_compensate_uri_changes.elts;
        for (i = 0; i < mcf->var_compensate_uri_changes.nelts; i++) {
            if (vars[i].index == *index
                && r->uri_changes < NGX_HTTP_MAX_URI_CHANGES + 1)
            {
                ++r->uri_changes;
                break;
            }
        }
    }

    if (wctx != NULL) {
        var_nocacheable_cache = wctx->var_nocacheable_cache.elts;
        for (i = 0; i < wctx->var_nocacheable_cache.nelts; i++) {
            if (var_nocacheable_cache[i].index == *index) {
                var_nocacheable_cache[i].checked = 1;
                var_nocacheable_cache[i].value.len = len;
                var_nocacheable_cache[i].value.data = (u_char *) res;
                break;
            }
        }
    }

    v->len = len;
    v->data = (u_char *) res;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}

