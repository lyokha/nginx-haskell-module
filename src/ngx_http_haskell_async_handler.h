/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_async_handler.h
 *
 *    Description:  Haskell async handlers
 *
 *        Version:  2.0
 *        Created:  05.02.2018 14:45:46
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_ASYNC_HANDLER_H
#define NGX_HTTP_HASKELL_ASYNC_HANDLER_H

#include <ngx_core.h>
#include <ngx_http.h>


ngx_int_t ngx_http_haskell_rewrite_phase_handler(ngx_http_request_t *r);
ngx_int_t ngx_http_haskell_run_async_content_handler(ngx_http_request_t *r);
ngx_int_t ngx_http_haskell_run_async_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);

#endif /* NGX_HTTP_HASKELL_ASYNC_HANDLER_H */

