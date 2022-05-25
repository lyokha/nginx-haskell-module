/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_content_handler.h
 *
 *    Description:  Haskell content handlers
 *
 *        Version:  3.0
 *        Created:  05.02.2018 16:44:35
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_CONTENT_HANDLER_H
#define NGX_HTTP_HASKELL_CONTENT_HANDLER_H

#include <ngx_core.h>
#include <ngx_http.h>


ngx_int_t ngx_http_haskell_content_handler(ngx_http_request_t *r);
ngx_int_t ngx_http_haskell_async_content_handler(ngx_http_request_t *r);
ngx_int_t ngx_http_haskell_service_hook(ngx_http_request_t *r);

#endif /* NGX_HTTP_HASKELL_CONTENT_HANDLER_H */

