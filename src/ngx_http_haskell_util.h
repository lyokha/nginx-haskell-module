/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_util.h
 *
 *    Description:  Utility functions
 *
 *        Version:  2.0
 *        Created:  05.02.2018 15:08:57
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_UTIL_H
#define NGX_HTTP_HASKELL_UTIL_H

#include "ngx_http_haskell_module.h"


ngx_int_t ngx_http_haskell_yy_handler_result(ngx_log_t *log,
    ngx_pool_t *pool, ngx_str_t *bufs, CInt n_bufs, ngx_str_t *res,
    void (*hs_free_stable_ptr)(HsStablePtr), HsStablePtr locked_bytestring,
    ngx_http_variable_t *cmvar, ngx_uint_t cleanup, ngx_uint_t service);
void ngx_http_haskell_yy_handler_cleanup(void *data);
ngx_int_t ngx_http_haskell_open_async_event_channel(ngx_fd_t fd[2]);
void ngx_http_haskell_close_async_event_channel(ngx_log_t *log, ngx_fd_t fd[2]);
ssize_t ngx_http_haskell_signal_async_event_channel(ngx_fd_t fd);
ssize_t ngx_http_haskell_consume_from_async_event_channel(ngx_fd_t fd);


extern const ngx_uint_t ngx_http_haskell_module_use_eventfd_channel;

#endif /* NGX_HTTP_HASKELL_UTIL_H */

