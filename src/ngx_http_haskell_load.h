/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_load.h
 *
 *    Description:  Load compiled Haskell library
 *
 *        Version:  1.0
 *        Created:  05.02.2018 14:18:12
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_LOAD_H
#define NGX_HTTP_HASKELL_LOAD_H

#include <ngx_core.h>


ngx_int_t ngx_http_haskell_load(ngx_cycle_t *cycle);
void ngx_http_haskell_unload(ngx_cycle_t *cycle, ngx_uint_t exiting);

#endif /* NGX_HTTP_HASKELL_LOAD_H */

