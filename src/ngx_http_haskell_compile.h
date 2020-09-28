/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_compile.h
 *
 *    Description:  Compile Haskell code
 *
 *        Version:  2.0
 *        Created:  05.02.2018 13:45:11
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_COMPILE_H
#define NGX_HTTP_HASKELL_COMPILE_H

#include <ngx_core.h>


char *ngx_http_haskell_write_code(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name, ngx_str_t *fragments, ngx_int_t n_fragments);
char *ngx_http_haskell_compile(ngx_conf_t *cf, void *conf,
    ngx_str_t source_name);

#endif /* NGX_HTTP_HASKELL_COMPILE_H */

