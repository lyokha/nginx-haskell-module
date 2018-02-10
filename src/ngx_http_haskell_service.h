/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_service.h
 *
 *    Description:  Haskell services
 *
 *        Version:  1.0
 *        Created:  05.02.2018 15:29:21
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_SERVICE_H
#define NGX_HTTP_HASKELL_SERVICE_H

#include <ngx_core.h>
#include <ngx_http.h>


ngx_int_t ngx_http_haskell_init_services(ngx_cycle_t *cycle);
void ngx_http_haskell_stop_services(ngx_cycle_t *cycle);
ngx_int_t ngx_http_haskell_service_var_init_zone(ngx_shm_zone_t *shm_zone,
    void *data);
ngx_int_t ngx_http_haskell_service_hooks_init_zone(ngx_shm_zone_t *shm_zone,
    void *data);
ngx_int_t ngx_http_haskell_setup_service_hook(ngx_cycle_t *cycle,
    ngx_array_t *service_code_vars, ngx_http_variable_t *cmvars,
    ngx_http_haskell_service_hook_t *hook);
ngx_int_t ngx_http_haskell_run_service_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
ngx_int_t ngx_http_haskell_shm_update_var_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);
ngx_int_t ngx_http_haskell_shm_stats_var_handler(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);

#endif /* NGX_HTTP_HASKELL_SERVICE_H */

