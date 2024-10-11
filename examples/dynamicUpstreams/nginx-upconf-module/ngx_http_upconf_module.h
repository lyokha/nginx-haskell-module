/*
 * =============================================================================
 *
 *       Filename:  ngx_http_upconf_module.h
 *
 *    Description:  nginx module for dynamic configuration of upstream peers
 *                  from a variable
 *
 *        Version:  1.0
 *        Created:  14.02.2017 17:47:21
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_UPCONF_MODULE_H
#define NGX_UPCONF_MODULE_H

#include <ngx_config.h>
#include <ngx_core.h>

#include "ngx_http_upconf_hash.h"


typedef struct {
    ngx_http_upconf_hash_conf_t   hash;
    ngx_array_t                  *hash_checkers;
} ngx_http_upconf_srv_conf_t;


typedef struct {
    ngx_str_t                     name;
    ngx_shm_zone_t               *shm_zone;
    time_t                        last_update;
} ngx_http_upconf_hash_checker_data_t;


extern ngx_module_t ngx_http_upconf_module;

#endif /* NGX_UPCONF_MODULE_H */

