/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_module.h
 *
 *    Description:  Nginx module for binding Haskell code in conf files
 *
 *        Version:  2.0
 *        Created:  05.02.2018 13:57:04
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef NGX_HTTP_HASKELL_MODULE_H
#define NGX_HTTP_HASKELL_MODULE_H

#include <ngx_core.h>
#include <ngx_http.h>

#include <HsFFI.h>


typedef HsWord32 (*ngx_http_haskell_handler_s_s)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_s_ss)
    (HsPtr, HsInt32, HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_s_ls)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_s)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_ss)
    (HsPtr, HsInt32, HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_ls)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_y_y)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_b_y)
    (HsPtr, HsInt32, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_ioy_y)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr);
typedef HsStablePtr (*ngx_http_haskell_handler_async_ioy_y)
    (HsPtr, HsInt32, HsInt32, HsInt32, HsPtr, HsWord32, HsWord32,
     HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsStablePtr (*ngx_http_haskell_handler_async_ioy_y_service)
    (HsPtr, HsInt32, HsInt32, HsInt32, volatile HsWord32 *, HsWord32, HsWord32,
     HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsStablePtr (*ngx_http_haskell_handler_async_ioy_yy)
    (HsPtr, HsPtr, HsInt32, HsPtr, HsInt32, HsInt32, HsWord32, HsPtr, HsPtr,
     HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_ch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_dch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsWord32 (*ngx_http_haskell_handler_uch)
    (HsPtr, HsInt32, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr);
typedef HsStablePtr (*ngx_http_haskell_handler_ach)
    (HsPtr, HsInt32, HsInt32, HsWord32, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr,
     HsPtr, HsPtr, HsPtr);
typedef HsStablePtr (*ngx_http_haskell_handler_ach_rb)
    (HsPtr, HsPtr, HsInt32, HsPtr, HsInt32, HsInt32, HsWord32, HsPtr, HsPtr,
     HsPtr, HsPtr, HsPtr, HsPtr, HsPtr, HsPtr);


typedef enum {
    ngx_http_haskell_module_wrap_mode_uninitialized = 0,
    ngx_http_haskell_module_wrap_mode_standalone,
    ngx_http_haskell_module_wrap_mode_modular
} ngx_http_haskell_module_wrap_mode_e;


typedef enum {
    ngx_http_haskell_compile_mode_uninitialized = 0,
    ngx_http_haskell_compile_mode_vanilla,
    ngx_http_haskell_compile_mode_debug,
    ngx_http_haskell_compile_mode_threaded,
    ngx_http_haskell_compile_mode_threaded_debug,
    ngx_http_haskell_compile_mode_load_existing
} ngx_http_haskell_compile_mode_e;


typedef enum {
    ngx_http_haskell_handler_type_uninitialized = 0,
    ngx_http_haskell_handler_type_s_s,
    ngx_http_haskell_handler_type_s_ss,
    ngx_http_haskell_handler_type_s_ls,
    ngx_http_haskell_handler_type_b_s,
    ngx_http_haskell_handler_type_b_ss,
    ngx_http_haskell_handler_type_b_ls,
    ngx_http_haskell_handler_type_y_y,
    ngx_http_haskell_handler_type_b_y,
    ngx_http_haskell_handler_type_ioy_y,
    ngx_http_haskell_handler_type_ioy_yy,
    ngx_http_haskell_handler_type_ch,
    ngx_http_haskell_handler_type_uch,
    ngx_http_haskell_handler_type_ach,
    ngx_http_haskell_handler_type_ach_rb
} ngx_http_haskell_handler_type_e;


typedef enum {
    ngx_http_haskell_handler_role_uninitialized = 0,
    ngx_http_haskell_handler_role_variable,
    ngx_http_haskell_handler_role_async_variable,
    ngx_http_haskell_handler_role_async_variable_rb,
    ngx_http_haskell_handler_role_service_variable,
    ngx_http_haskell_handler_role_content_handler,
    ngx_http_haskell_handler_role_async_content_handler,
    ngx_http_haskell_handler_role_async_content_handler_rb,
    ngx_http_haskell_handler_role_service_hook
} ngx_http_haskell_handler_role_e;


typedef struct {
    ngx_http_haskell_module_wrap_mode_e        wrap_mode;
    ngx_http_haskell_compile_mode_e            compile_mode;
    ngx_str_t                                  ghc_extra_options;
    ngx_array_t                                rts_options;
    ngx_array_t                                program_options;
    ngx_str_t                                  lib_path;
    ngx_array_t                                handlers;
    void                                      *dl_handle;
    void                                     (*hs_init)(int *, char ***);
    void                                     (*hs_exit)(void);
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 702
    void                                     (*hs_add_root)(void (*)(void));
    void                                     (*init_HsModule)(void);
#endif
    void                                     (*hs_free_stable_ptr)
                                                                (HsStablePtr);
    void                                     (*terminate_async_task)
                                                                (HsStablePtr);
    void                                     (*service_hook_interrupt)
                                                                (HsStablePtr);
    void                                     (*set_cycle_ptr)(HsPtr);
    void                                     (*set_upstream_main_conf_ptr)
                                                                (HsPtr);
    void                                     (*set_cached_time_ptr)
                                                            (volatile void **);
    HsBool                                   (*rts_has_thread_support)(void);
    ngx_array_t                                service_code_vars;
    ngx_array_t                                var_nocacheable;
    ngx_array_t                                var_compensate_uri_changes;
    ngx_array_t                                service_var_ignore_empty;
    ngx_array_t                                service_var_in_shm;
    ngx_shm_zone_t                            *shm_zone;
    ngx_str_t                                  shm_lock_files_path;
#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
    ngx_fd_t                                   shm_lock_fd;
#endif
    ngx_array_t                                service_hooks;
    ngx_shm_zone_t                            *service_hooks_shm_zone;
    ngx_str_t                                  request_var_name;
    ngx_uint_t                                 code_loaded:1;
    ngx_uint_t                                 module_failed:1;
    ngx_uint_t                                 has_async_tasks:1;
    ngx_uint_t                                 has_async_handlers:1;
    ngx_uint_t                                 request_var_name_done:1;
} ngx_http_haskell_main_conf_t;


typedef struct {
    ngx_int_t                                  handler;
    ngx_http_complex_value_t                  *args;
} ngx_http_haskell_content_handler_t;


typedef struct {
    ngx_str_t                                 *bufs;
    HsInt32                                    n_bufs;
    void                                     (*hs_free_stable_ptr)
                                                                (HsStablePtr);
    HsStablePtr                                locked_bytestring;
} ngx_http_haskell_yy_cleanup_data_t;


typedef struct {
    ngx_http_haskell_yy_cleanup_data_t         yy_cleanup_data;
    ngx_str_t                                  content_type;
    HsStablePtr                                locked_ct;
    HsInt32                                    status;
    HsWord32                                   error;
    ngx_uint_t                                 complete;
    ngx_uint_t                                 has_locked_ct;
} ngx_http_haskell_content_handler_data_t;


typedef struct {
    ngx_array_t                                code_vars;
    ngx_pool_t                                *pool;
    ngx_http_haskell_content_handler_t        *content_handler;
    ngx_http_haskell_content_handler_data_t   *content_handler_data;
    ngx_flag_t                                 request_body_read_temp_file;
    ngx_int_t                                  service_hook_index;
    ngx_uint_t                                 static_content;
} ngx_http_haskell_loc_conf_t;


typedef struct {
    void                                      *self;
    ngx_http_haskell_handler_type_e            type;
    ngx_http_haskell_handler_role_e            role;
    ngx_str_t                                  name;
    ngx_uint_t                                 n_args[3];
    ngx_uint_t                                 unsafe:1;
    ngx_uint_t                                 async:1;
    ngx_uint_t                                 service_hook:1;
} ngx_http_haskell_handler_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_int_t                                  handler;
    ngx_array_t                                args;
    ngx_uint_t                                 async;
} ngx_http_haskell_code_var_data_t;


typedef struct {
    ngx_str_t                                  data;
    ngx_uint_t                                 complete;
} ngx_http_haskell_async_result_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_http_haskell_yy_cleanup_data_t         yy_cleanup_data;
    HsWord32                                   error;
    ngx_http_haskell_async_result_t            result;
    ngx_uint_t                                 ref_count;
    ngx_uint_t                                 complete;
} ngx_http_haskell_async_data_t;


typedef struct {
    ngx_array_t                                async_data;
    ngx_array_t                                var_nocacheable_cache;
    ngx_array_t                                request_body;
    ngx_http_haskell_content_handler_data_t   *content_handler_data;
    ngx_uint_t                                 request_body_read_cycle;
    ngx_uint_t                                 waiting_more_request_body:1;
    ngx_uint_t                                 read_request_body_error:1;
    ngx_uint_t                                 no_request_body:1;
} ngx_http_haskell_ctx_t;


typedef struct {
    ngx_str_t                                  name;
    ngx_int_t                                  index;
    void                                      *data;
} ngx_http_haskell_var_handle_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_msec_t                                 modified;
} ngx_http_haskell_shm_var_handle_data_t;


typedef struct {
    ngx_int_t                                  index;
    ngx_str_t                                  value;
    ngx_uint_t                                 checked;
} ngx_http_haskell_var_cache_t;


typedef struct {
    /* ngx_connection_t stub to allow use c->fd as event ident */
    void                                             *data;
    ngx_event_t                                      *read;
    ngx_event_t                                      *write;
    ngx_fd_t                                          fd;
} ngx_http_haskell_async_event_stub_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t               s;
    ngx_cycle_t                                      *cycle;
    struct ngx_http_haskell_service_hook_s           *hook;
} ngx_http_haskell_service_hook_event_t;


struct ngx_http_haskell_service_hook_s {
    ngx_event_t                                       event;
    ngx_http_haskell_service_hook_event_t             hev;
    ngx_fd_t                                          event_channel[2];
    ngx_int_t                                         handler;
    ngx_int_t                                         service_hook_index;
    ngx_int_t                                         service_code_var_index;
    ngx_uint_t                                        update_hook;
    struct ngx_http_haskell_service_code_var_data_s  *service_code_var;
};

typedef struct ngx_http_haskell_service_hook_s ngx_http_haskell_service_hook_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t               s;
    ngx_http_request_t                               *r;
    ngx_uint_t                                       *complete;
    HsWord32                                         *error;
    ngx_int_t                                         index;
} ngx_http_haskell_async_event_t;


typedef struct {
    ngx_http_haskell_async_event_stub_t               s;
    ngx_cycle_t                                      *cycle;
    struct ngx_http_haskell_service_code_var_data_s  *service_code_var;
    ngx_uint_t                                        first_run;
} ngx_http_haskell_service_async_event_t;


struct ngx_http_haskell_service_code_var_data_s {
    ngx_http_haskell_code_var_data_t                 *data;
    ngx_http_haskell_async_data_t                     future_async_data;
    ngx_http_haskell_async_data_t                    *async_data;
    ngx_event_t                                       event;
    ngx_http_haskell_service_async_event_t            hev;
    ngx_int_t                                         shm_index;
    ngx_fd_t                                          shm_lock_fd;
    HsStablePtr                                       locked_async_task;
    volatile HsWord32                                 active;
    ngx_uint_t                                        cb:1;
    ngx_uint_t                                        noarg:1;
    ngx_uint_t                                        running:1;
    ngx_uint_t                                        ignore_empty:1;
    ngx_uint_t                                        has_update_hooks:1;
    ngx_uint_t                                        has_locked_async_task:1;
};

typedef struct ngx_http_haskell_service_code_var_data_s
    ngx_http_haskell_service_code_var_data_t;


extern const ngx_str_t ngx_http_haskell_module_handler_prefix;

extern ngx_module_t ngx_http_haskell_module;

#endif /* NGX_HTTP_HASKELL_MODULE_H */

