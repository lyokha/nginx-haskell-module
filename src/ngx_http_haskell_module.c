/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_module.c
 *
 *    Description:  Nginx module for binding Haskell code in conf files
 *
 *        Version:  1.0
 *        Created:  23.12.2015 12:53:00
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_compile.h"
#include "ngx_http_haskell_load.h"
#include "ngx_http_haskell_handler.h"
#include "ngx_http_haskell_content_handler.h"
#include "ngx_http_haskell_async_handler.h"
#include "ngx_http_haskell_service.h"


const ngx_str_t  ngx_http_haskell_module_handler_prefix =
ngx_string("ngx_hs_");

static const ngx_str_t  haskell_shm_file_lock_prefix =
ngx_string("/ngx_hs_var_");
static const ngx_str_t  haskell_shm_file_lock_suffix =
ngx_string(".lock");
static const ngx_str_t  haskell_module_shm_update_var_prefix =
ngx_string("_upd__");
static const ngx_str_t  haskell_module_shm_stats_var_prefix =
ngx_string("_shm__");

const ngx_uint_t  ngx_http_haskell_module_use_eventfd_channel =
#if (NGX_HAVE_EVENTFD)
    1;
#else
    0;
#endif


static char *ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_run(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char *ngx_http_haskell_content(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char *ngx_http_haskell_var_configure(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf);
static char *ngx_http_haskell_service_var_in_shm(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);
static ngx_int_t ngx_http_haskell_init(ngx_conf_t *cf);
static void *ngx_http_haskell_create_main_conf(ngx_conf_t *cf);
static void *ngx_http_haskell_create_loc_conf(ngx_conf_t *cf);
static char *ngx_http_haskell_merge_loc_conf(ngx_conf_t *cf, void *parent,
    void *child);
static ngx_int_t ngx_http_haskell_init_worker(ngx_cycle_t *cycle);
static void ngx_http_haskell_exit_worker(ngx_cycle_t *cycle);
static void ngx_http_haskell_var_init(ngx_log_t *log, ngx_array_t *cmvar,
    ngx_array_t *var, ngx_http_get_variable_pt get_handler);
static ngx_int_t ngx_http_haskell_shm_lock_init(ngx_cycle_t *cycle,
    ngx_file_t *out, ngx_str_t path, ngx_str_t zone_name, ngx_str_t *var_name,
    int mode);


static ngx_command_t  ngx_http_haskell_module_commands[] = {

    { ngx_string("haskell"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_2MORE,
      ngx_http_haskell,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_ANY,
      ngx_http_haskell_run,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run_async"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_ANY,
      ngx_http_haskell_run,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run_async_on_request_body"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_ANY,
      ngx_http_haskell_run,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_run_service"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_TAKE23,
      ngx_http_haskell_run,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_service_var_update_callback"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_TAKE23,
      ngx_http_haskell_run,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_static_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_unsafe_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_async_content"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_async_content_on_request_body"),
      NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE12,
      ngx_http_haskell_content,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_var_nocacheable"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_var_compensate_uri_changes"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_service_var_ignore_empty"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_service_var_in_shm"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_2MORE,
      ngx_http_haskell_service_var_in_shm,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("haskell_request_body_read_temp_file"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_FLAG,
      ngx_conf_set_flag_slot,
      NGX_HTTP_LOC_CONF_OFFSET,
      offsetof(ngx_http_haskell_loc_conf_t, request_body_read_temp_file),
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_haskell_module_ctx = {
    NULL,                                    /* preconfiguration */
    ngx_http_haskell_init,                   /* postconfiguration */

    ngx_http_haskell_create_main_conf,       /* create main configuration */
    NULL,                                    /* init main configuration */

    NULL,                                    /* create server configuration */
    NULL,                                    /* merge server configuration */

    ngx_http_haskell_create_loc_conf,        /* create location configuration */
    ngx_http_haskell_merge_loc_conf          /* merge location configuration */
};


ngx_module_t  ngx_http_haskell_module = {
    NGX_MODULE_V1,
    &ngx_http_haskell_module_ctx,            /* module context */
    ngx_http_haskell_module_commands,        /* module directives */
    NGX_HTTP_MODULE,                         /* module type */
    NULL,                                    /* init master */
    NULL,                                    /* init module */
    ngx_http_haskell_init_worker,            /* init process */
    NULL,                                    /* init thread */
    NULL,                                    /* exit thread */
    ngx_http_haskell_exit_worker,            /* exit process */
    NULL,                                    /* exit master */
    NGX_MODULE_V1_PADDING
};


static ngx_int_t
ngx_http_haskell_init(ngx_conf_t *cf)
{
    ngx_uint_t                     i;
    ngx_http_haskell_main_conf_t  *mcf;
    ngx_http_core_main_conf_t     *cmcf;
    ngx_array_t                   *hs;
    ngx_http_handler_pt           *h, *hs_elts;

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);
    cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);

    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    if (!mcf->has_async_tasks && mcf->var_nocacheable.nelts == 0) {
        if (mcf->has_async_handlers) {
            goto access_phase;
        }
        return NGX_OK;
    }

    hs = &cmcf->phases[NGX_HTTP_REWRITE_PHASE].handlers;

    h = ngx_array_push_n(hs, 2);
    if (h == NULL) {
        return NGX_ERROR;
    }

    hs_elts = hs->elts;
    for (i = hs->nelts - 2 ; i > 0; i--) {
        hs_elts[i] = hs_elts[i - 1];
    }

    *++h = ngx_http_haskell_rewrite_phase_handler;
    hs_elts[0] = ngx_http_haskell_rewrite_phase_handler;

    if (!mcf->has_async_handlers) {
        return NGX_OK;
    }

access_phase:

    hs = &cmcf->phases[NGX_HTTP_ACCESS_PHASE].handlers;

    h = ngx_array_push(hs);
    if (h == NULL) {
        return NGX_ERROR;
    }

    hs_elts = hs->elts;
    for (i = hs->nelts - 1 ; i > 0; i--) {
        hs_elts[i] = hs_elts[i - 1];
    }

    hs_elts[0] = ngx_http_haskell_access_phase_handler;

    return NGX_OK;
}


static void *
ngx_http_haskell_create_main_conf(ngx_conf_t *cf)
{
    ngx_http_haskell_main_conf_t  *mcf;

    mcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_haskell_main_conf_t));
    if (mcf == NULL) {
        return NULL;
    }

    if (ngx_array_init(&mcf->handlers, cf->pool, 1,
                       sizeof(ngx_http_haskell_handler_t)) != NGX_OK)
    {
        return NULL;
    }

    if (ngx_array_init(&mcf->service_code_vars, cf->pool, 1,
                       sizeof(ngx_http_haskell_service_code_var_data_t))
        != NGX_OK)
    {
        return NULL;
    }

#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
    mcf->shm_lock_fd = NGX_INVALID_FILE;
#endif

    return mcf;
}


static void *
ngx_http_haskell_create_loc_conf(ngx_conf_t *cf)
{
    ngx_http_haskell_loc_conf_t  *lcf;

    lcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_haskell_loc_conf_t));
    if (lcf == NULL) {
        return NULL;
    }

    if (ngx_array_init(&lcf->code_vars, cf->pool, 1,
                       sizeof(ngx_http_haskell_code_var_data_t)) != NGX_OK)
    {
        return NULL;
    }

    lcf->request_body_read_temp_file = NGX_CONF_UNSET;

    return lcf;
}


static char *
ngx_http_haskell_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child)
{
    ngx_http_haskell_loc_conf_t  *prev = parent;
    ngx_http_haskell_loc_conf_t  *conf = child;

    ngx_uint_t                    i;

    for (i = 0; i < prev->code_vars.nelts; i++) {
        ngx_http_haskell_code_var_data_t  *elem;

        elem = ngx_array_push(&conf->code_vars);
        if (elem == NULL) {
            return NGX_CONF_ERROR;
        }

        *elem = ((ngx_http_haskell_code_var_data_t *) prev->code_vars.elts)[i];
    }

    ngx_conf_merge_value(conf->request_body_read_temp_file,
                         prev->request_body_read_temp_file, 0);

    return NGX_CONF_OK;
}


static ngx_int_t
ngx_http_haskell_init_worker(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i, j;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_core_main_conf_t                 *cmcf;
    ngx_http_variable_t                       *cmvars;
    ngx_http_haskell_var_handle_t             *vars;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_file_t                                 out;
    ngx_int_t                                  index;
    ngx_uint_t                                 found;

    if (ngx_process == NGX_PROCESS_HELPER) {
        return NGX_OK;
    }

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    cmcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_core_module);
    cmvars = cmcf->variables.elts;

    vars = mcf->var_nocacheable.elts;
    for (i = 0; i < mcf->var_nocacheable.nelts; i++) {
        found = 0;
        for (j = 0; j < cmcf->variables.nelts; j++) {
            if (vars[i].name.len == cmvars[j].name.len
                && ngx_strncmp(vars[i].name.data, cmvars[j].name.data,
                               vars[i].name.len) == 0)
            {
                vars[i].index = cmvars[j].index;
                /* variables with any get handler are allowed here! */
                cmvars[j].flags |= NGX_HTTP_VAR_NOCACHEABLE;
                found = 1;
                break;
            }
        }
        if (found == 0) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" was not declared", &vars[i]);
        }
    }

    ngx_http_haskell_var_init(cycle->log, &cmcf->variables,
                              &mcf->var_compensate_uri_changes,
                              ngx_http_haskell_run_handler);
    ngx_http_haskell_var_init(cycle->log, &cmcf->variables,
                              &mcf->service_var_ignore_empty,
                              ngx_http_haskell_run_service_handler);
    ngx_http_haskell_var_init(cycle->log, &cmcf->variables,
                              &mcf->service_var_in_shm,
                              ngx_http_haskell_run_service_handler);

#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
    found = 0;
#endif
    service_code_vars = mcf->service_code_vars.elts;
    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        index = service_code_vars[i].data->index;

        vars = mcf->service_var_ignore_empty.elts;
        for (j = 0; j < mcf->service_var_ignore_empty.nelts; j++) {
            if (index == vars[j].index) {
                service_code_vars[i].ignore_empty = 1;
                break;
            }
        }

        vars = mcf->service_var_in_shm.elts;
        for (j = 0; j < mcf->service_var_in_shm.nelts; j++) {
            if (index == vars[j].index) {
                service_code_vars[i].shm_index = j;

                if (service_code_vars[i].cb) {
                    continue;
                }

                if (ngx_http_haskell_shm_lock_init(cycle, &out,
                                                   mcf->shm_lock_files_path,
                                                   mcf->shm_zone->shm.name,
                                                   &cmvars[index].name,
                                                   NGX_FILE_WRONLY)
                    != NGX_OK)
                {
                    return NGX_ERROR;
                }

                ((ngx_http_haskell_shm_var_handle_data_t *)
                                                vars[j].data)->index = index;

                service_code_vars[i].shm_lock_fd = out.fd;
#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
                found = 1;
#endif

                break;
            }
        }

        if (!service_code_vars[i].cb) {
            continue;
        }

        if (cmvars[index].get_handler != ngx_http_haskell_run_service_handler) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" has incompatible get handler",
                          &cmvars[index].name);
            service_code_vars[i].data->index = NGX_ERROR;
            continue;
        }

        if (service_code_vars[i].shm_index == NGX_ERROR) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "variable \"%V\" is not in shm", &cmvars[index].name);
            service_code_vars[i].data->index = NGX_ERROR;
        }
    }

#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
    if (found && mcf->shm_lock_fd == NGX_INVALID_FILE) {
        if (ngx_http_haskell_shm_lock_init(cycle, &out,
                                           mcf->shm_lock_files_path,
                                           mcf->shm_zone->shm.name,
                                           NULL, NGX_FILE_RDWR)
            != NGX_OK)
        {
            return NGX_ERROR;
        }

        mcf->shm_lock_fd = out.fd;
    }
#endif

    if (ngx_http_haskell_load(cycle) != NGX_OK) {
        return NGX_ERROR;
    }

    return ngx_http_haskell_init_services(cycle);
}


static void
ngx_http_haskell_exit_worker(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);

    if (ngx_process == NGX_PROCESS_HELPER || mcf == NULL) {
        return;
    }

    service_code_vars = mcf->service_code_vars.elts;
    if (mcf->code_loaded && mcf->dl_handle != NULL) {
        for (i = 0; i < mcf->service_code_vars.nelts; i++) {
            if (service_code_vars[i].has_locked_async_task) {
                mcf->terminate_async_task(
                                    service_code_vars[i].locked_async_task);
            }
        }
    }

    ngx_http_haskell_unload(cycle, 1);

    ngx_http_haskell_stop_services(cycle);

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (service_code_vars[i].shm_lock_fd != NGX_INVALID_FILE
            && ngx_close_file(service_code_vars[i].shm_lock_fd)
            == NGX_FILE_ERROR)
        {
            ngx_log_error(NGX_LOG_ALERT, cycle->log, ngx_errno,
                          "failed to close file lock handle for variable "
                          "in shared memory");
        }
    }

#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
    if (mcf->shm_lock_fd != NGX_INVALID_FILE
        && ngx_close_file(mcf->shm_lock_fd) == NGX_FILE_ERROR)
    {
        ngx_log_error(NGX_LOG_ALERT, cycle->log, ngx_errno,
                      "failed to close file lock handle for shared memory");

    }
#endif
}


static void
ngx_http_haskell_var_init(ngx_log_t *log, ngx_array_t *cmvar, ngx_array_t *var,
                          ngx_http_get_variable_pt get_handler)
{
    ngx_uint_t                                 i, j;
    ngx_http_haskell_var_handle_t             *vars;
    ngx_http_variable_t                       *cmvars;
    ngx_uint_t                                 found;

    cmvars = cmvar->elts;

    vars = var->elts;
    for (i = 0; i < var->nelts; i++) {
        found = 0;
        for (j = 0; j < cmvar->nelts; j++) {
            if (vars[i].name.len == cmvars[j].name.len
                && ngx_strncmp(vars[i].name.data, cmvars[j].name.data,
                               vars[i].name.len) == 0)
            {
                if (cmvars[j].get_handler != get_handler) {
                    ngx_log_error(NGX_LOG_ERR, log, 0,
                                  "variable \"%V\" has incompatible "
                                  "get handler", &vars[i].name);
                } else {
                    vars[i].index = cmvars[j].index;
                }
                found = 1;
                break;
            }
        }
        if (found == 0) {
            ngx_log_error(NGX_LOG_ERR, log, 0,
                          "variable \"%V\" was not declared", &vars[i].name);
        }
    }
}


static ngx_int_t
ngx_http_haskell_shm_lock_init(ngx_cycle_t *cycle, ngx_file_t *out,
                               ngx_str_t path, ngx_str_t zone_name,
                               ngx_str_t *var_name, int mode)
{
    ngx_int_t  len = var_name == NULL ? 0 : var_name->len;

    ngx_memzero(out, sizeof(ngx_file_t));
    out->name.len = path.len +
            haskell_shm_file_lock_prefix.len + zone_name.len + 1 + len +
            haskell_shm_file_lock_suffix.len;

    out->name.data = ngx_pnalloc(cycle->pool, out->name.len + 1);
    if (out->name.data == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to allocate memory for file lock for access "
                      "to variables in shared memory");
        return NGX_ERROR;
    }

    ngx_memcpy(out->name.data, path.data, path.len);
    ngx_memcpy(out->name.data + path.len,
               haskell_shm_file_lock_prefix.data,
               haskell_shm_file_lock_prefix.len);
    ngx_memcpy(out->name.data + path.len + haskell_shm_file_lock_prefix.len,
               zone_name.data, zone_name.len);
    ngx_memcpy(out->name.data + path.len + haskell_shm_file_lock_prefix.len +
                    zone_name.len,
               "_", 1);
    if (len > 0) {
        ngx_memcpy(out->name.data + path.len +
                        haskell_shm_file_lock_prefix.len + zone_name.len + 1,
                   var_name->data, len);
    }
    ngx_memcpy(out->name.data + path.len +
                    haskell_shm_file_lock_prefix.len + zone_name.len + 1 + len,
               haskell_shm_file_lock_suffix.data,
               haskell_shm_file_lock_suffix.len);
    out->name.data[out->name.len] = '\0';

    out->fd = ngx_open_file(out->name.data, mode,
                            NGX_FILE_TRUNCATE|O_EXCL,
                            NGX_FILE_OWNER_ACCESS);
    if (out->fd == NGX_INVALID_FILE && ngx_errno == NGX_EEXIST_FILE) {
        out->fd = ngx_open_file(out->name.data, mode,
                                NGX_FILE_TRUNCATE,
                                NGX_FILE_OWNER_ACCESS);
    }
    if (out->fd == NGX_INVALID_FILE) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, ngx_errno,
                      "failed to open file lock \"%V\" for access "
                      "to variables in shared memory", &out->name);
        return NGX_ERROR;
    }

    return NGX_OK;
}


static char *
ngx_http_haskell(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_main_conf_t   *mcf = conf;

    ngx_int_t                       i;
    ngx_str_t                      *value, base_name;
    ngx_file_info_t                 lib_info;
    ngx_int_t                       idx;
    ngx_uint_t                      load = 0, load_existing = 0;
    ngx_uint_t                      base_name_start = 0;
    ngx_uint_t                      has_wrap_mode = 0, has_threaded = 0;
    char                          **options;
    ngx_int_t                       len;

    value = cf->args->elts;

    if (value[1].len == 7
        && ngx_strncmp(value[1].data, "compile", 7) == 0)
    {
        if (cf->args->nelts < 4) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell compile requires 2 parameters");
            return NGX_CONF_ERROR;
        }

    } else if (value[1].len == 4
               && ngx_strncmp(value[1].data, "load", 4) == 0)
    {
        load = 1;

    } else if (value[1].len == 17
               && ngx_strncmp(value[1].data, "ghc_extra_options", 17) == 0)
    {
        if (mcf->code_loaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell ghc_extra_options must precede "
                    "directives haskell compile / load");
            return NGX_CONF_ERROR;
        }
        if (mcf->ghc_extra_options.len > 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell ghc_extra_options was already set");
            return NGX_CONF_ERROR;
        }
        len = cf->args->nelts - 2;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            len += value[i].len;
        }
        mcf->ghc_extra_options.len = len;
        mcf->ghc_extra_options.data = ngx_pnalloc(cf->pool, len);
        if (mcf->ghc_extra_options.data == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to allocate memory for ghc extra options");
            return NGX_CONF_ERROR;
        }
        len = 0;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            ngx_memcpy(mcf->ghc_extra_options.data + len++, " ", 1);
            ngx_memcpy(mcf->ghc_extra_options.data + len, value[i].data,
                       value[i].len);
            len += value[i].len;
        }

        return NGX_CONF_OK;

    } else if (value[1].len == 11
               && ngx_strncmp(value[1].data, "rts_options", 11) == 0)
    {
        if (mcf->rts_options.nelts > 1) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell rts_options was already set");
            return NGX_CONF_ERROR;
        }
        if (ngx_array_init(&mcf->rts_options, cf->pool, cf->args->nelts - 2,
                           sizeof(char *)) != NGX_OK
            || ngx_array_push_n(&mcf->rts_options, cf->args->nelts - 2)
            == NULL)
        {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to allocate memory for ghc RTS options");
            return NGX_CONF_ERROR;
        }
        options = mcf->rts_options.elts;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            options[i - 2] = (char *) value[i].data;
        }

        return NGX_CONF_OK;

    } else if (value[1].len == 15
               && ngx_strncmp(value[1].data, "program_options", 15) == 0)
    {
        if (mcf->program_options.nelts > 1) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directive haskell program_options was already set");
            return NGX_CONF_ERROR;
        }
        if (ngx_array_init(&mcf->program_options, cf->pool, cf->args->nelts - 2,
                           sizeof(char *)) != NGX_OK
            || ngx_array_push_n(&mcf->program_options, cf->args->nelts - 2)
            == NULL)
        {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "failed to allocate memory for program options");
            return NGX_CONF_ERROR;
        }
        options = mcf->program_options.elts;
        for (i = 2; (ngx_uint_t) i < cf->args->nelts; i++) {
            options[i - 2] = (char *) value[i].data;
        }

        return NGX_CONF_OK;

    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "unknown haskell directive \"%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }

    if (mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "only one haskell source code block is allowed");
        return NGX_CONF_ERROR;
    }

    if (cf->args->nelts > 6) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                    "directives haskell compile / load do not accept "
                    "more than 5 parameters");
        return NGX_CONF_ERROR;
    }

    if (value[2].len == 8 && ngx_strncmp(value[2].data, "threaded", 8) == 0) {
        has_threaded = 1;
    }

    idx = 2 + has_threaded;

    if (value[idx].len == 10
        && ngx_strncmp(value[idx].data, "standalone", 10) == 0)
    {
        has_wrap_mode = 1;
        mcf->wrap_mode = ngx_http_haskell_module_wrap_mode_standalone;
    } else if (value[idx].len == 7
               && ngx_strncmp(value[idx].data, "modular", 10) == 0)
    {
        has_wrap_mode = 1;
    }

    if (has_wrap_mode) {
        if (cf->args->nelts < 4 + has_threaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "directives haskell compile / load require "
                        "at least 2 parameters when wrap mode is specified");
            return NGX_CONF_ERROR;
        }
        if (!load && cf->args->nelts < 5 + has_threaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "directive haskell compile requires 3 parameters "
                        "when wrap mode is specified");
            return NGX_CONF_ERROR;
        }
    }

    if (load) {
        load_existing =
                cf->args->nelts < 4 + has_threaded + has_wrap_mode ? 1 : 0;
    }
    idx += has_wrap_mode;

    if (value[idx].len < 3
        || !(ngx_strncmp(value[idx].data + value[idx].len - 3, ".hs", 3) == 0
             || (load_existing
                 && ngx_strncmp(value[idx].data + value[idx].len - 3, ".so", 3)
                    == 0)))
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file must have extension \".hs\"");
        return NGX_CONF_ERROR;
    }
    if (!ngx_path_separator(value[idx].data[0])) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file path must be absolute");
        return NGX_CONF_ERROR;
    }
    for (i = value[idx].len - 4; i >= 0; i--) {
        if (ngx_path_separator(value[idx].data[i])) {
            base_name_start = i;
            break;
        }
    }
    base_name.len = value[idx].len - 4 - base_name_start;
    if (base_name.len == 0) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                        "haskell source code file base name is empty");
        return NGX_CONF_ERROR;
    }
    base_name.data = value[idx].data + base_name_start;

    mcf->lib_path.len = value[idx].len;
    mcf->lib_path.data = ngx_pnalloc(cf->pool, mcf->lib_path.len + 1);
    if (mcf->lib_path.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(mcf->lib_path.data, value[idx].data, value[idx].len - 3);
    ngx_memcpy(mcf->lib_path.data + value[idx].len - 3, ".so", 4);

    if (load) {
        if (ngx_file_info(mcf->lib_path.data, &lib_info) == NGX_FILE_ERROR) {
            if (load_existing) {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                        "haskell library cannot be loaded nor compiled");
                return NGX_CONF_ERROR;
            }
            load = 0;
        } else {
            if (has_threaded) {
                ngx_conf_log_error(NGX_LOG_NOTICE, cf, 0,
                        "haskell library exists but is asked to be compiled "
                        "as threaded, please make sure that it was indeed "
                        "compiled as threaded, otherwise async tasks may "
                        "stall in runtime");
                mcf->compile_mode = ngx_http_haskell_compile_mode_load_existing;
            }
        }
    }

    if (!load) {
        if (ngx_http_haskell_write_code(cf, conf, value[idx], value[idx + 1])
            != NGX_CONF_OK)
        {
            return NGX_CONF_ERROR;
        }

        mcf->compile_mode = has_threaded ?
                ngx_http_haskell_compile_mode_threaded :
                ngx_http_haskell_compile_mode_no_threaded;
        if (ngx_http_haskell_compile(cf, conf, value[idx]) != NGX_CONF_OK) {
            return NGX_CONF_ERROR;
        }

        if (ngx_file_info(mcf->lib_path.data, &lib_info) == NGX_FILE_ERROR) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                               "haskell library cannot be loaded");
            return NGX_CONF_ERROR;
        }

        lib_info.st_mode |= S_IRGRP|S_IXGRP|S_IROTH|S_IXOTH;
        if (chmod((const char *) mcf->lib_path.data, lib_info.st_mode) == -1) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                               "chmod() \"%V\" failed", &mcf->lib_path);
        }
    }

    mcf->code_loaded = 1;

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_run(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_loc_conf_t               *lcf = conf;

    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_str_t                                 *value;
    ngx_uint_t                                 n_args, n_size;
    ngx_str_t                                  handler_name;
    ngx_http_haskell_handler_t                *handlers;
    ngx_http_compile_complex_value_t           ccv;
    ngx_http_complex_value_t                  *args;
    ngx_http_variable_t                       *v;
    ngx_http_haskell_code_var_data_t          *code_var_data;
    ngx_http_haskell_service_code_var_data_t  *service_code_var_data;
    ngx_int_t                                  v_idx;
    ngx_uint_t                                *v_idx_ptr;
    ngx_http_get_variable_pt                   get_handler;
    ngx_uint_t                                 async, rb, service, service_cb;

    value = cf->args->elts;

    service_cb = value[0].len == 35
            && ngx_strncmp(value[0].data,
                           "haskell_service_var_update_callback", 35) == 0;
    service = service_cb
            || (value[0].len == 19
                && ngx_strncmp(value[0].data, "haskell_run_service", 19) == 0);

    mcf = service ? conf :
            ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    if (!mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "haskell code was not loaded");
        return NGX_CONF_ERROR;
    }

    if ((!service && cf->args->nelts < 4) || cf->args->nelts < 3) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "too few arguments");
        return NGX_CONF_ERROR;
    }
    n_args = cf->args->nelts - 3;
    n_size = n_args == 0 ? 1 : n_args;

    if (value[2].len < 2 || value[2].data[0] != '$') {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid variable name \"%V\"", &value[2]);
        return NGX_CONF_ERROR;
    }
    value[2].len--;
    value[2].data++;

    handler_name.len = value[1].len +
            ngx_http_haskell_module_handler_prefix.len;
    handler_name.data = ngx_pnalloc(cf->pool, handler_name.len + 1);
    if (handler_name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(handler_name.data,
               ngx_http_haskell_module_handler_prefix.data,
               ngx_http_haskell_module_handler_prefix.len);
    ngx_memcpy(handler_name.data + ngx_http_haskell_module_handler_prefix.len,
               value[1].data, value[1].len);
    handler_name.data[handler_name.len] ='\0';

    if (service) {
        service_code_var_data = ngx_array_push(&mcf->service_code_vars);
        if (service_code_var_data == NULL) {
            return NGX_CONF_ERROR;
        }
        code_var_data = ngx_palloc(cf->pool,
                                   sizeof(ngx_http_haskell_code_var_data_t));
        if (code_var_data == NULL) {
            return NGX_CONF_ERROR;
        }
        ngx_memzero(service_code_var_data,
                    sizeof(ngx_http_haskell_service_code_var_data_t));
        service_code_var_data->data = code_var_data;
        service_code_var_data->shm_index = NGX_ERROR;
        service_code_var_data->shm_lock_fd = NGX_INVALID_FILE;
        service_code_var_data->cb = service_cb ? 1 : 0;
        service_code_var_data->noarg = n_args > 0 ? 0 : 1;
    } else {
        code_var_data = ngx_array_push(&lcf->code_vars);
        if (code_var_data == NULL) {
            return NGX_CONF_ERROR;
        }
    }
    if (ngx_array_init(&code_var_data->args, cf->pool, n_size,
                       sizeof(ngx_http_complex_value_t)) != NGX_OK)
    {
        return NGX_CONF_ERROR;
    }

    code_var_data->handler = NGX_ERROR;

    rb = ngx_strncmp(value[0].data,
                     "haskell_run_async_on_request_body", 33) == 0;
    async = rb ? 1 : ngx_strncmp(value[0].data, "haskell_run_async", 17) == 0;
    mcf->has_async_tasks = mcf->has_async_tasks ? 1 : async;

    async = async ? 1 : service;
    if (async) {
        if (mcf->compile_mode == ngx_http_haskell_compile_mode_no_threaded) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "haskell module was compiled without thread "
                               "support, using async tasks will inevitably "
                               "cause stalls of requests in runtime");
            return NGX_CONF_ERROR;
        }
        if (mcf->compile_mode == ngx_http_haskell_compile_mode_no_threaded) {
            ngx_conf_log_error(NGX_LOG_NOTICE, cf, 0,
                               "haskell module was loaded from existing "
                               "library, please make sure that it was compiled "
                               "as threaded, otherwise async tasks may stall "
                               "in runtime");
        }
    }

    handlers = mcf->handlers.elts;
    for (i = 0; i < mcf->handlers.nelts; i++) {
        if (handler_name.len == handlers[i].name.len
            && ngx_strncmp(handler_name.data, handlers[i].name.data,
                           handler_name.len) == 0)
        {
            if (handlers[i].role
                == ngx_http_haskell_handler_role_content_handler)
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared as content handler", &value[1]);
                return NGX_CONF_ERROR;
            }
            if ((handlers[i].role
                 == ngx_http_haskell_handler_role_variable && async)
                || ((handlers[i].role
                     == ngx_http_haskell_handler_role_async_variable
                     || handlers[i].role
                     == ngx_http_haskell_handler_role_async_variable_rb
                     || handlers[i].role
                     == ngx_http_haskell_handler_role_service_variable)
                    && !async))
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" async attribute "
                                   "mismatch", &value[1]);
                return NGX_CONF_ERROR;
            }
            code_var_data->handler = i;
            break;
        }
    }
    if (code_var_data->handler == NGX_ERROR) {
        ngx_http_haskell_handler_t  *handler;

        handler = ngx_array_push(&mcf->handlers);
        if (handler == NULL) {
            return NGX_CONF_ERROR;
        }

        handler->self = NULL;
        handler->type = ngx_http_haskell_handler_type_uninitialized;
        handler->name = handler_name;
        ngx_memzero(&handler->n_args, sizeof(handler->n_args));
        handler->role = service ?
                ngx_http_haskell_handler_role_service_variable :
                (rb ? ngx_http_haskell_handler_role_async_variable_rb :
                 (async ? ngx_http_haskell_handler_role_async_variable :
                  ngx_http_haskell_handler_role_variable));

        handlers = mcf->handlers.elts;
        code_var_data->handler = mcf->handlers.nelts - 1;
    }

    ++handlers[code_var_data->handler].n_args[n_args > 2 ? 2 : n_size - 1];

    if (service_cb) {
        v_idx = ngx_http_get_variable_index(cf, &value[2]);
        if (v_idx == NGX_ERROR) {
            return NGX_CONF_ERROR;
        }
    } else {
        v = ngx_http_add_variable(cf, &value[2], NGX_HTTP_VAR_CHANGEABLE);
        if (v == NULL) {
            return NGX_CONF_ERROR;
        }
        v_idx = ngx_http_get_variable_index(cf, &value[2]);
        if (v_idx == NGX_ERROR) {
            return NGX_CONF_ERROR;
        }
        v_idx_ptr = ngx_palloc(cf->pool, sizeof(ngx_uint_t));
        if (v_idx_ptr == NULL) {
            return NGX_CONF_ERROR;
        }
        *v_idx_ptr = v_idx;

        v->data = (uintptr_t) v_idx_ptr;

        get_handler = v->get_handler;
        v->get_handler = service ? ngx_http_haskell_run_service_handler :
                (async ? ngx_http_haskell_run_async_handler :
                 ngx_http_haskell_run_handler);

        if (get_handler != NULL && get_handler != v->get_handler) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "variable \"%V\" has been already defined with "
                               "another variable handler", &value[2]);
            return NGX_CONF_ERROR;
        }
    }

    code_var_data->index = v_idx;

    if (ngx_array_push_n(&code_var_data->args, n_size) == NULL) {
        return NGX_CONF_ERROR;
    }
    args = code_var_data->args.elts;

    if (service) {
        ngx_str_null(&args[0].value);
        for (i = 0; i < n_args; i++) {
            args[i].value = value[3 + i];
        }
    } else {
        ngx_memzero(&ccv, sizeof(ngx_http_compile_complex_value_t));
        ccv.cf = cf;

        for (i = 0; i < n_args; i++) {
            ccv.value = &value[3 + i];
            ccv.complex_value = &args[i];

            if (ngx_http_compile_complex_value(&ccv) != NGX_OK) {
                return NGX_CONF_ERROR;
            }
        }
    }

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_content(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_loc_conf_t       *lcf = conf;

    ngx_uint_t                         i;
    ngx_http_haskell_main_conf_t      *mcf;
    ngx_str_t                         *value;
    ngx_str_t                          handler_name;
    ngx_http_haskell_handler_t        *handlers;
    ngx_http_core_loc_conf_t          *clcf;
    ngx_uint_t                         unsafe = 0, async = 0, async_rb = 0;

    mcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    if (!mcf->code_loaded) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "haskell code was not loaded");
        return NGX_CONF_ERROR;
    }

    if (lcf->content_handler != NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "haskell content handler was already set");
        return NGX_CONF_ERROR;
    }

    value = cf->args->elts;

    if (value[0].len == 22
        && ngx_strncmp(value[0].data, "haskell_static_content", 22) == 0)
    {
        lcf->static_content = 1;
        lcf->pool = cf->pool;
    } else if (value[0].len == 22
        && ngx_strncmp(value[0].data, "haskell_unsafe_content", 22) == 0)
    {
        unsafe = 1;
    } else if (value[0].len == 21
        && ngx_strncmp(value[0].data, "haskell_async_content", 21) == 0)
    {
        async = 1;
    } else if (value[0].len == 37
        && ngx_strncmp(value[0].data,
                       "haskell_async_content_on_request_body", 37) == 0)
    {
        async = 1;
        async_rb = 1;
    }

    mcf->has_async_handlers = mcf->has_async_handlers ? 1 : async;

    handler_name.len = value[1].len +
            ngx_http_haskell_module_handler_prefix.len;
    handler_name.data = ngx_pnalloc(cf->pool, handler_name.len + 1);
    if (handler_name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(handler_name.data,
               ngx_http_haskell_module_handler_prefix.data,
               ngx_http_haskell_module_handler_prefix.len);
    ngx_memcpy(handler_name.data + ngx_http_haskell_module_handler_prefix.len,
               value[1].data, value[1].len);
    handler_name.data[handler_name.len] ='\0';

    lcf->content_handler = ngx_pcalloc(cf->pool,
                                    sizeof(ngx_http_haskell_content_handler_t));

    if (lcf->content_handler == NULL) {
        return NGX_CONF_ERROR;
    }

    lcf->content_handler->handler = NGX_ERROR;
    handlers = mcf->handlers.elts;

    for (i = 0; i < mcf->handlers.nelts; i++) {
        if (handler_name.len == handlers[i].name.len
            && ngx_strncmp(handler_name.data, handlers[i].name.data,
                           handler_name.len) == 0)
        {
            if (handlers[i].role == ngx_http_haskell_handler_role_variable
                || handlers[i].role
                == ngx_http_haskell_handler_role_async_variable
                || handlers[i].role
                == ngx_http_haskell_handler_role_async_variable_rb
                || handlers[i].role
                == ngx_http_haskell_handler_role_service_variable)
            {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared as a variable handler", &value[1]);
                return NGX_CONF_ERROR;
            }
            if (handlers[i].unsafe != unsafe) {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared with a different safety attribute",
                                   &value[1]);
                return NGX_CONF_ERROR;
            }
            if (handlers[i].async != async) {
                ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                                   "haskell handler \"%V\" was already "
                                   "declared with a different async attribute",
                                   &value[1]);
                return NGX_CONF_ERROR;
            }
            lcf->content_handler->handler = i;
            break;
        }
    }

    if (lcf->content_handler->handler == NGX_ERROR) {
        ngx_http_haskell_handler_t  *handler;

        handler = ngx_array_push(&mcf->handlers);
        if (handler == NULL) {
            return NGX_CONF_ERROR;
        }

        handler->self = NULL;
        handler->type = ngx_http_haskell_handler_type_uninitialized;
        handler->name = handler_name;
        ngx_memzero(&handler->n_args, sizeof(handler->n_args));
        handler->role = async ?
                (async_rb ?
                    ngx_http_haskell_handler_role_async_content_handler_rb :
                    ngx_http_haskell_handler_role_async_content_handler) :
                ngx_http_haskell_handler_role_content_handler;
        handler->unsafe = unsafe;
        handler->async = async;

        handlers = mcf->handlers.elts;
        lcf->content_handler->handler = mcf->handlers.nelts - 1;
    }

    ++handlers[lcf->content_handler->handler].n_args[0];

    if (cf->args->nelts == 3) {
        ngx_http_compile_complex_value_t   ccv;

        lcf->content_handler->args = ngx_pcalloc(cf->pool,
                                            sizeof(ngx_http_complex_value_t));

        if (lcf->content_handler->args == NULL) {
            return NGX_CONF_ERROR;
        }

        ngx_memzero(&ccv, sizeof(ngx_http_compile_complex_value_t));
        ccv.cf = cf;

        ccv.value = &value[2];
        ccv.complex_value = lcf->content_handler->args;

        if (ngx_http_compile_complex_value(&ccv) != NGX_OK) {
            return NGX_CONF_ERROR;
        }
    }

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_haskell_content_handler;

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_var_configure(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_haskell_main_conf_t            *mcf = conf;

    ngx_uint_t                               i, j;
    ngx_str_t                               *value;
    ngx_array_t                             *data;
    ngx_http_haskell_var_handle_t           *vars;
    ngx_uint_t                               n_vars;
    ngx_http_variable_t                     *v;
    ngx_http_haskell_shm_var_handle_data_t  *v_data;
    ngx_str_t                                v_name;
    ngx_http_get_variable_pt                 get_handler;
    ngx_int_t                                idx = 0;
    ngx_uint_t                               create_shm_aux_vars = 0;

    value = cf->args->elts;

    if (value[0].len == 23
        && ngx_strncmp(value[0].data, "haskell_var_nocacheable", 23)
        == 0)
    {
        data = &mcf->var_nocacheable;
    } else if (value[0].len == 34
        && ngx_strncmp(value[0].data, "haskell_var_compensate_uri_changes", 34)
        == 0)
    {
        data = &mcf->var_compensate_uri_changes;
    } else if (value[0].len == 32
        && ngx_strncmp(value[0].data, "haskell_service_var_ignore_empty", 32)
        == 0)
    {
        data = &mcf->service_var_ignore_empty;
    } else if (value[0].len == 26
        && ngx_strncmp(value[0].data, "haskell_service_var_in_shm", 26)
        == 0)
    {
        data = &mcf->service_var_in_shm;
        create_shm_aux_vars = 1;
        idx = 3;
    } else {
        return NGX_CONF_ERROR;
    }

    if (data->nalloc > 0) {
        return "is duplicate";
    }

    n_vars = cf->args->nelts - idx - 1;

    if (ngx_array_init(data, cf->pool, n_vars,
                       sizeof(ngx_http_haskell_var_handle_t)) != NGX_OK
        || ngx_array_push_n(data, n_vars) == NULL)
    {
        return NGX_CONF_ERROR;
    }

    vars = data->elts;

    for (i = 0, j = idx + 1; i < n_vars; i++, j++) {
        if (value[j].len < 2 || value[j].data[0] != '$') {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "invalid variable name \"%V\"", &value[j]);
            return NGX_CONF_ERROR;
        }

        value[j].len--;
        value[j].data++;
        ngx_strlow(value[j].data, value[j].data, value[j].len);
        vars[i].name = value[j];
        vars[i].index = -1;

        if (!create_shm_aux_vars) {
            continue;
        }

        v_name.len = value[j].len + haskell_module_shm_update_var_prefix.len;
        v_name.data = ngx_pnalloc(cf->pool, v_name.len);
        if (v_name.data == NULL) {
            return NGX_CONF_ERROR;
        }

        ngx_memcpy(v_name.data,
                   haskell_module_shm_update_var_prefix.data,
                   haskell_module_shm_update_var_prefix.len);
        ngx_memcpy(v_name.data + haskell_module_shm_update_var_prefix.len,
                   value[j].data,
                   value[j].len);

        v = ngx_http_add_variable(cf, &v_name, NGX_HTTP_VAR_CHANGEABLE);
        if (v == NULL) {
            return NGX_CONF_ERROR;
        }

        v_data = ngx_pcalloc(cf->pool,
                             sizeof(ngx_http_haskell_shm_var_handle_data_t));
        if (v_data == NULL) {
            return NGX_CONF_ERROR;
        }

        v_data->index = NGX_ERROR;
        v->data = (uintptr_t) v_data;
        vars[i].data = v_data;

        get_handler = v->get_handler;
        v->get_handler = ngx_http_haskell_shm_update_var_handler;
        if (get_handler != NULL && get_handler != v->get_handler) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "variable \"%V\" has been already defined with "
                               "another variable handler", &v_name);
            return NGX_CONF_ERROR;
        }

        v_name.len = value[j].len + haskell_module_shm_stats_var_prefix.len;
        v_name.data = ngx_pnalloc(cf->pool, v_name.len);
        if (v_name.data == NULL) {
            return NGX_CONF_ERROR;
        }

        ngx_memcpy(v_name.data,
                   haskell_module_shm_stats_var_prefix.data,
                   haskell_module_shm_stats_var_prefix.len);
        ngx_memcpy(v_name.data + haskell_module_shm_stats_var_prefix.len,
                   value[j].data,
                   value[j].len);

        v = ngx_http_add_variable(cf, &v_name, NGX_HTTP_VAR_CHANGEABLE);
        if (v == NULL) {
            return NGX_CONF_ERROR;
        }

        v->data = (uintptr_t) v_data;

        get_handler = v->get_handler;
        v->get_handler = ngx_http_haskell_shm_stats_var_handler;
        if (get_handler != NULL && get_handler != v->get_handler) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "variable \"%V\" has been already defined with "
                               "another variable handler", &v_name);
            return NGX_CONF_ERROR;
        }
    }

    return NGX_CONF_OK;
}


static char *
ngx_http_haskell_service_var_in_shm(ngx_conf_t *cf, ngx_command_t *cmd,
                                    void *conf)
{
    ngx_http_haskell_main_conf_t      *mcf = conf;

    ngx_str_t                         *value;
    ssize_t                            shm_size;

    value = cf->args->elts;

    if (mcf->shm_zone != NULL) {
        return "is duplicate";
    }

    if (cf->args->nelts < 5) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0, "too few arguments");
        return NGX_CONF_ERROR;
    }

    shm_size = ngx_parse_size(&value[2]);

    if (shm_size == NGX_ERROR) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid zone size \"%V\"", &value[2]);
        return NGX_CONF_ERROR;
    }

    if (shm_size < (ssize_t) (8 * ngx_pagesize)) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "zone \"%V\" is too small", &value[1]);
        return NGX_CONF_ERROR;
    }

    mcf->shm_zone = ngx_shared_memory_add(cf, &value[1], shm_size,
                                          &ngx_http_haskell_module);
    if (mcf->shm_zone == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to add memory for zone \"%V\"", &value[1]);
        return NGX_CONF_ERROR;
    }

    mcf->shm_zone->init = ngx_http_haskell_service_var_init_zone;
    mcf->shm_zone->data = &mcf->service_var_in_shm;

    mcf->shm_zone->noreuse = 1;

    mcf->shm_lock_files_path = value[3];

    return ngx_http_haskell_var_configure(cf, cmd, conf);
}
