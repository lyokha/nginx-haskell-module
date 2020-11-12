/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_service.c
 *
 *    Description:  Haskell services
 *
 *        Version:  2.0
 *        Created:  05.02.2018 15:34:44
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_service.h"
#include "ngx_http_haskell_util.h"

#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
#define NGX_HTTP_HASKELL_SHM_WLOCK   ngx_http_haskell_wlock(mcf->shm_lock_fd);
#define NGX_HTTP_HASKELL_SHM_RLOCK   ngx_http_haskell_rlock(mcf->shm_lock_fd);
#define NGX_HTTP_HASKELL_SHM_UNLOCK  ngx_http_haskell_unlock(mcf->shm_lock_fd);
#else
#define NGX_HTTP_HASKELL_SHM_WLOCK   ngx_shmtx_lock(&shpool->mutex);
#define NGX_HTTP_HASKELL_SHM_RLOCK   ngx_shmtx_lock(&shpool->mutex);
#define NGX_HTTP_HASKELL_SHM_UNLOCK  ngx_shmtx_unlock(&shpool->mutex);
#endif


typedef struct {
    time_t                                     modified;
    size_t                                     size;
    ngx_uint_t                                 changes;
    ngx_uint_t                                 failures;
    ngx_uint_t                                 failed;
} ngx_http_haskell_shm_stats_var_t;


typedef struct {
    ngx_http_haskell_var_handle_t              handle;
    ngx_http_haskell_async_data_t              data;
    uint64_t                                   seqn;
    ngx_http_haskell_shm_stats_var_t           stats;
} ngx_http_haskell_shm_var_handle_t;


typedef struct {
    ngx_str_t                                  data;
} ngx_http_haskell_shm_service_hook_handle_t;


static ngx_int_t ngx_http_haskell_run_service(ngx_cycle_t *cycle,
    ngx_http_haskell_service_code_var_data_t *service_code_var,
    ngx_uint_t service_first_run);
static void ngx_http_haskell_service_event(ngx_event_t *ev);
static void ngx_http_haskell_service_hook_event(ngx_event_t *ev);
static void ngx_http_haskell_run_service_hook(ngx_cycle_t *cycle,
    ngx_http_haskell_main_conf_t *mcf, ngx_http_haskell_service_hook_t *hook,
    ngx_str_t *arg);
static void ngx_http_haskell_service_handler_cleanup(void *data);
#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK
static void ngx_http_haskell_wlock(ngx_fd_t fd);
static void ngx_http_haskell_rlock(ngx_fd_t fd);
static void ngx_http_haskell_unlock(ngx_fd_t fd);
static ngx_err_t ngx_http_haskell_rlock_fd(ngx_fd_t fd);
#endif

static ngx_event_t  dummy_write_event;


ngx_int_t
ngx_http_haskell_init_services(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (!service_code_vars[i].cb
            && ngx_http_haskell_run_service(cycle, &service_code_vars[i], 1)
            != NGX_OK)
        {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to start haskell services");
            return NGX_ERROR;
        }
    }

    return NGX_OK;
}


void
ngx_http_haskell_stop_services(ngx_cycle_t *cycle)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return;
    }

    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (!service_code_vars[i].running) {
            continue;
        }
        if (service_code_vars[i].async_data != NULL
            && --service_code_vars[i].async_data->ref_count == 0)
        {
            if (service_code_vars[i].async_data->result.complete == 2) {
                ngx_free(service_code_vars[i].async_data->result.data.data);
            }
            ngx_free(service_code_vars[i].async_data);
            service_code_vars[i].async_data = NULL;
        }
        if (service_code_vars[i].hev.s.fd == NGX_INVALID_FILE) {
            if (service_code_vars[i].event.timer_set) {
                ngx_del_timer(&service_code_vars[i].event);
            }
            continue;
        }
        service_code_vars[i].event.active = 0;
        if (ngx_del_event(&service_code_vars[i].event, NGX_READ_EVENT, 0)
            == NGX_ERROR)
        {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to delete event while stopping service");
        }
        if (close(service_code_vars[i].hev.s.fd) == -1) {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                          "failed to close async event channel "
                          "while stopping service");
        }
    }
}


ngx_int_t
ngx_http_haskell_service_var_init_zone(ngx_shm_zone_t *shm_zone, void *data)
{
    ngx_uint_t                          i;
    ngx_slab_pool_t                    *shpool;
    ngx_array_t                        *vars;
    ngx_http_haskell_var_handle_t      *vars_elts;
    ngx_http_haskell_shm_var_handle_t  *shm_vars;

    if (shm_zone->shm.exists) {
        return NGX_OK;
    }

    shpool = (ngx_slab_pool_t *) shm_zone->shm.addr;
    vars = shm_zone->data;
    vars_elts = vars->elts;

    ngx_shmtx_lock(&shpool->mutex);

    shm_vars = ngx_slab_alloc_locked(shpool,
                    sizeof(ngx_http_haskell_shm_var_handle_t) * vars->nelts);
    if (shm_vars == NULL) {
        ngx_shmtx_unlock(&shpool->mutex);
        return NGX_ERROR;
    }

    for (i = 0; i < vars->nelts; i++) {
        ngx_memzero(&shm_vars[i], sizeof(ngx_http_haskell_shm_var_handle_t));
        shm_vars[i].handle = vars_elts[i];
        shm_vars[i].data.index = vars_elts[i].index;
    }

    ngx_shmtx_unlock(&shpool->mutex);

    shpool->data = shm_vars;
    shm_zone->data = shm_vars;

    return NGX_OK;
}


ngx_int_t
ngx_http_haskell_service_hooks_init_zone(ngx_shm_zone_t *shm_zone, void *data)
{
    ngx_uint_t                                   i;
    ngx_slab_pool_t                             *shpool;
    ngx_array_t                                 *hooks;
    ngx_http_haskell_shm_service_hook_handle_t  *shm_vars;

    if (shm_zone->shm.exists) {
        return NGX_OK;
    }

    shpool = (ngx_slab_pool_t *) shm_zone->shm.addr;
    hooks = shm_zone->data;

    ngx_shmtx_lock(&shpool->mutex);

    shm_vars = ngx_slab_alloc_locked(shpool,
            sizeof(ngx_http_haskell_shm_service_hook_handle_t) * hooks->nelts);
    if (shm_vars == NULL) {
        ngx_shmtx_unlock(&shpool->mutex);
        return NGX_ERROR;
    }

    for (i = 0; i < hooks->nelts; i++) {
        ngx_memzero(&shm_vars[i],
                    sizeof(ngx_http_haskell_shm_service_hook_handle_t));
    }

    ngx_shmtx_unlock(&shpool->mutex);

    shpool->data = shm_vars;
    shm_zone->data = shm_vars;

    return NGX_OK;
}


ngx_int_t
ngx_http_haskell_init_service_hook(ngx_cycle_t *cycle,
                                   ngx_array_t *service_code_vars,
                                   ngx_http_variable_t *cmvars,
                                   ngx_http_haskell_service_hook_t *hook)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars_elts;
    ngx_event_t                               *event;
    ngx_http_haskell_service_hook_event_t     *hev;

    service_code_vars_elts = service_code_vars->elts;
    for (i = 0; i < service_code_vars->nelts; i++) {
        if (hook->service_code_var_index
            == service_code_vars_elts[i].data->index)
        {
            hook->service_code_var = &service_code_vars_elts[i];
            break;
        }
    }

    if (hook->service_code_var == NULL) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "service hook will not be enabled because "
                      "variable \"%V\" is not a service variable",
                      &cmvars[hook->service_code_var_index].name);
        hook->service_code_var_index = NGX_DECLINED;
        goto close_event_channel;
    }

    if (hook->update_hook) {
        hook->service_code_var->has_update_hooks = 1;
        if (hook->service_code_var->shm_index == NGX_ERROR) {
            goto close_event_channel;
        }
    }

    event = &hook->event;
    hev = &hook->hev;

    ngx_memzero(event, sizeof(ngx_event_t));
    event->data = hev;
    event->handler = ngx_http_haskell_service_hook_event;
    event->log = cycle->log;

    ngx_memzero(hev, sizeof(ngx_http_haskell_service_hook_event_t));
    hev->cycle = cycle;
    hev->hook = hook;

    hev->s.read = event;
    hev->s.write = &dummy_write_event;
    hev->s.fd = hook->event_channel[0];

    if (ngx_add_event(event, NGX_READ_EVENT, NGX_CLEAR_EVENT) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to add event for service hook");
        ngx_http_haskell_close_async_event_channel(cycle->log,
                                                   hook->event_channel);
        return NGX_ERROR;
    }

    return NGX_OK;

close_event_channel:

    ngx_http_haskell_close_async_event_channel(cycle->log, hook->event_channel);

    return NGX_OK;
}


void
ngx_http_haskell_close_service_hook(ngx_cycle_t *cycle,
                                    ngx_http_haskell_service_hook_t *hook)
{
    if (hook->service_code_var_index < 0
        || (hook->update_hook
            && hook->service_code_var->shm_index == NGX_ERROR))
    {
        return;
    }

    hook->service_code_var_index = NGX_AGAIN;

    if (ngx_del_event(&hook->event, NGX_READ_EVENT, 0) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to delete service hook event data");
    }

    ngx_http_haskell_close_async_event_channel(cycle->log,
                                               hook->event_channel);
}


static ngx_int_t
ngx_http_haskell_run_service(ngx_cycle_t *cycle,
                    ngx_http_haskell_service_code_var_data_t *service_code_var,
                    ngx_uint_t service_first_run)
{
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_handler_t                *handlers;
    ngx_event_t                               *event;
    ngx_http_haskell_service_async_event_t    *hev;
    ngx_http_complex_value_t                  *args;
    ngx_str_t                                  arg1;
    ngx_fd_t                                   fd[2];

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);

    /* make scan-build happy */
    if (mcf == NULL) {
        return NGX_ERROR;
    }

    if (ngx_terminate || ngx_exiting) {
        return NGX_OK;
    }

    service_code_var->running = 1;

    event = &service_code_var->event;
    hev = &service_code_var->hev;

    ngx_memzero(event, sizeof(ngx_event_t));
    event->data = hev;
    event->handler = ngx_http_haskell_service_event;
    event->log = cycle->log;

    ngx_memzero(hev, sizeof(ngx_http_haskell_service_async_event_t));
    hev->s.fd = NGX_INVALID_FILE;
    hev->cycle = cycle;
    hev->service_code_var = service_code_var;
    hev->first_run = service_first_run;

    hev->s.read = event;
    hev->s.write = &dummy_write_event;

    if (ngx_http_haskell_open_async_event_channel(fd) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, ngx_errno,
                      "failed to create async event channel for future "
                      "async result, postponing IO task for 0.5 sec");
        ngx_add_timer(event, 500);
        return NGX_OK;
    }

    hev->s.fd = fd[0];

    if (ngx_add_event(event, NGX_READ_EVENT, 0) != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to add event for future async result, "
                      "postponing IO task for 2 sec");
        ngx_http_haskell_close_async_event_channel(cycle->log, fd);
        hev->s.fd = NGX_INVALID_FILE;
        ngx_add_timer(event, 2000);
        return NGX_OK;
    }

    handlers = mcf->handlers.elts;

    service_code_var->future_async_data.yy_cleanup_data.bufs =
            &service_code_var->future_async_data.result.data;
    args = service_code_var->data->args.elts;
    arg1 = args[0].value;
    service_code_var->locked_async_task =
        ((ngx_http_haskell_handler_async_ioy_y_service)
         handlers[service_code_var->data->handler].self)
            (arg1.data, arg1.len, fd[1], service_code_var->shm_lock_fd,
             &service_code_var->active,
             ngx_http_haskell_module_use_eventfd_channel, service_first_run,
             &service_code_var->future_async_data.yy_cleanup_data.bufs,
             &service_code_var->future_async_data.yy_cleanup_data.n_bufs,
             &service_code_var->future_async_data.error,
             &service_code_var->
                        future_async_data.yy_cleanup_data.locked_bytestring);
    service_code_var->has_locked_async_task = 1;
    service_code_var->future_async_data.yy_cleanup_data.hs_free_stable_ptr =
                                                        mcf->hs_free_stable_ptr;

    return NGX_OK;
}


static void
ngx_http_haskell_service_event(ngx_event_t *ev)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_service_async_event_t    *hev = ev->data;
    ngx_cycle_t                               *cycle = hev->cycle;

    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_core_main_conf_t                 *cmcf;
    ngx_http_variable_t                       *cmvars;
    ngx_http_haskell_service_code_var_data_t  *service_code_var;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_http_haskell_service_hook_t           *service_hooks;
    ngx_slab_pool_t                           *shpool;
    ngx_http_haskell_shm_var_handle_t         *shm_vars;
    ngx_int_t                                  shm_index = NGX_ERROR;
    ngx_str_t                                 *var;
    u_char                                    *var_data;
    ngx_http_haskell_async_data_t             *async_data;
    ngx_http_complex_value_t                  *args;
    ngx_str_t                                  arg = ngx_null_string;
    time_t                                     modified;
    ngx_uint_t                                 shm_var_updated = 0;
    ngx_uint_t                                 skip_hooks = 0;
    ngx_uint_t                                 log_level;
    ngx_int_t                                  rc;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);

    /* make scan-build happy */
    if (mcf == NULL) {
        return;
    }

    cmcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_core_module);

    /* make scan-build happy */
    if (cmcf == NULL) {
        return;
    }

    service_code_var = hev->service_code_var;
    service_code_var->running = 0;

    if (hev->s.fd == NGX_INVALID_FILE) {
        ngx_http_haskell_run_service(cycle, service_code_var, hev->first_run);
        return;
    }

    ev->active = 0;
    if (ngx_del_event(ev, NGX_READ_EVENT, 0) == NGX_ERROR) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "failed to delete event "
                      "after service task was finished");
    }

    if (close(hev->s.fd) == -1) {
        ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                      "failed to close async event channel "
                      "after service task was finished");
    }

    cmvars = cmcf->variables.elts;

    async_data = &service_code_var->future_async_data;

    rc = ngx_http_haskell_yy_handler_result(cycle->log, NULL,
                    async_data->yy_cleanup_data.bufs,
                    async_data->yy_cleanup_data.n_bufs,
                    &async_data->result.data,
                    async_data->yy_cleanup_data.hs_free_stable_ptr,
                    async_data->yy_cleanup_data.locked_bytestring,
                    &cmvars[service_code_var->data->index], 1, 1);
    async_data->result.complete = rc == NGX_OK ? 2 : (rc == NGX_DONE ? 1 : 0);

    if (async_data->yy_cleanup_data.n_bufs == -1) {
        ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                      "memory allocation error while getting "
                      "value of service variable \"%V\", "
                      "using old value",
                      &cmvars[service_code_var->data->index].name);
    }

    if (async_data->error) {
        ngx_uint_t  terminating = async_data->error == 3;

        if (terminating || async_data->error == 4) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "a deadly exception was caught while getting "
                          "value of service variable \"%V\": \"%V\", %s",
                          &cmvars[service_code_var->data->index].name,
                          &async_data->result.data,
                          terminating ? "terminating the worker process" :
                              "the worker process shall be respawned");
            exit(terminating ? 2 : 1);
        }
        log_level = async_data->error == 2 ? NGX_LOG_NOTICE : NGX_LOG_ERR;
        ngx_log_error(log_level, cycle->log, 0,
                      "an exception was caught while getting "
                      "value of service variable \"%V\": \"%V\", "
                      "using old value",
                      &cmvars[service_code_var->data->index].name,
                      &async_data->result.data);
    }

    mcf->hs_free_stable_ptr(service_code_var->locked_async_task);
    service_code_var->has_locked_async_task = 0;

    if (service_code_var->cb) {
        if (service_code_var->noarg) {
            args = service_code_var->data->args.elts;
            ngx_free(args[0].value.data);
            args[0].value.len = 0;
        }
        if (async_data->result.complete == 2) {
            ngx_free(async_data->result.data.data);
        } else if (async_data->result.complete == 1) {
            mcf->hs_free_stable_ptr(
                                async_data->yy_cleanup_data.locked_bytestring);
        }
        return;
    }

    if ((service_code_var->ignore_empty && async_data->result.data.len == 0)
        || async_data->yy_cleanup_data.n_bufs == -1 || async_data->error)
    {
        if (async_data->result.complete == 2) {
            ngx_free(async_data->result.data.data);
        } else if (async_data->result.complete == 1) {
            mcf->hs_free_stable_ptr(
                                async_data->yy_cleanup_data.locked_bytestring);
        }
        goto run_service;
    }

    shm_index = service_code_var->shm_index;

    if (shm_index == NGX_ERROR) {
        if (service_code_var->async_data != NULL) {
            ngx_http_haskell_service_handler_cleanup(
                                                service_code_var->async_data);
        }
        service_code_var->async_data =
                ngx_alloc(sizeof(ngx_http_haskell_async_data_t), cycle->log);

        if (service_code_var->async_data == NULL) {
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to allocate memory for "
                          "service variable \"%V\", using old value",
                          &cmvars[service_code_var->data->index].name);
            ngx_free(async_data->yy_cleanup_data.bufs);
            mcf->hs_free_stable_ptr(
                                async_data->yy_cleanup_data.locked_bytestring);
            goto run_service;
        } else {
            *service_code_var->async_data = *async_data;
            service_code_var->async_data->ref_count = 1;
        }
        goto run_hooks;
    }

    modified = ngx_time();

    shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
    shm_vars = shpool->data;

    NGX_HTTP_HASKELL_SHM_WLOCK

    shm_vars[shm_index].stats.failed = 0;

    var = &shm_vars[shm_index].data.result.data;

    service_code_vars = mcf->service_code_vars.elts;

    if (var->len == async_data->result.data.len) {
        if (var->data == NULL) {
            goto unlock_and_run_cb;
        }
        for (i = 0; i < mcf->service_code_vars.nelts; i++) {
            if (service_code_vars[i].data->index
                == service_code_var->data->index
                && service_code_vars[i].cb && !service_code_vars[i].running)
            {
                if (ngx_memcmp(var->data, async_data->result.data.data,
                               var->len) == 0)
                {
                    goto unlock_and_run_cb;
                } else {
                    shm_var_updated = 1;
                    break;
                }
            }
        }
        ++shm_vars[shm_index].seqn;
        /* FIXME: semantics of the following stats fields depends on actual
         * usage of a callback, because the current code scope may have been
         * skipped from the upper for-cycle depending on the value of flag
         * service_code_vars[i].cb */
        shm_vars[shm_index].stats.modified = modified;
        ++shm_vars[shm_index].stats.changes;
        ngx_memcpy(var->data, async_data->result.data.data, var->len);
        goto unlock_and_run_cb;
    }

    shm_var_updated = 1;

    ++shm_vars[shm_index].seqn;

    shm_vars[shm_index].stats.modified = modified;
    shm_vars[shm_index].stats.size = async_data->result.data.len;
    ++shm_vars[shm_index].stats.changes;

    if (async_data->result.data.len == 0) {
        if (var->data != NULL) {
            ngx_slab_free_locked(shpool, var->data);
        }
        ngx_str_null(var);
        goto unlock_and_run_cb;
    }

    var_data = ngx_slab_alloc_locked(shpool, async_data->result.data.len);
    if (var_data == NULL) {
        ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                      "failed to allocate memory to store "
                      "service variable \"%V\", using old value",
                      &shm_vars[shm_index].handle.name);
        --shm_vars[shm_index].seqn;
        shm_vars[shm_index].stats.failed = 1;
        shm_vars[shm_index].stats.size = var->len;
        ++shm_vars[shm_index].stats.failures;
        --shm_vars[shm_index].stats.changes;
        shm_var_updated = 0;
        skip_hooks = 1;
        goto unlock_and_run_cb;
    }

    ngx_memcpy(var_data, async_data->result.data.data,
               async_data->result.data.len);

    if (var->data != NULL) {
        ngx_slab_free_locked(shpool, var->data);
    }
    var->len = async_data->result.data.len;
    var->data = var_data;

unlock_and_run_cb:

    NGX_HTTP_HASKELL_SHM_UNLOCK

    if (shm_var_updated) {
        var = &async_data->result.data;
        for (i = 0; i < mcf->service_code_vars.nelts; i++) {
            if (service_code_vars[i].data->index
                == service_code_var->data->index
                && service_code_vars[i].cb && !service_code_vars[i].running)
            {
                if (service_code_vars[i].noarg) {
                    args = service_code_vars[i].data->args.elts;
                    args[0].value.len = var->len;
                    args[0].value.data = NULL;
                    if (var->len > 0) {
                        args[0].value.data = ngx_alloc(var->len, cycle->log);
                        if (args[0].value.data == NULL) {
                            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                                          "failed to allocate memory for "
                                          "haskell callback argument");
                            args[0].value.len = 0;
                            continue;
                        }
                        ngx_memcpy(args[0].value.data, var->data, var->len);
                    }
                }
                ngx_http_haskell_run_service(cycle, &service_code_vars[i],
                                             hev->first_run);
            }
        }
    }

run_hooks:

    service_hooks = mcf->service_hooks.elts;
    if (service_code_var->has_update_hooks && !skip_hooks) {
        var = &async_data->result.data;
        for (i = 0; i < mcf->service_hooks.nelts; i++) {
            if (!service_hooks[i].update_hook
                || service_hooks[i].service_code_var_index
                != service_code_var->data->index)
            {
                continue;
            }
            if (shm_index == NGX_ERROR) {
                if (var->len > 0) {
                    /* var_data will be freed on the Haskell side */
                    var_data = ngx_alloc(var->len, cycle->log);
                    if (var_data == NULL) {
                        ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                                    "failed to allocate memory for "
                                    "service update hook data");
                        break;
                    }
                    ngx_memcpy(var_data, var->data, var->len);
                    arg.data = var_data;
                    arg.len = var->len;
                }
                ngx_http_haskell_run_service_hook(cycle, mcf,
                                                  &service_hooks[i], &arg);
                break;
            }
            if (ngx_http_haskell_consume_from_async_event_channel(
                                    service_hooks[i].event_channel[0]) == -1)
            {
                if (ngx_errno != NGX_EAGAIN) {
                    ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                                  "failed to read from service update hook "
                                  "event channel");
                }
            }
            if (ngx_http_haskell_signal_async_event_channel(
                                    service_hooks[i].event_channel[1]) == -1)
            {
                ngx_log_error(NGX_LOG_CRIT, cycle->log, ngx_errno,
                              "failed to write to service update hook "
                              "event channel");
            }
            break;
        }
    }

    if (shm_index != NGX_ERROR) {
        if (async_data->result.complete == 2) {
            ngx_free(async_data->result.data.data);
        } else if (async_data->result.complete == 1) {
            mcf->hs_free_stable_ptr(
                            async_data->yy_cleanup_data.locked_bytestring);
        }
    }

run_service:

    ngx_http_haskell_run_service(cycle, service_code_var, 0);
}


static void
ngx_http_haskell_service_hook_event(ngx_event_t *ev)
{
    ngx_http_haskell_service_hook_event_t       *hev = ev->data;
    ngx_cycle_t                                 *cycle = hev->cycle;

    ngx_http_haskell_main_conf_t                *mcf;
    ngx_http_haskell_service_code_var_data_t    *service_code_var;
    ngx_slab_pool_t                             *shpool;
    ngx_str_t                                    arg = ngx_null_string;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);

    /* make scan-build happy */
    if (mcf == NULL) {
        return;
    }

    service_code_var = hev->hook->service_code_var;

    if (hev->hook->update_hook) {
        ngx_http_haskell_shm_var_handle_t           *shm_vars;
        ngx_str_t                                   *var_data;

        shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
        shm_vars = shpool->data;

        if (service_code_var->shm_index == NGX_ERROR) {
            return;
        }

        NGX_HTTP_HASKELL_SHM_RLOCK

        var_data = &shm_vars[hev->hook->service_hook_index].data.result.data;
        if (shm_vars[service_code_var->shm_index].data.result.data.len == 0) {
            NGX_HTTP_HASKELL_SHM_UNLOCK
            goto run_handler;
        }

        arg.len = var_data->len;

        /* arg.data will be freed on the Haskell side */
        arg.data = ngx_alloc(arg.len, cycle->log);

        if (arg.data == NULL) {
            NGX_HTTP_HASKELL_SHM_UNLOCK
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to allocate memory for service update "
                          "hook data");
            return;
        }

        ngx_memcpy(arg.data, var_data->data, arg.len);

        NGX_HTTP_HASKELL_SHM_UNLOCK

    } else {
        ngx_http_haskell_shm_service_hook_handle_t  *shm_vars;
        ngx_str_t                                   *hook_data;

        if (mcf->service_hooks_shm_zone == NULL) {
            goto run_handler;
        }

        shpool = (ngx_slab_pool_t *) mcf->service_hooks_shm_zone->shm.addr;
        shm_vars = shpool->data;

        ngx_shmtx_lock(&shpool->mutex);

        hook_data = &shm_vars[hev->hook->service_hook_index].data;
        if (hook_data == NULL) {
            ngx_shmtx_unlock(&shpool->mutex);
            goto run_handler;
        }

        arg.len = hook_data->len;

        /* arg.data will be freed on the Haskell side */
        arg.data = ngx_alloc(arg.len, cycle->log);

        if (arg.data == NULL) {
            ngx_shmtx_unlock(&shpool->mutex);
            ngx_log_error(NGX_LOG_CRIT, cycle->log, 0,
                          "failed to allocate memory for service "
                          "hook data");
            return;
        }

        ngx_memcpy(arg.data, hook_data->data, arg.len);

        ngx_shmtx_unlock(&shpool->mutex);
    }

run_handler:

    ngx_http_haskell_run_service_hook(cycle, mcf, hev->hook, &arg);

    if (hev->hook->update_hook) {
        return;
    }

    /* BEWARE: flag active is set asynchronously and without notification from
     * the Haskell service, however this is not really a problem as it is set
     * only once on the first run of the service and before the service's
     * handler actually starts */
    if (service_code_var->active && service_code_var->running
        && service_code_var->has_locked_async_task)
    {
        mcf->service_hook_interrupt(service_code_var->locked_async_task);
    }
}


static void
ngx_http_haskell_run_service_hook(ngx_cycle_t *cycle,
                                  ngx_http_haskell_main_conf_t *mcf,
                                  ngx_http_haskell_service_hook_t *hook,
                                  ngx_str_t *arg)
{
    ngx_http_haskell_handler_t                  *handlers;
    ngx_str_t                                   *res_yy, buf_yy;
    HsStablePtr                                  locked_bytestring = NULL;
    CInt                                         len;
    CUInt                                        err;
    ngx_str_t                                    reslen = ngx_null_string;

    handlers = mcf->handlers.elts;

    res_yy = &buf_yy;

    err = ((ngx_http_haskell_handler_ioy_y)
               handlers[hook->handler].self)
                    (arg->data, arg->len, &res_yy, &len, &locked_bytestring);

    if (len == -1) {
        ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                      "memory allocation error while running service hook");
    }

    if (res_yy != NULL) {
        reslen.len = res_yy->len;
        reslen.data = res_yy->data;
    }

    if (ngx_http_haskell_yy_handler_result(cycle->log, NULL, res_yy, len,
                                           &reslen, NULL, NULL, NULL, 0, 1)
        == NGX_ERROR)
    {
        ngx_log_error(NGX_LOG_NOTICE, cycle->log, 0,
                      "service hook returned bad result");
    } else {
        if (err) {
            ngx_log_error(NGX_LOG_ERR, cycle->log, 0,
                          "an exception was caught while running service hook: "
                          "\"%V\"", &reslen);
        } else if (reslen.len > 0) {
            ngx_log_error(NGX_LOG_NOTICE, cycle->log, 0,
                          "service hook reported \"%V\"", &reslen);
        }
    }

    if (len > 0) {
        if (len > 1) {
            ngx_free(res_yy);
            ngx_free(reslen.data);
        }
        mcf->hs_free_stable_ptr(locked_bytestring);
    }
}


ngx_int_t
ngx_http_haskell_update_service_hook_data(ngx_http_request_t *r,
                                          ngx_int_t hook_index, ngx_str_t data)
{
    ngx_http_haskell_main_conf_t                *mcf;
    ngx_slab_pool_t                             *shpool;
    ngx_http_haskell_shm_service_hook_handle_t  *shm_vars;
    ngx_str_t                                   *hook_data;
    u_char                                      *hook_data_data;

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);

    shpool = (ngx_slab_pool_t *) mcf->service_hooks_shm_zone->shm.addr;
    shm_vars = shpool->data;

    ngx_shmtx_lock(&shpool->mutex);

    hook_data = &shm_vars[hook_index].data;

    if (hook_data->data != NULL) {
        ngx_slab_free_locked(shpool, hook_data->data);
    }
    ngx_str_null(hook_data);

    hook_data_data = ngx_slab_alloc_locked(shpool, data.len);
    if (hook_data_data == NULL) {
        ngx_shmtx_unlock(&shpool->mutex);
        ngx_log_error(NGX_LOG_CRIT, r->connection->log, 0,
                      "failed to allocate memory to store service hook data");
        return NGX_ERROR;
    }

    ngx_memcpy(hook_data_data, data.data, data.len);

    hook_data->len = data.len;
    hook_data->data = hook_data_data;

    ngx_shmtx_unlock(&shpool->mutex);

    return NGX_OK;
}


static void
ngx_http_haskell_service_handler_cleanup(void *data)
{
    ngx_http_haskell_async_data_t            *async_data = data;

    if (--async_data->ref_count != 0) {
        return;
    }

    if (async_data->result.complete == 2) {
        ngx_free(async_data->result.data.data);
    } else if (async_data->result.complete == 1) {
        async_data->yy_cleanup_data.hs_free_stable_ptr(
                                async_data->yy_cleanup_data.locked_bytestring);
    }

    ngx_free(async_data);
}


ngx_int_t
ngx_http_haskell_run_service_handler(ngx_http_request_t *r,
                                     ngx_http_variable_value_t *v,
                                     uintptr_t data)
{
    ngx_int_t                                 *index = (ngx_int_t *) data;

    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_int_t                                  found_idx = NGX_ERROR;
    ngx_slab_pool_t                           *shpool;
    ngx_http_haskell_shm_var_handle_t         *shm_vars;
    ngx_int_t                                  shm_index = NGX_ERROR;
    ngx_str_t                                  res = ngx_null_string;
    ngx_http_haskell_async_data_t             *service_data = NULL;
    ngx_pool_cleanup_t                        *cln;

    if (index == NULL) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (*index == service_code_vars[i].data->index) {
            shm_index = service_code_vars[i].shm_index;
            found_idx = i;
            break;
        }
    }
    if (found_idx == NGX_ERROR) {
        return NGX_ERROR;
    }

    if (shm_index != NGX_ERROR) {
        shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
        shm_vars = shpool->data;

        NGX_HTTP_HASKELL_SHM_RLOCK

        if (shm_vars[shm_index].data.result.data.len == 0) {
            NGX_HTTP_HASKELL_SHM_UNLOCK
            goto update_var;
        }

        /* BEWARE: there is no cache for res, normally it must be OK because
         * this handler must be called only once in normal case when the
         * associated variable is not cacheable */
        res.len = shm_vars[shm_index].data.result.data.len;
        res.data = ngx_pnalloc(r->pool, res.len);
        if (res.data == NULL) {
            NGX_HTTP_HASKELL_SHM_UNLOCK
            return NGX_ERROR;
        }

        ngx_memcpy(res.data, shm_vars[shm_index].data.result.data.data,
                   res.len);

        NGX_HTTP_HASKELL_SHM_UNLOCK

        goto update_var;
    }

    /* FIXME: using cache for res. That's OK except that shm service variables
     * do not use cache (see the comment a few lines above). To ensure the same
     * behavior in all cases, perhaps it makes sense to prohibit directive
     * haskell_var_nocacheable for variables with
     * ngx_http_haskell_run_service_handler */

    for (cln = r->pool->cleanup; cln != NULL; cln = cln->next) {
        if (cln->handler == ngx_http_haskell_service_handler_cleanup) {
            service_data = cln->data;
            if (service_data->index != *index) {
                service_data = NULL;
                continue;
            }
            break;
        }
    }

    if (service_data == NULL) {
        service_data = service_code_vars[found_idx].async_data;
        if (service_data == NULL) {
            return NGX_ERROR;
        }
        cln = ngx_pool_cleanup_add(r->pool, 0);
        if (cln == NULL) {
            ngx_log_error(NGX_LOG_ERR, r->connection->log, 0,
                          "failed to register cleanup handler for "
                          "service data");
            return NGX_ERROR;
        }
        ++service_data->ref_count;
        cln->handler = ngx_http_haskell_service_handler_cleanup;
        cln->data = service_data;
    }

    res = service_data->result.data;

update_var:

    v->len = res.len;
    v->data = res.data;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


ngx_int_t
ngx_http_haskell_shm_update_var_handler(ngx_http_request_t *r,
                                        ngx_http_variable_value_t *v,
                                        uintptr_t data)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_http_haskell_shm_var_handle_data_t    *shm_handle_data;
    ngx_slab_pool_t                           *shpool;
    ngx_http_haskell_shm_var_handle_t         *shm_vars;
    ngx_int_t                                  shm_index = NGX_ERROR;
    ngx_str_t                                  res = ngx_null_string;

    shm_handle_data = (ngx_http_haskell_shm_var_handle_data_t *) data;

    if (shm_handle_data->index == NGX_ERROR) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (shm_handle_data->index == service_code_vars[i].data->index) {
            shm_index = service_code_vars[i].shm_index;
            break;
        }
    }

    if (shm_index == NGX_ERROR) {
        return NGX_ERROR;
    }

    shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
    shm_vars = shpool->data;

    NGX_HTTP_HASKELL_SHM_RLOCK

    /* FIXME: there is an extremely rare hypothetical case when seqn in shm
     * wraps around after overflow while the local value of seqn did not change
     * during the whole wrapping cycle, and thus the variable will mistakenly
     * not be updated */
    if (shm_handle_data->seqn == shm_vars[shm_index].seqn) {
        NGX_HTTP_HASKELL_SHM_UNLOCK
        goto update_var;
    }

    shm_handle_data->seqn = shm_vars[shm_index].seqn;

    if (shm_vars[shm_index].data.result.data.len == 0) {
        NGX_HTTP_HASKELL_SHM_UNLOCK
        goto update_var;
    }

    res.len = shm_vars[shm_index].data.result.data.len;
    res.data = ngx_pnalloc(r->pool, res.len);
    if (res.data == NULL) {
        NGX_HTTP_HASKELL_SHM_UNLOCK
        return NGX_ERROR;
    }

    ngx_memcpy(res.data, shm_vars[shm_index].data.result.data.data, res.len);

    NGX_HTTP_HASKELL_SHM_UNLOCK

update_var:

    v->len = res.len;
    v->data = res.data;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


ngx_int_t
ngx_http_haskell_shm_stats_var_handler(ngx_http_request_t *r,
                                       ngx_http_variable_value_t *v,
                                       uintptr_t data)
{
    ngx_uint_t                                 i;
    ngx_http_haskell_main_conf_t              *mcf;
    ngx_http_haskell_service_code_var_data_t  *service_code_vars;
    ngx_http_haskell_shm_var_handle_data_t    *shm_handle_data;
    ngx_slab_pool_t                           *shpool;
    ngx_http_haskell_shm_var_handle_t         *shm_vars;
    ngx_int_t                                  shm_index = NGX_ERROR;
    ngx_http_haskell_shm_stats_var_t           stats;
    u_char                                    *buf, *p;
    static const size_t                        buf_size = 256;

    shm_handle_data = (ngx_http_haskell_shm_var_handle_data_t *) data;

    if (shm_handle_data->index == NGX_ERROR) {
        return NGX_ERROR;
    }

    mcf = ngx_http_get_module_main_conf(r, ngx_http_haskell_module);
    service_code_vars = mcf->service_code_vars.elts;

    for (i = 0; i < mcf->service_code_vars.nelts; i++) {
        if (shm_handle_data->index == service_code_vars[i].data->index) {
            shm_index = service_code_vars[i].shm_index;
            break;
        }
    }

    if (shm_index == NGX_ERROR) {
        return NGX_ERROR;
    }

    shpool = (ngx_slab_pool_t *) mcf->shm_zone->shm.addr;
    shm_vars = shpool->data;

    NGX_HTTP_HASKELL_SHM_RLOCK

    stats = shm_vars[shm_index].stats;

    NGX_HTTP_HASKELL_SHM_UNLOCK

    buf = ngx_pnalloc(r->pool, buf_size);
    if (buf == NULL) {
        return NGX_ERROR;
    }

    p = ngx_snprintf(buf, buf_size - 1, "%T | %uz | %ui | %ui | %ui",
                     stats.modified, stats.size, stats.changes, stats.failures,
                     stats.failed);

    v->len = p - buf;
    v->data = buf;
    v->valid = 1;
    v->no_cacheable = 0;
    v->not_found = 0;

    return NGX_OK;
}


#ifdef NGX_HTTP_HASKELL_SHM_USE_SHARED_RLOCK

/* the following 2 functions are implemented with ngx_shmtx_lock() used as
 * a pattern */

static void
ngx_http_haskell_wlock(ngx_fd_t fd)
{
    ngx_err_t  err;

    err = ngx_lock_fd(fd);

    if (err == 0) {
        return;
    }

    ngx_log_abort(err, "write lock on data in shared memory failed");
}


static void
ngx_http_haskell_rlock(ngx_fd_t fd)
{
    ngx_err_t  err;

    err = ngx_http_haskell_rlock_fd(fd);

    if (err == 0) {
        return;
    }

    ngx_log_abort(err, "read lock on data in shared memory failed");
}


/* this function is implemented with ngx_shmtx_unlock() used as a pattern */

static void
ngx_http_haskell_unlock(ngx_fd_t fd)
{
    ngx_err_t  err;

    err = ngx_unlock_fd(fd);

    if (err == 0) {
        return;
    }

    ngx_log_abort(err, "unlock on data in shared memory failed");
}


/* this function is the same as existing Nginx function ngx_lock_fd(), except
 * it uses F_RDLCK instead of F_WRLCK, and thus is capable to create shared
 * locks for simultaneous access from multiple processes */

static ngx_err_t
ngx_http_haskell_rlock_fd(ngx_fd_t fd)
{
    struct flock  fl;

    ngx_memzero(&fl, sizeof(struct flock));
    fl.l_type = F_RDLCK;
    fl.l_whence = SEEK_SET;

    if (fcntl(fd, F_SETLKW, &fl) == -1) {
        return ngx_errno;
    }

    return 0;
}

#endif

