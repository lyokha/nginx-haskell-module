/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_load.c
 *
 *    Description:  Load compiled Haskell library
 *
 *        Version:  1.0
 *        Created:  05.02.2018 14:21:03
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <dlfcn.h>
#include <ghcversion.h>

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_load.h"


static const ngx_str_t  haskell_module_type_checker_prefix =
ngx_string("type_");

/* FIXME: installing signal handlers ("yes", which is default) makes a worker
 * defunct when sending SIGINT to it, disabling signal handlers by setting "no"
 * fixes this but may make a worker hang on terminate_async_task() when sending
 * any signal to it, which is apparently worse; current workaround makes use of
 * "yes" with further calling of install_signal_handler() that makes a worker
 * ignore SIGINT while still keeping it killable by the master's SIGINT */
static char  *haskell_module_install_signal_handlers_option =
"--install-signal-handlers=yes";

static const HsInt32  haskell_module_ngx_export_api_version_major = 1;
static const HsInt32  haskell_module_ngx_export_api_version_minor = 4;


ngx_int_t
ngx_http_haskell_load(ngx_cycle_t *cycle)
{
    typedef HsInt32               (*version_f_t)(HsInt32 *, HsInt32);
    typedef HsInt32               (*type_checker_t)(void);

    ngx_uint_t                      i;
    ngx_http_haskell_main_conf_t   *mcf;
    ngx_http_haskell_handler_t     *handlers;
    char                           *dl_error;
    char                          **argv = NULL;
    int                             argc;
    version_f_t                     version_f = NULL;
    HsInt32                         version[4], version_len;
    void                          (*install_signal_handler)(void);

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return NGX_OK;
    }

    if (mcf->dl_handle != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "haskell library has been unexpectedly loaded");
        return NGX_ERROR;
    }

    mcf->dl_handle = dlopen((char *) mcf->lib_path.data, RTLD_LAZY);
    dl_error = dlerror();
    if (mcf->dl_handle == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load compiled haskell library: %s", dl_error);
        return NGX_ERROR;
    }

    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_modular) {
        version_f = (version_f_t) dlsym(mcf->dl_handle, "ngxExportVersion");
        dl_error = dlerror();
        if (version_f == NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to get API version of haskell library: %s",
                          dl_error);
            goto dlclose_and_exit;
        }
    }

    mcf->hs_init = (void (*)(int *, char ***)) dlsym(mcf->dl_handle,
                                                     "hs_init_with_rtsopts");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_init_with_rtsopts\": %s",
                      dl_error);
        goto dlclose_and_exit;
    }

    mcf->hs_exit = (void (*)(void)) dlsym(mcf->dl_handle, "hs_exit");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_exit\": %s", dl_error);
        goto dlclose_and_exit;
    }

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 702
    mcf->hs_add_root = (void (*)(void (*)(void))) dlsym(mcf->dl_handle,
                                                        "hs_add_root");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_add_root\": %s", dl_error);
        goto dlclose_and_exit;
    }

    mcf->init_HsModule = (void (*)(void)) dlsym(mcf->dl_handle,
                                            "__stginit_NgxHaskellUserRuntime");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"__stginit_NgxHaskellUserRuntime\": %s", dl_error);
        goto dlclose_and_exit;
    }
#endif

    mcf->hs_free_stable_ptr = (void (*)(HsStablePtr)) dlsym(mcf->dl_handle,
                                                        "hs_free_stable_ptr");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"hs_free_stable_ptr\": %s",
                      dl_error);
        goto dlclose_and_exit;
    }

    mcf->terminate_async_task = (void (*)(HsStablePtr)) dlsym(mcf->dl_handle,
                                                    "ngxExportTerminateTask");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function \"ngxExportTerminateTask\": %s",
                      dl_error);
        goto dlclose_and_exit;
    }

    mcf->service_hook_interrupt = (void (*)(HsStablePtr)) dlsym(mcf->dl_handle,
                                            "ngxExportServiceHookInterrupt");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"ngxExportServiceHookInterrupt\": %s", dl_error);
        goto dlclose_and_exit;
    }

    mcf->rts_has_thread_support = (HsBool (*)(void)) dlsym(mcf->dl_handle,
                                                    "rtsSupportsBoundThreads");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"rtsSupportsBoundThreads\": %s", dl_error);
        goto dlclose_and_exit;
    }

    mcf->set_cycle_ptr = (void (*)(HsPtr)) dlsym(mcf->dl_handle,
                                                 "ngxExportSetCyclePtr");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"ngxExportSetCyclePtr\": %s", dl_error);
        goto dlclose_and_exit;
    }

    install_signal_handler = (void (*)(void)) dlsym(mcf->dl_handle,
                                            "ngxExportInstallSignalHandler");
    dl_error = dlerror();
    if (dl_error != NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to load function "
                      "\"ngxExportInstallSignalHandler\": %s", dl_error);
        goto dlclose_and_exit;
    }

    argc = mcf->program_options.nelts + 3 + mcf->rts_options.nelts;
    argv = ngx_palloc(cycle->pool, argc * sizeof(char *));
    if (argv == NULL) {
        ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                      "failed to allocate artifacts for haskell init options");
        goto dlclose_and_exit;
    }
    argv[0] = "NgxHaskellUserRuntime";
    for (i = 0; i < mcf->program_options.nelts; i++) {
        argv[1 + i] = ((char **) mcf->program_options.elts)[i];
        if (ngx_strcmp(argv[1 + i], "+RTS") == 0) {
            ngx_log_error(NGX_LOG_ALERT, cycle->log, 0,
                          "found option \"+RTS\" in the list of "
                          "\"ghc program_options\", consider using directive "
                          "\"ghc rts_options\" for RTS options");
        }
    }
    argv[mcf->program_options.nelts + 1] = "+RTS";
    argv[mcf->program_options.nelts + 2] =
            haskell_module_install_signal_handlers_option;
    for (i = 0; i < mcf->rts_options.nelts; i++) {
        argv[mcf->program_options.nelts + 3 + i] =
                ((char **) mcf->rts_options.elts)[i];
    }

    /* FIXME: hs_init() may exit(EXIT_FAILURE), and in this case Nginx master
     * may begin to continuously restart workers; not sure if it's fixable */
    mcf->hs_init(&argc, &argv);
    ngx_pfree(cycle->pool, argv);

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 702
    mcf->hs_add_root(mcf->init_HsModule);
#endif

    install_signal_handler();

    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_modular) {
        version_len = version_f(version, sizeof(version) / sizeof(version[0]));
        if (version_len < 2) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "bad API version of haskell library");
            goto unload_and_exit;
        }
        if (version[0] != haskell_module_ngx_export_api_version_major
            || version[1] != haskell_module_ngx_export_api_version_minor)
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "bad API version of haskell library: %D.%D "
                          "(expected %D.%D)", version[0], version[1],
                          haskell_module_ngx_export_api_version_major,
                          haskell_module_ngx_export_api_version_minor);
            goto unload_and_exit;
        }
    }

    handlers = mcf->handlers.elts;

    for (i = 0; i < mcf->handlers.nelts; i++) {
        ngx_str_t        handler_name;
        type_checker_t   type_checker;
        char            *type_checker_name = NULL;
        ngx_uint_t       wrong_n_args = 0;

        if (handlers[i].name.len <= ngx_http_haskell_module_handler_prefix.len
            || ngx_strncmp(handlers[i].name.data,
                           ngx_http_haskell_module_handler_prefix.data,
                           ngx_http_haskell_module_handler_prefix.len) != 0)
        {
            continue;
        }

        handler_name.len = handlers[i].name.len -
                ngx_http_haskell_module_handler_prefix.len;
        handler_name.data = handlers[i].name.data +
                ngx_http_haskell_module_handler_prefix.len;

        handlers[i].self = dlsym(mcf->dl_handle,
                                 (char *) handlers[i].name.data);
        dl_error = dlerror();
        if (dl_error != NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to load haskell handler \"%V\": %s",
                          &handler_name, dl_error);
            goto unload_and_exit;
        }

        type_checker_name = ngx_palloc(cycle->pool,
            haskell_module_type_checker_prefix.len + handlers[i].name.len + 1);
        if (type_checker_name == NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to allocate artifacts for type checker");
            goto unload_and_exit;
        }

        ngx_memcpy(type_checker_name,
                   haskell_module_type_checker_prefix.data,
                   haskell_module_type_checker_prefix.len);
        ngx_memcpy(type_checker_name + haskell_module_type_checker_prefix.len,
                   handlers[i].name.data, handlers[i].name.len + 1);

        type_checker = (type_checker_t) dlsym(mcf->dl_handle,
                                              type_checker_name);
        dl_error = dlerror();
        ngx_pfree(cycle->pool, type_checker_name);
        if (dl_error != NULL) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "failed to load haskell handler type checker \"%V\": "
                          "%s", &handler_name, dl_error);
            goto unload_and_exit;
        }

        handlers[i].type = type_checker();

        if ((handlers[i].role == ngx_http_haskell_handler_role_content_handler
             && (handlers[i].type != ngx_http_haskell_handler_type_ch
                 && handlers[i].type != ngx_http_haskell_handler_type_uch
                 && handlers[i].type != ngx_http_haskell_handler_type_y_y))
            ||
            (handlers[i].role
             == ngx_http_haskell_handler_role_async_content_handler
             && handlers[i].type != ngx_http_haskell_handler_type_ach)
            ||
            (handlers[i].role
             == ngx_http_haskell_handler_role_async_content_handler_rb
             && handlers[i].type != ngx_http_haskell_handler_type_ach_rb)
            ||
            (handlers[i].role == ngx_http_haskell_handler_role_variable
             && (handlers[i].type == ngx_http_haskell_handler_type_ch
                 || handlers[i].type == ngx_http_haskell_handler_type_uch
                 || handlers[i].type == ngx_http_haskell_handler_type_ach
                 || handlers[i].type == ngx_http_haskell_handler_type_ach_rb
                 || handlers[i].type == ngx_http_haskell_handler_type_ioy_yy))
            ||
            ((handlers[i].role == ngx_http_haskell_handler_role_async_variable
              || handlers[i].role
              == ngx_http_haskell_handler_role_service_variable)
             && handlers[i].type != ngx_http_haskell_handler_type_ioy_y)
            ||
            (handlers[i].role == ngx_http_haskell_handler_role_async_variable_rb
             && handlers[i].type != ngx_http_haskell_handler_type_ioy_yy)
            ||
            (handlers[i].role == ngx_http_haskell_handler_role_service_hook
             && handlers[i].type != ngx_http_haskell_handler_type_ioy_y))
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "haskell handler \"%V\" role and type mismatch",
                          &handler_name);
            goto unload_and_exit;
        }

        if (handlers[i].role == ngx_http_haskell_handler_role_content_handler
            && ((handlers[i].type == ngx_http_haskell_handler_type_ch
                 && handlers[i].unsafe)
                || (handlers[i].type == ngx_http_haskell_handler_type_uch
                    && !handlers[i].unsafe)))
        {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "haskell handler \"%V\" safety attribute mismatch",
                          &handler_name);
            goto unload_and_exit;
        }

        switch (handlers[i].type) {
        case ngx_http_haskell_handler_type_s_s:
        case ngx_http_haskell_handler_type_b_s:
        case ngx_http_haskell_handler_type_y_y:
        case ngx_http_haskell_handler_type_b_y:
        case ngx_http_haskell_handler_type_ioy_y:
        case ngx_http_haskell_handler_type_ioy_yy:
        case ngx_http_haskell_handler_type_ch:
        case ngx_http_haskell_handler_type_uch:
        case ngx_http_haskell_handler_type_ach:
        case ngx_http_haskell_handler_type_ach_rb:
            wrong_n_args = handlers[i].n_args[0] == 0
                        || handlers[i].n_args[1] > 0
                        || handlers[i].n_args[2] > 0 ? 1 : 0;
            break;
        case ngx_http_haskell_handler_type_s_ss:
        case ngx_http_haskell_handler_type_b_ss:
            wrong_n_args = handlers[i].n_args[0] > 0
                        || handlers[i].n_args[1] == 0
                        || handlers[i].n_args[2] > 0 ? 1 : 0;
            break;
        case ngx_http_haskell_handler_type_s_ls:
        case ngx_http_haskell_handler_type_b_ls:
            break;
        default:
            goto unload_and_exit;
        }

        if (wrong_n_args) {
            ngx_log_error(NGX_LOG_EMERG, cycle->log, 0,
                          "actual type of haskell handler \"%V\" "
                          "does not match call samples", &handler_name);
            goto unload_and_exit;
        }
    }

    return NGX_OK;

dlclose_and_exit:

    dlclose(mcf->dl_handle);

    return NGX_ERROR;

unload_and_exit:

    ngx_http_haskell_unload(cycle, 0);

    return NGX_ERROR;
}


void
ngx_http_haskell_unload(ngx_cycle_t *cycle, ngx_uint_t exiting)
{
    ngx_http_haskell_main_conf_t    *mcf;

    mcf = ngx_http_cycle_get_module_main_conf(cycle, ngx_http_haskell_module);
    if (mcf == NULL || !mcf->code_loaded) {
        return;
    }

    if (mcf->dl_handle != NULL) {
        mcf->hs_exit();
        /* dlclose() may cause sigsegv when a haskell service wakes up
         * during or after munmap() but before the worker exits */
        if (!exiting) {
            dlclose(mcf->dl_handle);
        }
        mcf->dl_handle = NULL;
    }
}

