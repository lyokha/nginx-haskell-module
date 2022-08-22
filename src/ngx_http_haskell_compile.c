/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_compile.c
 *
 *    Description:  Compile Haskell code
 *
 *        Version:  3.0
 *        Created:  05.02.2018 13:48:01
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_module.h"
#include "ngx_http_haskell_compile.h"
#include "ngx_http_haskell_standalone.h"

#include <ghcversion.h>


#if defined(__GLASGOW_HASKELL__)

static const ngx_str_t  haskell_compile_cmd =
ngx_string("ghc -O2 -dynamic -shared -fPIC -o ");
static const ngx_str_t  template_haskell_option =
ngx_string(" -XTemplateHaskell");
#if __GLASGOW_HASKELL__ < 900
static const ngx_str_t  ghc_rtslib_vanilla =
ngx_string(" -lHSrts-ghc$(ghc --numeric-version)");
static const ngx_str_t  ghc_rtslib_thr =
ngx_string(" -lHSrts_thr-ghc$(ghc --numeric-version)");
static const ngx_str_t  ghc_rtslib_debug =
ngx_string(" -lHSrts_debug-ghc$(ghc --numeric-version)");
static const ngx_str_t  ghc_rtslib_thr_debug =
ngx_string(" -lHSrts_thr_debug-ghc$(ghc --numeric-version)");
#else
static const ngx_str_t  ghc_rtslib_vanilla =
ngx_string(" -flink-rts");
static const ngx_str_t  ghc_rtslib_thr =
ngx_string(" -flink-rts -threaded");
static const ngx_str_t  ghc_rtslib_debug =
ngx_string(" -flink-rts -debug");
static const ngx_str_t  ghc_rtslib_thr_debug =
ngx_string(" -flink-rts -threaded -debug");
#endif


char *
ngx_http_haskell_write_code(ngx_conf_t *cf, void *conf, ngx_str_t source_name,
                            ngx_str_t *fragments, ngx_int_t n_fragments)
{
    ngx_http_haskell_main_conf_t  *mcf = conf;

    ngx_int_t                      i;
    ngx_file_t                     out;
    ngx_uint_t                     standalone_mode;

    ngx_memzero(&out, sizeof(ngx_file_t));

    out.name.data = ngx_pnalloc(cf->pool, source_name.len + 1);
    if (out.name.data == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(out.name.data, source_name.data, source_name.len);
    out.name.data[source_name.len] = '\0';
    out.name.len = source_name.len;

    out.fd = ngx_open_file(out.name.data, NGX_FILE_WRONLY, NGX_FILE_TRUNCATE,
                           NGX_FILE_DEFAULT_ACCESS);
    if (out.fd == NGX_INVALID_FILE) {
        goto write_failed;
    }

    standalone_mode =
            mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_standalone;

    if (standalone_mode
        && ngx_write_file(&out,
                          ngx_http_haskell_module_standalone_header.data,
                          ngx_http_haskell_module_standalone_header.len,
                          out.offset)
        == NGX_ERROR)
    {
        goto write_failed;
    }
    for (i = 0; i < n_fragments; i++) {
        if (ngx_write_file(&out,
                           fragments[i].data,
                           fragments[i].len,
                           out.offset)
            == NGX_ERROR)
        {
            goto write_failed;
        }
    }
    if (standalone_mode
        && ngx_write_file(&out,
                          ngx_http_haskell_module_standalone_footer.data,
                          ngx_http_haskell_module_standalone_footer.len,
                          out.offset)
        == NGX_ERROR)
    {
        goto write_failed;
    }

    if (ngx_close_file(out.fd) == NGX_FILE_ERROR) {
        ngx_conf_log_error(NGX_LOG_ERR, cf, ngx_errno,
                           "failed to close haskell source code file handle");
    }

    return NGX_CONF_OK;

write_failed:

    ngx_conf_log_error(NGX_LOG_EMERG, cf, ngx_errno,
                       "failed to write haskell source code file");

    return NGX_CONF_ERROR;
}


char *
ngx_http_haskell_compile(ngx_conf_t *cf, void *conf, ngx_str_t source_name)
{
    ngx_http_haskell_main_conf_t  *mcf = conf;

    char                          *compile_cmd;
    ngx_str_t                      rtslib;
    ngx_uint_t                     extra_len = 0, th_len = 0;
    ngx_uint_t                     passed_len, full_len;
    ngx_uint_t                     compile_cmd_len;

    compile_cmd_len = haskell_compile_cmd.len;

    switch (mcf->compile_mode) {
    case ngx_http_haskell_compile_mode_vanilla:
        rtslib = ghc_rtslib_vanilla;
        break;
    case ngx_http_haskell_compile_mode_threaded:
        rtslib = ghc_rtslib_thr;
        break;
    case ngx_http_haskell_compile_mode_debug:
        rtslib = ghc_rtslib_debug;
        break;
    case ngx_http_haskell_compile_mode_threaded_debug:
        rtslib = ghc_rtslib_thr_debug;
        break;
    default:
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "unexpected haskell compile mode %d",
                           mcf->compile_mode);
        return NGX_CONF_ERROR;
    }

    if (mcf->ghc_extra_options.len > 0) {
        extra_len = mcf->ghc_extra_options.len;
    }
    if (mcf->wrap_mode == ngx_http_haskell_module_wrap_mode_modular) {
        th_len = template_haskell_option.len;
    }
    full_len = compile_cmd_len + mcf->lib_path.len + source_name.len +
            rtslib.len + extra_len + th_len + 2;

    compile_cmd = ngx_pnalloc(cf->pool, full_len);
    if (compile_cmd == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_memcpy(compile_cmd,
               haskell_compile_cmd.data, compile_cmd_len);
    ngx_memcpy(compile_cmd + compile_cmd_len,
               mcf->lib_path.data, mcf->lib_path.len);
    passed_len = compile_cmd_len + mcf->lib_path.len;
    ngx_memcpy(compile_cmd + passed_len,
               rtslib.data, rtslib.len);
    passed_len += rtslib.len;
    if (th_len > 0) {
        ngx_memcpy(compile_cmd + passed_len,
                   template_haskell_option.data, th_len);
        passed_len += th_len;
    }
    if (extra_len > 0) {
        ngx_memcpy(compile_cmd + passed_len,
                   mcf->ghc_extra_options.data, mcf->ghc_extra_options.len);
        passed_len += extra_len;
    }
    ngx_memcpy(compile_cmd + passed_len, " ", 1);
    ngx_memcpy(compile_cmd + passed_len + 1, source_name.data, source_name.len);
    compile_cmd[full_len - 1] = '\0';

    if (system(compile_cmd) != 0) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to compile haskell source code file with "
                           "command \"%s\"", compile_cmd);
        return NGX_CONF_ERROR;
    }

    return NGX_CONF_OK;
}

#else

char *
ngx_http_haskell_write_code(ngx_conf_t *cf, void *conf, ngx_str_t source_name,
                            ngx_str_t *fragments, ngx_int_t n_fragments)
{
    ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
            "compilation of haskell source code is only supported with ghc");
    return NGX_CONF_ERROR;
}


char *
ngx_http_haskell_compile(ngx_conf_t *cf, void *conf, ngx_str_t source_name)
{
    ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
            "compilation of haskell source code is only supported with ghc");
    return NGX_CONF_ERROR;
}

#endif

