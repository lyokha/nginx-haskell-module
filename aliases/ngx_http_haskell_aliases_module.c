#include "ngx_http_haskell_module.h"


static char *ngx_http_haskell_aliases_cache_variable(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);


static ngx_command_t  ngx_http_haskell_aliases_module_commands[] = {

    { ngx_string("cache_variable"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE2,
      ngx_http_haskell_aliases_cache_variable,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },

      ngx_null_command
};


static ngx_http_module_t  ngx_http_haskell_aliases_module_ctx = {
    NULL,                                  /* preconfiguration */
    NULL,                                  /* postconfiguration */

    NULL,                                  /* create main configuration */
    NULL,                                  /* init main configuration */

    NULL,                                  /* create server configuration */
    NULL,                                  /* merge server configuration */

    NULL,                                  /* create location configuration */
    NULL                                   /* merge location configuration */
};


ngx_module_t  ngx_http_haskell_aliases_module = {
    NGX_MODULE_V1,
    &ngx_http_haskell_aliases_module_ctx,      /* module context */
    ngx_http_haskell_aliases_module_commands,  /* module directives */
    NGX_HTTP_MODULE,                           /* module type */
    NULL,                                      /* init master */
    NULL,                                      /* init module */
    NULL,                                      /* init process */
    NULL,                                      /* init thread */
    NULL,                                      /* exit thread */
    NULL,                                      /* exit process */
    NULL,                                      /* exit master */
    NGX_MODULE_V1_PADDING
};


static char *
ngx_http_haskell_aliases_cache_variable(ngx_conf_t *cf, ngx_command_t *cmd,
                                        void *conf)
{
    ngx_str_t                       *value;
    ngx_conf_t                       cf_haskell_run;
    ngx_array_t                      cf_haskell_run_args;
    ngx_str_t                       *directive, *handler, *var, *cached_var;
    ngx_http_haskell_loc_conf_t     *hlcf;

    value = cf->args->elts;

    if (ngx_array_init(&cf_haskell_run_args, cf->pool, 4, sizeof(ngx_str_t))
        != NGX_OK)
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to allocate memory for directive "
                           "\"cache_variable\" data");
        return NGX_CONF_ERROR;
    }

    directive = ngx_array_push(&cf_haskell_run_args);
    if (directive == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_str_set(directive, "haskell_run");

    handler = ngx_array_push(&cf_haskell_run_args);
    if (handler == NULL) {
        return NGX_CONF_ERROR;
    }

    ngx_str_set(handler, "!");

    var = ngx_array_push(&cf_haskell_run_args);
    if (var == NULL) {
        return NGX_CONF_ERROR;
    }

    var->len = value[1].len + 2;
    var->data = ngx_pnalloc(cf->pool, var->len);
    if (var->data == NULL) {
        return NGX_CONF_ERROR;
    }
    ngx_memcpy(var->data, (u_char *) "<!", 2);
    ngx_memcpy(var->data + 2, value[1].data, value[1].len);

    cached_var = ngx_array_push(&cf_haskell_run_args);
    if (cached_var == NULL) {
        return NGX_CONF_ERROR;
    }

    *cached_var = value[2];

    cf_haskell_run = *cf;
    cf_haskell_run.args = &cf_haskell_run_args;

    hlcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_haskell_module);

    return ngx_http_haskell_run(&cf_haskell_run, NULL, hlcf);
}

