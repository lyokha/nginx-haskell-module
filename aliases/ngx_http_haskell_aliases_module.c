#include "ngx_http_haskell_module.h"


typedef enum {
    ngx_http_haskell_aliases_set_mode_cache,
    ngx_http_haskell_aliases_set_mode_lazy,
    ngx_http_haskell_aliases_set_mode_var_alias
} ngx_http_haskell_aliases_set_mode_e;


static char *ngx_http_haskell_aliases_set(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf, ngx_http_haskell_aliases_set_mode_e mode);
static char *ngx_http_haskell_aliases_cache_set(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_aliases_lazy_set(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_aliases_var_alias(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);
static char *ngx_http_haskell_aliases_var_configure(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);


static ngx_command_t  ngx_http_haskell_aliases_module_commands[] = {

    { ngx_string("cache_set"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE2,
      ngx_http_haskell_aliases_cache_set,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("lazy_set"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE2,
      ngx_http_haskell_aliases_lazy_set,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("var_alias"),
      NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_HTTP_LIF_CONF|NGX_CONF_TAKE2,
      ngx_http_haskell_aliases_var_alias,
      NGX_HTTP_LOC_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("var_nocacheable"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_aliases_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },
    { ngx_string("var_nohash"),
      NGX_HTTP_MAIN_CONF|NGX_CONF_1MORE,
      ngx_http_haskell_aliases_var_configure,
      NGX_HTTP_MAIN_CONF_OFFSET,
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
ngx_http_haskell_aliases_set(ngx_conf_t *cf, ngx_command_t *cmd, void *conf,
                             ngx_http_haskell_aliases_set_mode_e mode)
{
    ngx_str_t                       *value;
    ngx_conf_t                       cf_haskell_run;
    ngx_array_t                      cf_haskell_run_args;
    ngx_str_t                       *directive, *handler, *var, *expr;
    ngx_http_haskell_loc_conf_t     *hlcf;

    value = cf->args->elts;

    if (ngx_array_init(&cf_haskell_run_args, cf->pool, 4, sizeof(ngx_str_t))
        != NGX_OK)
    {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to allocate memory for directive \"%V\" "
                           "data", &value[0]);
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

    switch (mode) {
    case ngx_http_haskell_aliases_set_mode_cache:
        var->len = value[1].len + 2;
        var->data = ngx_pnalloc(cf->pool, var->len);
        if (var->data == NULL) {
            return NGX_CONF_ERROR;
        }
        ngx_memcpy(var->data, (u_char *) "<!", 2);
        ngx_memcpy(var->data + 2, value[1].data, value[1].len);
        break;
    case ngx_http_haskell_aliases_set_mode_lazy:
        *var = value[1];
        break;
    case ngx_http_haskell_aliases_set_mode_var_alias:
        var->len = value[1].len + 2;
        var->data = ngx_pnalloc(cf->pool, var->len);
        if (var->data == NULL) {
            return NGX_CONF_ERROR;
        }
        ngx_memcpy(var->data, (u_char *) "<<", 2);
        ngx_memcpy(var->data + 2, value[1].data, value[1].len);
        break;
    default:
        return NGX_CONF_ERROR;
    }

    expr = ngx_array_push(&cf_haskell_run_args);
    if (expr == NULL) {
        return NGX_CONF_ERROR;
    }

    *expr = value[2];

    cf_haskell_run = *cf;
    cf_haskell_run.args = &cf_haskell_run_args;

    hlcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_haskell_module);

    return ngx_http_haskell_run(&cf_haskell_run, NULL, hlcf);
}


static char *
ngx_http_haskell_aliases_cache_set(ngx_conf_t *cf, ngx_command_t *cmd,
                                   void *conf)
{
    return ngx_http_haskell_aliases_set(cf, cmd, conf,
                                ngx_http_haskell_aliases_set_mode_cache);
}


static char *
ngx_http_haskell_aliases_lazy_set(ngx_conf_t *cf, ngx_command_t *cmd,
                                  void *conf)
{
    return ngx_http_haskell_aliases_set(cf, cmd, conf,
                                ngx_http_haskell_aliases_set_mode_lazy);
}


static char *
ngx_http_haskell_aliases_var_alias(ngx_conf_t *cf, ngx_command_t *cmd,
                                   void *conf)
{
    return ngx_http_haskell_aliases_set(cf, cmd, conf,
                                ngx_http_haskell_aliases_set_mode_var_alias);
}


static char *
ngx_http_haskell_aliases_var_configure(ngx_conf_t *cf, ngx_command_t *cmd,
                                       void *conf)
{
    ngx_str_t                       *value;
    ngx_http_haskell_main_conf_t    *hmcf;

    value = cf->args->elts;

    if (value[0].len == 15
        && ngx_strncmp(value[0].data, "var_nocacheable", 15) == 0)
    {
        ngx_str_set(&value[0], "haskell_var_nocacheable");
    } else if (value[0].len == 10
        && ngx_strncmp(value[0].data, "var_nohash", 10) == 0)
    {
        ngx_str_set(&value[0], "haskell_var_nohash");
    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to configure an alias for directive \"%V\"",
                           &value[0]);
        return NGX_CONF_ERROR;
    }

    hmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_haskell_module);

    return ngx_http_haskell_var_configure(cf, NULL, hmcf);
}

