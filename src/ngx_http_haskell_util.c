/*
 * =============================================================================
 *
 *       Filename:  ngx_http_haskell_util.c
 *
 *    Description:  Utility functions
 *
 *        Version:  2.0
 *        Created:  05.02.2018 15:10:56
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include "ngx_http_haskell_util.h"


const ngx_uint_t  ngx_http_haskell_module_use_eventfd_channel =
#if (NGX_HAVE_EVENTFD)
    1;
#else
    0;
#endif


ngx_int_t
ngx_http_haskell_yy_handler_result(ngx_log_t *log, ngx_pool_t *pool,
                                   ngx_str_t *bufs, CInt n_bufs, ngx_str_t *res,
                                   void (*hs_free_stable_ptr)(HsStablePtr),
                                   HsStablePtr locked_bytestring,
                                   ngx_http_variable_t *cmvar,
                                   ngx_uint_t cleanup, ngx_uint_t service)
{
    ngx_int_t                            i;
    ngx_pool_cleanup_t                  *cln = NULL;
    ngx_http_haskell_yy_cleanup_data_t  *clnd = NULL;
    ngx_int_t                            written;
    ngx_int_t                            rc;

    rc = service ? NGX_DONE : NGX_OK;

    if (n_bufs == -1) {
        return NGX_ERROR;
    }

    if (bufs == NULL) {
        rc = service && cleanup ? NGX_ERROR : NGX_OK;
        if (n_bufs == 0) {
            res->len = 0;
            res->data = (u_char *) "";
        } else {
            ngx_log_error(NGX_LOG_CRIT, log, 0,
                          "impossible branch while running "
                          "haskell handler");
            rc = NGX_ERROR;
        }
        return rc;
    }

    if (n_bufs == 0) {
        ngx_log_error(NGX_LOG_CRIT, log, 0,
                      "impossible branch while running "
                      "haskell handler");
        return NGX_ERROR;       /* cleanup is dangerous here! */
    } else if (n_bufs == 1) {
        if (cleanup && !service /* any synchronous handler */) {
            cln = ngx_pool_cleanup_add(pool, 0);
            clnd = ngx_pcalloc(pool,
                               sizeof(ngx_http_haskell_yy_cleanup_data_t));
            if (cln == NULL || clnd == NULL) {
                if (cmvar != NULL) {
                    ngx_log_error(NGX_LOG_ERR, log, 0,
                                  "failed to allocate cleanup handler "
                                  "for variable \"%V\"", cmvar->name);
                }
                goto cleanup;
            }
            clnd->n_bufs = 1;
            clnd->hs_free_stable_ptr = hs_free_stable_ptr;
            clnd->locked_bytestring = locked_bytestring;
            cln->handler = ngx_http_haskell_yy_handler_cleanup;
            cln->data = clnd;
            res->len = bufs->len;
            res->data = bufs->data;
        }
        return rc;
    } else {
        res->len = 0;
        for (i = 0; i < n_bufs; i++) {
            res->len += bufs[i].len;
        }
        res->data = service ? ngx_alloc(res->len, log) :
                                            ngx_pnalloc(pool, res->len);
        if (res->data == NULL) {
            if (cmvar != NULL) {
                ngx_log_error(NGX_LOG_ERR, log, 0,
                              "failed to allocate contiguous memory block "
                              "for variable \"%V\"", cmvar->name);
            } else {
                ngx_log_error(NGX_LOG_ERR, log, 0,
                              "failed to allocate contiguous memory block "
                              "for error log message in haskell async "
                              "content handler");
            }
            goto cleanup;
        }
        written = 0;
        for (i = 0; i < n_bufs; i++) {
            ngx_memcpy(res->data + written, bufs[i].data, bufs[i].len);
            written += bufs[i].len;
        }
        if (cleanup && bufs != NULL) {
            ngx_free(bufs);
            hs_free_stable_ptr(locked_bytestring);
        }
    }

    return NGX_OK;

cleanup:

    if (cleanup && bufs != NULL) {
        if (n_bufs > 1) {
            ngx_free(bufs);
        }
        hs_free_stable_ptr(locked_bytestring);
    }

    return NGX_ERROR;
}


void
ngx_http_haskell_yy_handler_cleanup(void *data)
{
    ngx_http_haskell_yy_cleanup_data_t       *clnd = data;

    if (clnd->n_bufs > 0) {
        if (clnd->n_bufs > 1 || clnd->free_single_buffer) {
            ngx_free(clnd->bufs);
        }
        if (clnd->hs_free_stable_ptr != NULL) {
            clnd->hs_free_stable_ptr(clnd->locked_bytestring);
        }
        clnd->n_bufs = 0;
    }
}


ngx_int_t
ngx_http_haskell_open_async_event_channel(ngx_fd_t fd[2])
{
#if (NGX_HAVE_EVENTFD)
#if (NGX_HAVE_SYS_EVENTFD_H)
    fd[0] = fd[1] = eventfd(0, EFD_NONBLOCK);
#else
    fd[0] = fd[1] = syscall(323, O_NONBLOCK);
#endif
    return fd[0] == NGX_INVALID_FILE ? NGX_ERROR : NGX_OK;
#else
    if (pipe(fd) == -1) {
        fd[0] = fd[1] = NGX_INVALID_FILE;
        return NGX_ERROR;
    }
    if (fcntl(fd[0], F_SETFL, O_NONBLOCK) == -1
        || fcntl(fd[1], F_SETFL, O_NONBLOCK) == -1)
    {
        ngx_http_haskell_close_async_event_channel(NULL, fd);
        fd[0] = fd[1] = NGX_INVALID_FILE;
        return NGX_ERROR;
    }
    return NGX_OK;
#endif
}


void
ngx_http_haskell_close_async_event_channel(ngx_log_t *log, ngx_fd_t fd[2])
{
    ngx_int_t  i;

    for (i = 0; i < (fd[0] == fd[1] ? 1 : 2); i++) {
        if (close(fd[i]) == -1 && log != NULL) {
            ngx_log_error(NGX_LOG_CRIT, log, ngx_errno,
                          "failed to close file descriptor of "
                          "async event channel");
        }
    }
}


ssize_t
ngx_http_haskell_signal_async_event_channel(ngx_fd_t fd)
{
    ssize_t  res = 0, cur = 0;

#if (NGX_HAVE_EVENTFD)
    uint64_t  v = 1;

    for ( ;; ) {
        cur = write(fd, &v + cur, sizeof(uint64_t) - cur);
        if (cur == -1) {
            if (ngx_errno == NGX_EINTR) {
                cur = res;
                continue;
            } else {
                return -1;
            }
        }
        res += cur;
        if (res >= (ssize_t) sizeof(uint64_t) || cur == 0) {
            break;
        }
    }
#else
    uint8_t  v = 1;

    for ( ;; ) {
        cur = write(fd, &v + cur, sizeof(uint8_t) - cur);
        if (cur == -1) {
            if (ngx_errno == NGX_EINTR) {
                cur = res;
                continue;
            } else {
                return -1;
            }
        }
        res += cur;
        if (res >= (ssize_t) sizeof(uint8_t) || cur == 0) {
            break;
        }
    }
#endif

    return res;
}


ssize_t
ngx_http_haskell_consume_from_async_event_channel(ngx_fd_t fd)
{
    ssize_t  res = 0, cur = 0;

#if (NGX_HAVE_EVENTFD)
    uint64_t  v;

    for ( ;; ) {
        cur = read(fd, &v + cur, sizeof(uint64_t) - cur);
        if (cur == -1) {
            if (ngx_errno == NGX_EINTR) {
                cur = res;
                continue;
            } else {
                return -1;
            }
        }
        res += cur;
        if (res >= (ssize_t) sizeof(uint64_t) || cur == 0) {
            break;
        }
    }
#else
    uint8_t  v;

    for ( ;; ) {
        cur = read(fd, &v + cur % sizeof(uint8_t),
                   sizeof(uint8_t) - cur % sizeof(uint8_t));
        if (cur == -1) {
            if (ngx_errno == NGX_EAGAIN) {
                break;
            } else if (ngx_errno == NGX_EINTR) {
                cur = res;
                continue;
            } else {
                return -1;
            }
        }
        res += cur;
        if (cur == 0) {
            break;
        }
    }
#endif

    return res;
}

