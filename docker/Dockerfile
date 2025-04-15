ARG FEDORA_VERSION=41
ARG NGINX_VERSION=1.26.3

FROM fedora:$FEDORA_VERSION AS base

ARG GHC_VERSION=9.10

ENV CABAL_DIR=/cabal
ENV CABAL_UPDATE_DIR=/cabal-update

RUN dnf install -y gcc xz openssl-devel pcre-devel zlib-devel procps-ng && \
    dnf install -y ghc$GHC_VERSION ghc$GHC_VERSION-compiler-default     && \
    dnf install -y patchelf                                             && \
    dnf clean all                                                       && \
    rm -rf /var/cache/dnf

RUN curl -L 'https://downloads.haskell.org/~cabal/cabal-install-3.14.2.0/cabal-install-3.14.2.0-x86_64-linux-fedora38.tar.xz' \
            -o cabal-install.tar.xz    && \
    tar xvf cabal-install.tar.xz cabal && \
    mv cabal /usr/bin                  && \
    rm -f cabal-install.tar.xz

RUN curl -L 'https://raw.githubusercontent.com/zserge/jsmn/25647e692c7906b96ffd2b05ca54c097948e879c/jsmn.h' \
            -o jsmn.h && \
    mv jsmn.h /usr/include

RUN mkdir -p /var/lib/nginx

RUN useradd -s /sbin/nologin nginx


FROM base AS nginx-build

ARG NGINX_VERSION

RUN curl -L 'https://github.com/lyokha/nginx-healthcheck-plugin/archive/1.6.3.tar.gz' \
            -o nginx-healthcheck-plugin.tar.gz                      && \
    tar xvf nginx-healthcheck-plugin.tar.gz                         && \
    curl -L 'https://github.com/lyokha/nginx-log-plugin/archive/1.5.2.tar.gz' \
            -o nginx-log-plugin.tar.gz                              && \
    tar xvf nginx-log-plugin.tar.gz                                 && \
    rm -f nginx-healthcheck-plugin.tar.gz nginx-log-plugin.tar.gz   && \
    curl -L 'https://github.com/openresty/echo-nginx-module/archive/v0.63.tar.gz' \
            -o echo-nginx-module.tar.gz                             && \
    tar xvf echo-nginx-module.tar.gz                                && \
    curl -L 'https://github.com/lyokha/nginx-custom-counters-module/archive/4.4.tar.gz' \
            -o nginx-custom-counters-module.tar.gz                  && \
    tar xvf nginx-custom-counters-module.tar.gz                     && \
    curl -L 'https://github.com/lyokha/nginx-combined-upstreams-module/archive/2.3.1.tar.gz' \
            -o nginx-combined-upstreams-module.tar.gz               && \
    tar xvf nginx-combined-upstreams-module.tar.gz                  && \
    curl -L 'https://github.com/lyokha/nginx-haskell-module/archive/3.4.3.tar.gz' \
            -o nginx-haskell-module.tar.gz                          && \
    tar xvf nginx-haskell-module.tar.gz                             && \
    curl -L 'https://github.com/lyokha/nginx-easy-context/archive/1.0.tar.gz' \
            -o nginx-easy-context.tar.gz                            && \
    tar xvf nginx-easy-context.tar.gz                               && \
    curl -L 'https://github.com/lyokha/nginx-proxy-peer-host/archive/1.0.tar.gz' \
            -o nginx-proxy-peer-host.tar.gz                         && \
    tar xvf nginx-proxy-peer-host.tar.gz                            && \
    curl -L "https://nginx.org/download/nginx-$NGINX_VERSION.tar.gz" \
            -o nginx.tar.gz                                         && \
    tar xvf nginx.tar.gz                                            && \
    cd nginx-"$NGINX_VERSION"                                       && \
    NGX_HTTP_COMBINED_UPSTREAMS_PERSISTENT_UPSTRAND_INTERCEPT_CTX=yes  \
    NGX_HTTP_CUSTOM_COUNTERS_PERSISTENCY=yes                           \
            ./configure --prefix=/opt/nginx                            \
            --user=nginx --group=nginx                                 \
            --with-http_ssl_module                                     \
            --with-http_stub_status_module                             \
            --add-module=../echo-nginx-module-0.63                     \
            --add-module=../nginx-easy-context-1.0                     \
            --add-module=../nginx-proxy-peer-host-1.0                  \
            --add-module=../nginx-custom-counters-module-4.4           \
            --add-module=../nginx-combined-upstreams-module-2.3.1      \
            --add-module=../nginx-haskell-module-3.4.3                 \
            --add-module=../nginx-haskell-module-3.4.3/aliases         \
            --add-module=../nginx-haskell-module-3.4.3/examples/dynamicUpstreams/nginx-upconf-module \
            --add-dynamic-module=/nginx-healthcheck-plugin-1.6.3       \
            --add-dynamic-module=/nginx-log-plugin-1.5.2               \
            --add-dynamic-module=/nginx-log-plugin-1.5.2/module     && \
    make                                                            && \
    make install                                                    && \
    cp objs/ngx_healthcheck_plugin.so \
            /usr/lib64/libngx_healthcheck_plugin.so                 && \
    cp objs/ngx_log_plugin.so \
            /usr/lib64/libngx_log_plugin.so                         && \
    cp objs/ngx_log_plugin_module.so \
            /var/lib/nginx                                          && \
    cd ..                                                           && \
    rm -rf echo-nginx-module.tar.gz echo-nginx-module-0.63             \
            nginx-custom-counters-module.tar.gz                        \
            nginx-custom-counters-module-4.4                           \
            nginx-combined-upstreams-module.tar.gz                     \
            nginx-combined-upstreams-module-2.3.1                      \
            nginx-haskell-module.tar.gz nginx-haskell-module-3.4.3     \
            nginx-healthcheck-plugin-1.6.3 nginx-log-plugin-1.5.2      \
            nginx-easy-context.tar.gz nginx-easy-context-1.0           \
            nginx-proxy-peer-host.tar.gz nginx-proxy-peer-host-1.0     \
            nginx.tar.gz


FROM nginx-build

ARG NGINX_VERSION

COPY data/test.conf /opt/nginx/conf/nginx.conf
COPY data/test.hs /build/test.hs
COPY data/test_c_plugin.c /build/test_c_plugin.c
COPY data/Makefile /build/Makefile
COPY data/Setup.hs /build/Setup.hs
COPY data/test.cabal /build/test.cabal
COPY data/cabal.project /build/cabal.project
COPY data/hie.yaml /build/hie.yaml

RUN cabal update                                               && \
    (cd /build; make NGX_HOME=/nginx-"$NGINX_VERSION" install) && \
    rm -rf /build

CMD ["/opt/nginx/sbin/nginx", "-g", "daemon off;"]

