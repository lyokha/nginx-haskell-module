FROM fedora:33 AS base

ARG GHC_VERSION=8.8

ENV CABAL_DIR=/cabal
ENV CABAL_UPDATE_DIR=/cabal-update

RUN dnf install -y gcc xz pcre-devel zlib-devel procps-ng                  && \
    dnf module enable -y ghc:$GHC_VERSION                                  && \
    dnf module install -y ghc/minimal                                      && \
    dnf install -y ghc-template-haskell-devel ghc-time-devel ghc-stm-devel && \
    dnf clean all                                                          && \
    rm -rf /var/cache/dnf

RUN curl -L 'https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz' \
            -o cabal-install.tar.xz    && \
    tar xvf cabal-install.tar.xz cabal && \
    mv cabal /usr/bin                  && \
    rm -f cabal-install.tar.xz

RUN curl -L 'https://raw.githubusercontent.com/zserge/jsmn/0837288b7c6dbd3c015f6a184cfa1e99937c5d09/jsmn.h' \
            -o jsmn.h && \
    mv jsmn.h /usr/include

RUN mkdir -p /var/lib/nginx

RUN useradd -s /sbin/nologin nginx


FROM base AS cabal-install

ARG CABAL_CONSTRAINTS

RUN cabal v1-update                                               && \
    cabal_constraints=$(echo $CABAL_CONSTRAINTS | \
            sed "s/\s*\([^;]*\)\s*;\?\s*/--constraint='\1' /g")   && \
    eval "cabal v1-install ngx-export-tools-extra \
            $cabal_constraints"                                   && \
    curl -L 'https://github.com/lyokha/nginx-healthcheck-plugin/archive/1.0.tar.gz' \
            -o nginx-healthcheck-plugin.tar.gz                    && \
    tar xvf nginx-healthcheck-plugin.tar.gz                       && \
    curl -L 'https://github.com/lyokha/nginx-log-plugin/archive/0.2.tar.gz' \
            -o nginx-log-plugin.tar.gz                            && \
    tar xvf nginx-log-plugin.tar.gz                               && \
    cd nginx-healthcheck-plugin-1.0                               && \
    eval "cabal v1-install $cabal_constraints"                    && \
    cd ../nginx-log-plugin-0.2                                    && \
    eval "cabal v1-install $cabal_constraints"                    && \
    cd ..                                                         && \
    rm -f nginx-healthcheck-plugin.tar.gz nginx-log-plugin.tar.gz && \
    eval "cabal v1-install JuicyPixels regex-pcre-builtin \
            $cabal_constraints"                                   && \
    mkdir $CABAL_UPDATE_DIR                                       && \
    mv "$CABAL_DIR"/packages/hackage.haskell.org/01-index.* \
            "$CABAL_UPDATE_DIR"                                   && \
    rm -rf "$CABAL_DIR"/logs/*


FROM base AS nginx-build

COPY --from=cabal-install /cabal /cabal
COPY --from=cabal-install /root/.ghc /root/.ghc
COPY --from=cabal-install /nginx-healthcheck-plugin-1.0 /nginx-healthcheck-plugin-1.0
COPY --from=cabal-install /nginx-log-plugin-0.2 /nginx-log-plugin-0.2

RUN curl -L 'https://github.com/openresty/echo-nginx-module/archive/v0.62.tar.gz' \
            -o echo-nginx-module.tar.gz                && \
    tar xvf echo-nginx-module.tar.gz                   && \
    curl -L 'https://github.com/lyokha/nginx-custom-counters-module/archive/4.3.tar.gz' \
            -o nginx-custom-counters-module.tar.gz     && \
    tar xvf nginx-custom-counters-module.tar.gz        && \
    curl -L 'https://github.com/lyokha/nginx-combined-upstreams-module/archive/2.1.tar.gz' \
            -o nginx-combined-upstreams-module.tar.gz  && \
    tar xvf nginx-combined-upstreams-module.tar.gz     && \
    curl -L 'https://github.com/lyokha/nginx-haskell-module/archive/2.8.0.tar.gz' \
            -o nginx-haskell-module.tar.gz             && \
    tar xvf nginx-haskell-module.tar.gz                && \
    curl -L 'https://nginx.org/download/nginx-1.18.0.tar.gz' \
            -o nginx.tar.gz                            && \
    tar xvf nginx.tar.gz                               && \
    cd nginx-1.18.0                                    && \
    NGX_HTTP_CUSTOM_COUNTERS_PERSISTENCY=yes                    \
            ./configure --prefix=/opt/nginx                     \
            --user=nginx --group=nginx                          \
            --with-http_stub_status_module                      \
            --add-module=../echo-nginx-module-0.62              \
            --add-module=../nginx-custom-counters-module-4.3    \
            --add-module=../nginx-combined-upstreams-module-2.1 \
            --add-module=../nginx-haskell-module-2.8.0          \
            --add-dynamic-module=/nginx-healthcheck-plugin-1.0  \
            --add-dynamic-module=/nginx-log-plugin-0.2 && \
    make                                               && \
    make install                                       && \
    cp objs/ngx_healthcheck_plugin.so \
            /usr/lib64/libngx_healthcheck_plugin.so    && \
    cp objs/ngx_log_plugin.so \
            /usr/lib64/libngx_log_plugin.so            && \
    cd ..                                              && \
    rm -rf echo-nginx-module.tar.gz echo-nginx-module-0.62         \
            nginx-custom-counters-module.tar.gz                    \
            nginx-custom-counters-module-4.3                       \
            nginx-combined-upstreams-module.tar.gz                 \
            nginx-combined-upstreams-module-2.1                    \
            nginx-haskell-module.tar.gz nginx-haskell-module-2.8.0 \
            nginx-healthcheck-plugin-1.0 nginx-log-plugin-0.2      \
            nginx.tar.gz


FROM nginx-build

COPY data/test.conf /opt/nginx/conf/nginx.conf
COPY data/test.hs /build/test.hs
COPY data/test_c_plugin.h /build/test_c_plugin.h
COPY data/test_c_plugin.c /build/test_c_plugin.c

RUN cd /build                                  && \
    NGX_HOME=/nginx-1.18.0                     && \
    gcc -O2 -fPIC -c -o test_c_plugin.o    \
            -I $NGX_HOME/src/core          \
            -I $NGX_HOME/src/http          \
            -I $NGX_HOME/src/http/modules  \
            -I $NGX_HOME/src/event         \
            -I $NGX_HOME/src/event/modules \
            -I $NGX_HOME/src/os/unix       \
            -I $NGX_HOME/objs test_c_plugin.c  && \
    ghc -Wall -O2 -dynamic -shared -fPIC              \
            -lHSrts_thr-ghc$(ghc --numeric-version)   \
            -lngx_healthcheck_plugin -lngx_log_plugin \
            test_c_plugin.o test.hs -o test.so && \
    mv test.so /var/lib/nginx                  && \
    cd ..                                      && \
    rm -rf /build

CMD ["/opt/nginx/sbin/nginx", "-g", "daemon off;"]

