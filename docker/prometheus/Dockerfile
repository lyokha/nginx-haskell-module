ARG BASE_IMAGE_TAG=latest

FROM lyokha/nginx-haskell-module:$BASE_IMAGE_TAG

COPY data/test-prometheus.conf /opt/nginx/conf/nginx.conf
COPY data/test-prometheus.hs /build/test-prometheus.hs

RUN cd /build                                        && \
    ghc -Wall -O2 -dynamic -shared -fPIC -flink-rts -threaded \
            -lngx_healthcheck_plugin -lngx_log_plugin         \
            test-prometheus.hs -o test-prometheus.so && \
    mv test-prometheus.so /var/lib/nginx             && \
    cd ..                                            && \
    rm -rf /build

CMD ["/opt/nginx/sbin/nginx", "-g", "daemon off;"]

