ARG BASE_IMAGE_TAG=latest

FROM lyokha/nginx-haskell-module:$BASE_IMAGE_TAG

COPY data/test-prometheus.conf /opt/nginx/conf/nginx.conf
COPY data/test_prometheus.hs /build/test_prometheus.hs
COPY data/Makefile /build/Makefile
COPY data/Setup.hs /build/Setup.hs
COPY data/test-prometheus.cabal /build/test-prometheus.cabal
COPY data/cabal.project /build/cabal.project
COPY data/hie.yaml /build/hie.yaml

RUN (cd /build; make install) && rm -rf /build

CMD ["/opt/nginx/sbin/nginx", "-g", "daemon off;"]

