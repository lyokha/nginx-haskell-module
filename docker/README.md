#### Basic docker image

This docker image contains Nginx built with *nginx-haskell-module*,
[*nginx-custom-counters-module*](https://github.com/lyokha/nginx-custom-counters-module),
[*nginx-combined-upstream-module*](https://github.com/lyokha/nginx-combined-upstreams-module),
[*echo-nginx-module*](https://github.com/openresty/echo-nginx-module), and
plugins [*nginx-healthcheck-plugin*](https://github.com/lyokha/nginx-healthcheck-plugin)
and [*nginx-log-plugin*](https://github.com/lyokha/nginx-log-plugin). For
building custom Haskell libraries and C plugins, it additionally contains *gcc*
and *ghc* compilers, *cabal* and Nginx source code.

The image can be pulled from *Docker hub*.

```ShellSession
$ docker pull lyokha/nginx-haskell-module:2.7.1-fedora-33-ghc-8.8
```

Basically, the image was supposed to be used for testing examples shown in
[*Yet another doc with
examples*](https://nginx-haskell-module.readthedocs.io/en/latest/yet-another-doc-with-examples/nginx-haskell-module-yadwe.html), but it can be easily adapted for using with other Haskell handlers
and Nginx configurations.

Run a container with

```ShellSession
$ docker run --name test-yadwe --network host -d lyokha/nginx-haskell-module:2.7.1-fedora-33-ghc-8.8
```

Do some testing with *curl* (see details in the aforementioned document).

```ShellSession
$ curl 'http://127.0.0.1:8010/?u=hello&r=world&a=1&b=10&c=1'
toUpper hello = HELLO
reverse world = dlrow
1 `isInList` [10, 1, ] = 1
```

```ShellSession
$ curl -d 'timer=3' 'http://127.0.0.1:8010/timer'
Waited 3 sec
```

```ShellSession
$ curl 'http://127.0.0.1:8010/httpbin/sortlinks'
/forms/post
https://github.com/requests/httpbin
https://kennethreitz.org
mailto:me@kennethreitz.org
```

```ShellSession
$ file marbles.tif
marbles.tif: TIFF image data, big-endian, direntries=16, height=1001, bps=0, compression=LZW, PhotometricIntepretation=RGB, width=1419
$ curl -s --data-binary @marbles.tif 'http://127.0.0.1:8010/convert/topng' -o marbles.png
$ file marbles.png
marbles.png: PNG image data, 1419 x 1001, 8-bit/color RGB, non-interlaced
```

```ShellSession
$ curl -D- 'http://localhost:8010/cplugin'
HTTP/1.1 200 OK
Server: nginx/1.18.0
Date: Thu, 18 Feb 2021 15:02:58 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive
X-Powered-By: Nginx Haskell module

Test C plugin returned Success!
```

Stop and delete the container.

```ShellSession
$ docker kill -s QUIT test-yadwe
test-yadwe
$ docker rm test-yadwe
test-yadwe
```

#### Building custom docker images

The basic docker image can be used to spin off another docker image with
different Haskell handlers and Nginx configuration. Say, we want to test custom
Prometheus metrics available with *nginx-custom-counters-module* and
[*ngx-export-tools-extra*](https://github.com/lyokha/ngx-export-tools-extra).

Below are Dockerfile and files *test-prometheus.hs* and *test-prometheus.conf*
put in sub-directory *data/*.

```Dockerfile
FROM lyokha/nginx-haskell-module

COPY data/test-prometheus.conf /opt/nginx/conf/nginx.conf
COPY data/test-prometheus.hs /build/test-prometheus.hs

RUN cd /build                                        && \
    ghc -Wall -O2 -dynamic -shared -fPIC              \
            -lHSrts_thr-ghc$(ghc --numeric-version)   \
            -lngx_healthcheck_plugin -lngx_log_plugin \
            test-prometheus.hs -o test-prometheus.so && \
    mv test-prometheus.so /var/lib/nginx             && \
    cd ..                                            && \
    rm -rf /build

CMD ["/opt/nginx/sbin/nginx", "-g", "daemon off;"]
```

```haskell
module DockerTestPrometheus where

import NgxExport.Tools.Prometheus ()
```

The basic docker image already contains the required Haskell package
*ngx-export-tools-extra*. It may happen that the custom Haskell code requires
modules from packages not installed in the basic image: in this case `cabal
v1-update && cabal v1-install <list-of-required-packages>` must be put inside
the RUN recipe of the Dockerfile.

```nginx
user                    nginx;
worker_processes        4;

events {
    worker_connections  1024;
}

error_log               /tmp/nginx-test-haskell-error.log info;

http {
    default_type        application/octet-stream;
    sendfile            on;
    error_log           /tmp/nginx-test-haskell-error.log info;
    access_log          /tmp/nginx-test-haskell-access.log;

    haskell load /var/lib/nginx/test-prometheus.so;

    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_visits", "Number of visits to specific locations")
                    ]
                , pcGauges = fromList
                    ["cnt_stub_status_active"]
                , pcScale1000 = fromList
                    []
                }';

    haskell_var_empty_on_error $hs_prom_metrics;

    server {
        listen       8010;
        server_name  main;

        location /a {
            counter $cnt_visits@location=(/a) inc;
            return 200;
        }

        location /b {
            counter $cnt_visits@location=(/b) inc;
            return 200;
        }

        location / {
            counter $cnt_visits@location=(other) inc;
            return 200;
        }
    }

    server {
        listen       8020;
        server_name  stats;

        location / {
            haskell_run toPrometheusMetrics $hs_prom_metrics
                    '["main"
                     ,$cnt_collection
                     ,$cnt_histograms
                     ,{"cnt_stub_status_active": $cnt_stub_status_active
                      ,"cnt_uptime": $cnt_uptime
                      ,"cnt_uptime_reload": $cnt_uptime_reload
                      }
                     ]';

            if ($hs_prom_metrics = '') {
                return 503;
            }

            default_type "text/plain; version=0.0.4; charset=utf-8";

            echo -n $hs_prom_metrics;
        }

        location /counters {
            echo $cnt_collection;
        }

        location /uptime {
            echo "Uptime (after reload): $cnt_uptime ($cnt_uptime_reload)";
        }
    }
}
```

Let's build module,

```ShellSession
$ docker build -t nginx-haskell-module-test-prometheus .
```

run a container,

```ShellSession
$ docker run --name test-prometheus --network host -d nginx-haskell-module-test-prometheus
```

and do some tests.

```ShellSession
$ for i in {1..20} ; do curl 'http://127.0.0.1:8010/a' & done
  ...
$ for i in {1..50} ; do curl 'http://127.0.0.1:8010/b' & done
  ...
$ curl 'http://127.0.0.1:8020/'
# HELP cnt_stub_status_active
# TYPE cnt_stub_status_active gauge
cnt_stub_status_active 1.0
# HELP cnt_uptime
# TYPE cnt_uptime counter
cnt_uptime 19.0
# HELP cnt_uptime_reload
# TYPE cnt_uptime_reload counter
cnt_uptime_reload 19.0
# HELP cnt_visits Number of visits to specific locations
# TYPE cnt_visits counter
cnt_visits{location="/a"} 20.0
cnt_visits{location="/b"} 50.0
cnt_visits{location="other"} 0.0
```

```ShellSession
$ curl -s 'http://127.0.0.1:8020/counters' | jq
{
  "main": {
    "cnt_visits@location=(/a)": 20,
    "cnt_visits@location=(/b)": 50,
    "cnt_visits@location=(other)": 0
  }
}
```

```ShellSession
$ curl 'http://127.0.0.1:8020/uptime'
Uptime (after reload): 64 (64)
```

Stop and delete the container.

```ShellSession
$ docker kill -s QUIT test-prometheus
test-prometheus
$ docker rm test-prometheus
test-prometheus
```

