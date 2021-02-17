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

Run the image with

```ShellSession
$ docker run --name test-yadwe --network host -d lyokha/nginx-haskell-module:2.7.1-fedora-33-ghc-8.8
```

Do some testing with *curl* (see details in the aforementioned document).

```ShellSession
$ curl 'http://127.0.0.1:8010/?u=hello&r=world&a=1&b=10&c=1'
toUpper hello = HELLO
reverse world = dlrow
1 `isInList` [10, 1, ] = 1
$ curl -d 'timer=3' 'http://127.0.0.1:8010/timer'
Waited 3 sec
$ curl 'http://127.0.0.1:8010/httpbin/sortlinks'
/forms/post
https://github.com/requests/httpbin
https://kennethreitz.org
mailto:me@kennethreitz.org
```

Stop and delete the container.

```ShellSession
$ docker kill -s QUIT test-yadwe
test-yadwe
$ docker rm test-yadwe
test-yadwe
```

