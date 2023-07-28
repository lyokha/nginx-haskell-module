##### Building and installation

The build tool requires Cabal, [*patchelf*](https://github.com/NixOS/patchelf),
[*hslibdeps*](https://github.com/lyokha/nginx-haskell-module/blob/master/utils/README.md#utility-hslibdeps),
and [*cabal-plan*](https://hackage.haskell.org/package/cabal-plan).

```ShellSession
$ make NGX_HOME=/path/to/nginx_sources
$ sudo make install
```

With ghc older than *8.10.6*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

