##### Building and installation

The build tool requires Cabal, [*patchelf*](https://github.com/NixOS/patchelf),
and utility *nhm-tool* which is shipped with package *ngx-export-distribution*.

```ShellSession
$ make
$ sudo make install
```

With ghc older than *9.0.1*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

