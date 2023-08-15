<a id="utility-hslibdeps"></a>

### hslibdeps

Collects all Haskell libraries the target library depends on. Optionally,
patches the target library by extending its *RUNPATH* value with a specified
directory (this requires utility [*patchelf*](https://nixos.org/patchelf.html)).

Say, to collect all Haskell libraries a library *ngx_haskell.so* depends on, run

```ShellSession
$ hslibdeps -d hslibs ngx_haskell.so
```

All dependent libraries will be copied into directory *hslibs/* (or into default
directory *.hslibs/* when option *-d* is omitted). Then they can be moved to the
target directory on the target machine.

If the target directory (say, */var/lib/nginx/hslibs/*) is not known to the
dynamic linker, then the target library must be patched.

```ShellSession
$ hslibdeps -d hslibs -t /var/lib/nginx/hslibs ngx_haskell.so
```

To skip collecting dependent libraries, use option *-p* (patch-only).

```ShellSession
$ hslibdeps -p -t /var/lib/nginx/hslibs ngx_haskell.so
```

With option *-e*, print all direct dependencies found by
[*cabal-plan*](https://hackage.haskell.org/package/cabal-plan) to *stdout*.

```ShellSession
$ hslibdeps -e
```

The output is compatible with format of *GHC environment* files.

### nhm-init

Bootstraps environment to build custom Haskell handlers. Running

```ShellSession
$ nhm-init project-name
```

produces files *cabal.project*, *Setup.hs*, *project-name.cabal*, and
*Makefile*. If any of the files exist, add option *-f* to override them. This
set of files implements approach outlined
[*here*](../haskell/ngx-export-distribution#building-dependencies-with-cabal-v2-build).

By default, the target library will be linked against the threaded Haskell RTS
library. To link against the base RTS library, add option *-no-threaded*.

The target library will be installed in directory */var/lib/nginx*. Use option
*-p prefix* to override the install directory.

Note that the root Haskell source file must be *project_name.hs*
where *project_name* is *project-name* with all dashes replaced by underscores.
If the source file depends on packages other than *base* and *ngx-export*, add
them into *project-name.cabal* manually.

After bootstrapping the environment, make sure that Cabal,
[*patchelf*](https://github.com/NixOS/patchelf),
[*hslibdeps*](#utility-hslibdeps), and
[*cabal-plan*](https://hackage.haskell.org/package/cabal-plan) are available in
the system, and then build and install the target library.

```ShellSession
$ make
$ sudo make install
```

With ghc older than *8.10.6*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

