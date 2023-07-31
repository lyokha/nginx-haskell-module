#### Utility hslibdeps

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

