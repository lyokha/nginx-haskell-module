##### Compile

```ShellSession
$ ghc -O2 -dynamic -shared -fPIC -lHSrts_thr-ghc$(ghc --numeric-version) test_tools.hs -o test_tools.so -fforce-recomp
```

To see how *Template Haskell* declarations get instantiated, add option
*``-ddump-splices``*.

##### Install

Before installing *test_tools.so*, you may need to collect and install all
dependent Haskell libraries, and patch *test_tools.so* using utility
[*hslibdeps*](/utils/README.md#utility-hslibdeps).

```ShellSession
$ hslibdeps -t /var/lib/nginx/$(arch)-linux-ghc-$(ghc --numeric-version) test_tools.so
```

The name of the target directory is arbitrary: the only requirement is that it
must be accessible by the user of Nginx worker processes (i.e. *nginx* or
*nobody*).

Copy library *test_tools.so* into directory */var/lib/nginx/* (this must
correspond to the directory specified in Nginx directive *haskell load*) being
a superuser.

```ShellSession
# cp test_tools.so /var/lib/nginx
```

Then copy all dependent Haskell libraries into the target directory.

```ShellSession
# cp -v .hslibs/* /var/lib/nginx/$(arch)-linux-ghc-$(ghc --numeric-version)
```

