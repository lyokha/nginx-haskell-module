Static linkage against basic haskell libraries
----------------------------------------------

By default *ghc* is configured to link the built haskell module against dynamic
haskell libraries which means that basic haskell packages must have been
installed on the target machine even when directive *haskell load* only loads an
already compiled library. In principle *ghc* permits building an independent
all-in-one shared library with static linkage against other haskell libraries,
but unfortunately the system linker will likely fail if those have been compiled
without compilation flag *-fPIC* on vast majority of modern platforms, notably
on *GNU/Linux x86_64*.

Here I want to show how to build haskell code from
[test/tsung/nginx-static.conf](../../test/tsung/nginx-static.conf) into an
independent all-in-one shared library on *Fedora 23 x86_64* with *ghc 7.10.2*
installed from a [copr
repository](http://copr.fedorainfracloud.org/coprs/petersen/ghc-7.10.2/).

First of all, building of such a library is only possible from command-line as
soon as it requires tuning of some *ghc* options not available from directive
*haskell ghc_extra_options*. Therefore the haskell code from the configuration
file must be extracted in a separate source file, say
*NgxHaskellUserRuntime.hs*. Here it is.

```haskell
{-# LANGUAGE TemplateHaskell, MagicHash, ViewPatterns #-}

module NgxHaskellUserRuntime where

import           NgxExport
import           Data.FileEmbed
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Unsafe
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
import           Safe

fromFile (tailSafe . C8.unpack -> f) =
    case lookup f $(embedDir "/usr/local/webdata") of
        Just p  -> (p,                         text_plain, 200)
        Nothing -> (pack 14 "File not found"#, text_plain, 404)
    where pack l s = accursedUnutterablePerformIO $ unsafePackAddressLen l s
          text_plain = pack 10 "text/plain"#

ngxExportUnsafeHandler 'fromFile
```

(Notice that a new *LANGUAGE* pragma *TemplateHaskell* was added and the
backslash before the *'fromFile* on the last line was removed). To compile this
as an independent library all dependent libraries such as *rts*, *base*, *safe*,
*file-embed* and their sub-dependencies must be compiled with flag *-fPIC* and
archived in static libraries. This is not an easy task considering that we do
not aim to replace the whole system *ghc* and installed packages.

So let's start with *rts*. The *rts* is not a haskell package but rather a part
of *ghc*, therefore we have to retrieve *ghc* sources from a branch that
corresponds to the version of the system *ghc*.

```ShellSession
$ git clone -b ghc-7.10.2-release --recursive git://git.haskell.org/ghc.git ghc-7.10.2
```

Now *cd* to the source directory and perform the first usual steps.

```ShellSession
$ cd ghc-7.10.2
$ ./boot
$ ./configure
```

Here we are going to do a trick. Static FFI library must be compiled with
*-fPIC* but *ghc* seems to not have a hook for this, so we must put the option
into the *CFLAGS* declaration in *libffi/ghc.mk* manually.

```ShellSession
$ sed -i 's/CFLAGS="/&-fPIC /' libffi/ghc.mk
```

Now we are ready to compile *rts*.

```ShellSession
$ cd rts
$ make EXTRA_HC_OPTS=-fPIC
```

Making *rts* takes a long time. After it's done we can check that the built
static libraries contain relocations.

```ShellSession
$ readelf --relocs dist/build/libCffi.a | egrep '(GOT|PLT|JU?MP_SLOT)'
$ readelf --relocs dist/build/libHSrts.a | egrep '(GOT|PLT|JU?MP_SLOT)'
```

(This method was found [here](http://stackoverflow.com/a/1351771/5655455)). If
these commands have produced long outputs then the libraries are good. Now we
must put them in a directory that will be passed to *ghc* while compiling the
final library. Let the directory be located in a related to *ghc* system path.
The following commands must be executed with a superuser privileges.

```ShellSession
# mkdir $(ghc --print-libdir)/static-fpic
# cp -r dist/build/ $(ghc --print-libdir)/static-fpic/rts
```

*Template-haskell* must also be built from here as soon as ghc seems to apply
some magic when building it and I do not manage to get a compatible static
archive when building from the list of dependent libraries as shown hereinafter.
Make sure that version to build corresponds to that of the system
*template-haskell* package (it holds true for most cases)!

```ShellSession
$ cd ../libraries/template-haskell
$ make EXTRA_HC_OPTS=-fPIC
```

Wait a bit and then copy the built artifacts to the directory *static-fpic*
(being a superuser).

```ShellSession
# cp -r dist-install/build/ $(ghc --print-libdir)/static-fpic/template-haskell
```

Now let's turn to haskell packages and their dependencies. *Cd* to a new
directory and try to track down all dependencies and sub-dependencies of
packages that we're going to use: *base*, *file-embed*, *template-haskell* (only
dependencies, not itself), *bytestring*, *safe* and *ngx-export*. To see
versions and dependencies of the installed packages command *ghc-pkg field* can
be used. For example,

```ShellSession
$ ghc-pkg field base version,depends
version: 4.8.1.0
depends:
    builtin_rts ghc-prim-0.4.0.0-af16264bc80979d06e37ac63e3ba9a21
    integer-gmp-1.0.0.0-8e0f14d0262184533b417ca1f8b44482
$ ghc-pkg field bytestring version,depends
version: 0.10.6.0
depends:
    base-4.8.1.0-4f7206fd964c629946bb89db72c80011
    deepseq-1.4.1.1-8fb9688ae42216e388cee132aef3d148
    ghc-prim-0.4.0.0-af16264bc80979d06e37ac63e3ba9a21
    integer-gmp-1.0.0.0-8e0f14d0262184533b417ca1f8b44482
```

Package *base* in my system has version *4.8.1.0* and depends on packages
*ghc-prim* and *integer-gmp*, package *bytestring* has version *0.10.6.0* and
depends on packages *base*, *deepseq*, *ghc-prim* and *integer-gmp*. There could
be multiple *version* and *depends* clauses per single package: the safest way
to choose versions and dependencies is taking clauses with the latest version.
We must track dependencies down and collect all sub-dependencies recursively
(*deepseq* and its dependencies and sub-dependencies etc.). It looks boring and
I wish I knew an automatic way for such dependency tracking<sup>[1](#fn1)</sup>.
Finally the following list of libraries to build was collected (in an arbitrary
order): *ghc-prim*, *integer-gmp*, *deepseq*, *array*, *bytestring*,
*directory*, *filepath*, *file-embed*, *time*, *unix*, *pretty* and *safe* (I
excluded *base* and *ngx-export* from the list because they differ in the way
how they are built).

Let's first build and install package *base*<sup>[2](#fn2)</sup>.

```ShellSession
$ cabal get base-4.8.1.0
$ cd base-4.8.1.0
$ cabal configure --ghc-options=-fPIC -finteger-gmp2
$ cabal build
$ sudo cp -r dist/build $(ghc --print-libdir)/static-fpic/base
$ cd -
```

Then build and install the libraries from the dependency list shown above.

```ShellSession
$ export DEPPACKS=$(for p in ghc-prim integer-gmp deepseq array bytestring directory filepath file-embed time unix pretty safe ; do ghc-pkg field $p version | head -1 | cut -d' ' -f2 | sed "s/^/$p-/" ; done)
$ for p in $DEPPACKS ; do cabal get $p ; cd $p ; cabal configure --ghc-options=-fPIC ; cabal build ; cd - ; done
```

The next command requires a superuser privileges.

```ShellSession
# for p in $DEPPACKS ; do DEPDST=$(echo $p | sed 's/-\([0-9]\+\.\)*[0-9]\+$//') ; cp -r $p/dist/build $(ghc --print-libdir)/static-fpic/$DEPDST ; done
```

Now *cd* to the *ngx-export* source directory and do all the same.

```ShellSession
$ cd haskell/ngx-export
$ cabal configure --ghc-options=-fPIC
$ cabal build
$ sudo cp -r dist/build $(ghc --print-libdir)/static-fpic/ngx-export
```

At this moment all dependent libraries have been installed. Let's build
*ngx_haskell.so*.

```ShellSession
$ GHCSTATICLIBS=$(find $(ghc --print-libdir)/static-fpic -maxdepth 1 | sed 's/^/-L/')
$ ghc -O2 -shared -fPIC -o ngx_haskell.so $GHCSTATICLIBS -lHSrts -lCffi -lrt NgxHaskellUserRuntime.hs
[1 of 1] Compiling NgxHaskellUserRuntime ( NgxHaskellUserRuntime.hs, NgxHaskellUserRuntime.o )
ghc: panic! (the 'impossible' happened)
  (GHC version 7.10.2 for x86_64-unknown-linux):
	Loading archives not supported

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
```

Pull in the reins! Being unable to load static archives seems too restrictive,
especially when it is cheatable.

```ShellSession
$ ghc -O2 -shared -fPIC -o ngx_haskell.so $GHCSTATICLIBS NgxHaskellUserRuntime.hs
[1 of 1] Compiling NgxHaskellUserRuntime ( NgxHaskellUserRuntime.hs, NgxHaskellUserRuntime.o )
Linking ngx_haskell.so ...
/usr/bin/ld: NgxHaskellUserRuntime.o: relocation R_X86_64_PC32 against undefined symbol `stg_newMutVarzh' can not be used when making a shared object; recompile with -fPIC
/usr/bin/ld: final link failed: Bad value
collect2: error: ld returned 1 exit status
$ ghc -O2 -shared -fPIC -o ngx_haskell.so $GHCSTATICLIBS -lHSrts -lCffi -lrt NgxHaskellUserRuntime.hs
Linking ngx_haskell.so ...
```

The library was built. Check that *ngx_haskell.so* does not depend on shared
haskell libraries.

```ShellSession
$ ldd ngx_haskell.so
	linux-vdso.so.1 (0x00007ffca784d000)
	librt.so.1 => /lib64/librt.so.1 (0x00007f51e0681000)
	libutil.so.1 => /lib64/libutil.so.1 (0x00007f51e047d000)
	libdl.so.2 => /lib64/libdl.so.2 (0x00007f51e0279000)
	libpthread.so.0 => /lib64/libpthread.so.0 (0x00007f51e005c000)
	libgmp.so.10 => /lib64/libgmp.so.10 (0x00007f51dfde3000)
	libc.so.6 => /lib64/libc.so.6 (0x00007f51dfa22000)
	/lib64/ld-linux-x86-64.so.2 (0x000055e11c5c7000)
```

Yes, *ldd* shows only system *C* libraries. Install the library.

```ShellSession
$ cp ngx_haskell.so /tmp
```

Replace directive *haskell compile* in the configuration file with directive

```nginx
    haskell load /tmp/ngx_haskell.so;
```

and finally run nginx with haskell code inside but without external dependencies
on *ghc* and haskell libraries!

<br><hr><a name="fn1"><sup>**1**</sup></a>&nbsp; There is a way! As it was
suggested
[here](https://www.reddit.com/r/haskell/comments/4my2cn/a_story_of_how_i_built_static_haskell_libraries/d4047uz),
all dependencies can be extracted from a shared library with command *ldd*.
Let's first make a shared library with name, say *libtmp.so*.

```ShellSession
$ ghc -O2 -dynamic -shared -fPIC -lHSrts-ghc$(ghc --numeric-version) -o libtmp.so NgxHaskellUserRuntime.hs
```

Now we can extract the list of all dependencies in a variable, say *DEPS*.

```ShellSession
$ DEPS=$(ldd libtmp.so | sed -r '/^\s*libHS/!d; s/^\s*libHS//; /^(rts|base|ngx-export|template-haskell)-/d; s/^(\S+)-([0-9]+\.){2,}.*/\1/')
```

<a name="fn2"><sup>**2**</sup></a>&nbsp; When using the newer *ghc 8.0.1*,
*cabal configure* may require an additional option *``--ipid=$(ghc-pkg field
base id | head -1 | cut -d' ' -f2)``*. This also refers to building other
dependent libraries and *ngx-export*. Values of *ipid* must be extracted from
system packages because different values will cause loading of the system
packages in place of their built counterparts, and consequently symbol
relocation errors when linking the final library.

