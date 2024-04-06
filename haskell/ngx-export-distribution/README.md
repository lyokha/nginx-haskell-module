#### Module *NgxExport.Distribution*

[![Hackage](https://img.shields.io/hackage/v/ngx-export-distribution.svg?label=hackage%20%7C%20ngx-export-distribution&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/ngx-export-distribution)

This module allows for building regular shared libraries and collecting
Haskell libraries they depend on with Cabal.

##### An example

###### File *ngx_distribution_test.hs*

```haskell
{-# LANGUAGE TemplateHaskell #-}

module NgxDistributionTest where

import           NgxExport

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Data.Maybe

incCnt :: ByteString -> C8L.ByteString
incCnt = C8L.pack . show . succ . fromMaybe (0 :: Int) . decodeStrict
ngxExportYY 'incCnt
```

###### File *ngx-distribution-test.cabal*

```Cabal
name:                       ngx-distribution-test
version:                    0.1.0.0
build-type:                 Custom
cabal-version:              1.24

custom-setup
  setup-depends:            base >= 4.8 && < 5
                          , ngx-export-distribution

library
  default-language:         Haskell2010
  build-depends:            base >= 4.8 && < 5
                          , ngx-export
                          , bytestring
                          , aeson

  ghc-options:             -Wall -O2 -no-keep-hi-files -no-keep-o-files
```

###### File *Setup.hs*

```haskell
import NgxExport.Distribution
main = defaultMain
```

The configuration step requires that utilities *nhm-tool* and *patchelf* were
found in the paths of environment variable *PATH*. The *nhm-tool* is packaged
with this module and can be installed by running

```ShellSession
$ cabal install
```

from the root directory of the module or by running

```ShellSession
$ cabal install ngx-export-distribution
```

from any other directory, in which case it will be installed from *Hackage*.

Building is a bit cumbersome: it expects explicit option *--prefix* at the
configuration step (which will be interpreted as the prefix part of the
*rpath* by *nhm-tool dist*) and explicit ghc option *-o* at the build
step which is as well used by *nhm-tool dist* as the name of the target library.
To avoid complexity, bootstrap the project with *nhm-tool init*.

##### Building with cabal v1-commands

Let's build the example with commands *cabal v1-configure* and
*cabal v1-build*.

```ShellSession
$ cabal v1-install --only-dependencies
Resolving dependencies...
All the requested packages are already installed:
Use --reinstall if you want to reinstall anyway.
```

```ShellSession
$ cabal v1-configure --prefix=/var/lib/nginx
Resolving dependencies...
[1 of 2] Compiling Main             ( dist/setup/setup.hs, dist/setup/Main.o )
[2 of 2] Linking ./dist/setup/setup
Configuring ngx-distribution-test-0.1.0.0...
```

```ShellSession
$ cabal v1-build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
[1 of 2] Compiling NgxDistributionTest ( ngx_distribution_test.hs, ngx_distribution_test.o )
[2 of 2] Linking ngx_distribution_test.so
---> Collecting libraries
/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSOneTuple-0.4.1.1-GTlScb3X0Hn7Y4A5VTBpq8-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSOneTuple-0.4.1.1-GTlScb3X0Hn7Y4A5VTBpq8-ghc9.6.2.so
/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSQuickCheck-2.14.3-FxURKqK1tk15J8arEBmUtc-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSQuickCheck-2.14.3-FxURKqK1tk15J8arEBmUtc-ghc9.6.2.so
/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSStateVar-1.2.2-F3B0sJlZ41353sDhwwFm1B-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSStateVar-1.2.2-F3B0sJlZ41353sDhwwFm1B-ghc9.6.2.so
/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSaeson-2.2.0.0-KcH800TS6us8tZ6AZDtIQh-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSaeson-2.2.0.0-KcH800TS6us8tZ6AZDtIQh-ghc9.6.2.so
/usr/lib64/ghc-9.6.2/lib/../lib/x86_64-linux-ghc-9.6.2/libHSarray-0.5.5.0-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSarray-0.5.5.0-ghc9.6.2.so
/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSassoc-1.1-5sFQqIOvFZQJX5kdLbEWB9-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSassoc-1.1-5sFQqIOvFZQJX5kdLbEWB9-ghc9.6.2.so

   ...

/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSvector-stream-0.1.0.0-Kd51wsO6Y2s1Z8znT4c6B5-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSvector-stream-0.1.0.0-Kd51wsO6Y2s1Z8znT4c6B5-ghc9.6.2.so
/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2/libHSwitherable-0.4.2-KGoJH5wjsMLKLjx4iVJYPg-ghc9.6.2.so -> x86_64-linux-ghc-9.6.2/libHSwitherable-0.4.2-KGoJH5wjsMLKLjx4iVJYPg-ghc9.6.2.so
/lib64/libffi.so.8 -> x86_64-linux-ghc-9.6.2/libffi.so.8

---> Patching ngx_distribution_test.so
/var/lib/nginx/x86_64-linux-ghc-9.6.2:/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.6.2:/usr/lib64/ghc-9.6.2/lib/../lib/x86_64-linux-ghc-9.6.2

---> Archiving artifacts
ngx_distribution_test.so
x86_64-linux-ghc-9.6.2/
x86_64-linux-ghc-9.6.2/libHSOneTuple-0.4.1.1-GTlScb3X0Hn7Y4A5VTBpq8-ghc9.6.2.so
x86_64-linux-ghc-9.6.2/libHSQuickCheck-2.14.3-FxURKqK1tk15J8arEBmUtc-ghc9.6.2.so
x86_64-linux-ghc-9.6.2/libHSStateVar-1.2.2-F3B0sJlZ41353sDhwwFm1B-ghc9.6.2.so
x86_64-linux-ghc-9.6.2/libHSaeson-2.2.0.0-KcH800TS6us8tZ6AZDtIQh-ghc9.6.2.so

   ...

x86_64-linux-ghc-9.6.2/libHSvector-stream-0.1.0.0-Kd51wsO6Y2s1Z8znT4c6B5-ghc9.6.2.so
x86_64-linux-ghc-9.6.2/libHSwitherable-0.4.2-KGoJH5wjsMLKLjx4iVJYPg-ghc9.6.2.so
x86_64-linux-ghc-9.6.2/libffi.so.8
```

Notes about the value of *--ghc-options* in command *cabal v1-build*.

- In ghc older than *9.0.1*, option *-threaded* must be replaced with option
  *-lHSrts_thr-ghc&dollar;(ghc --numeric-version)* because ghc option
  *-flink-rts*, which is passed by the module internally, has first appeared in
  the said release,
- clause *ghc-options* in the Cabal file is a better place for such a generic
  option as *-threaded*,
- if the base name of the source file (*__ngx_distribution_test__.hs*) had
  exactly matched the package name (*__ngx-distribution-test__*), then
  options *ngx_distribution_test.hs -o ngx_distribution_test.so* could have
  been omitted.

Now the current working directory contains new files
*ngx_distribution_test.so* and *ngx-distribution-test-0.1.0.0.tar.gz* and a
new directory *x86_64-linux-ghc-9.6.2*. The tar-file contains the patched
shared library and the directory with dependent libraries: it is ready for
installation in directory */var/lib/nginx* at the target system.

##### Building with Setup.hs commands

For building custom artifacts, options of *nhm-tool dist* must be accessed
directly. For this, commands *runhaskell Setup.hs configure / build* can be
used instead of *cabal v1-configure / v1-build*. Let's change the names of
the directory with dependent libraries and the tar-file to *deps/* and
*deps.tar.gz* respectively, and also define the *rpath* directory without
using option *--prefix*.

```ShellSession
$ runhaskell Setup.hs configure --user --nhm-tool-options="-t/var/lib/nginx/deps -ddeps -adeps"
```

Note that despite the name *--nhm-tool-options*, the specified options are
passed internally into a sub-command *nhm-tool dist*.

```ShellSession
$ runhaskell Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
```

##### Building dependencies with cabal v2-build

Nowadays, Cabal recommends building packages as *Nix-style local builds*.
This means that dependent packages do not get installed in places known to
GHC. However, they can be built inside GHC *package environments*. Let's
build dependencies and put them in a package environment in the current
working directory.

```ShellSession
$ cabal install --lib --only-dependencies --package-env .
```

This command creates both the package environment and the *build plan*. We must
tune the package environment by replacing existing *package-id* records with
precise direct dependencies of the target library. With
[cabal-plan](https://hackage.haskell.org/package/cabal-plan), finding the
direct dependencies in the cabal build plan is easy.

```ShellSession
$ sed -i 's/\(^package-id \)/--\1/' .ghc.environment.x86_64-linux-$(ghc --numeric-version)
```

This *sed* command comments out all lines that start with word *package-id*
in file *.ghc.environment.x86_64-linux-9.6.2*.

```ShellSession
$ nhm-tool deps ngx-distribution-test >> .ghc.environment.x86_64-linux-$(ghc --numeric-version)
```

Command *nhm-tool deps* builds around the code of the *cabal-plan* library.
After running this, four lines looking similar to

```
package-id aeson-2.2.0.0-711db3f5b99af756f1eae54020c04616c024ecab6013b0b2140b60e4c06a6e9d
package-id base-4.18.0.0
package-id bytestring-0.11.4.0
package-id ngx-export-1.7.7.1-7f7a3d21f396899b6466d425218188ba097f7cc49638994748bb4e4828d9e354
```

will appear at the end of file *.ghc.environment.x86_64-linux-9.6.2*. This
shall expose the four dependent packages at the next steps.

```ShellSession
$ ADD_CABAL_STORE=$(sed -n 's/^\(package-db\)\s\+/--\1=/p' .ghc.environment.x86_64-linux-$(ghc --numeric-version))
$ ADD_DIRECT_DEPS=$(sed -n 's/^package-id\s\+\(.*\)\(-\([0-9]\+\.\)*[0-9]\+\($\|-.*\)\)/--dependency=\1=\1\2/p' .ghc.environment.x86_64-linux-$(ghc --numeric-version))
$ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs configure --package-db=clear --package-db=global $ADD_CABAL_STORE $ADD_DIRECT_DEPS --prefix=/var/lib/nginx
```

Shell variable *&dollar;ADD_CABAL_STORE* wraps all *package-db* records found in
the GHC environment file into the list of options suitable for passing to the
*configure* command. Normally, this list shall contain only one directory
*&dollar;HOME/.cabal/store/ghc-&dollar;(ghc --numeric-version)/package.db* with
all packages ever built by *cabal v2-build*. Variable *&dollar;ADD_DIRECT_DEPS*
does similar job with *package-id* records.

```ShellSession
$ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
```

This should build library *ngx_distribution_test.so* and link it against
Haskell libraries found in the global package database and the Cabal package
store.

##### Bootstrapping a new project

All the steps listed in the previous section can be automated. This is exactly
what *nhm-tool init* does. Running

```ShellSession
$ nhm-tool init project-name
```

produces files *cabal.project*, *Setup.hs*, *project-name.cabal*, *Makefile*,
and *project_name.hs*. If any of the former four files exist, add option *-f*
to override them.

Note that the root Haskell source file is *project_name.hs* where *project_name*
is *project-name* with all dashes replaced by underscores. If the source code
will depend on packages other than *base* and *ngx-export*, add them into
*project-name.cabal* manually.

By default, the target library will be linked against the threaded Haskell RTS
library. To link against the base RTS library, add option *-no-threaded*.

The target library will be installed in directory */var/lib/nginx*. Use option
*-p prefix* to override the install directory.

Build and install the target library.

```ShellSession
$ make
$ sudo make install
```

With ghc older than *9.0.1*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

To delete intermediate build files, run

```ShellSession
$ make clean
```

or, to additionally delete the built artifact,

```ShellSession
$ make clean-all
```

##### Drawbacks

With all the building approaches shown above, the following list of drawbacks
must be taken into account.

- Utility *nhm-tool* collects only libraries prefixed with *libHS* or
  *libffi.so*,
- clean commands such as *cabal v1-clean* do not delete build artifacts in
  the current working directory,
- behavior of Cabal commands other than *configure*, *build* and *clean* is
  not well defined.

