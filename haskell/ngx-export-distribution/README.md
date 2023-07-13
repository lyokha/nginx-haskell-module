#### Module *NgxExport.Distribution*

[![Hackage](https://img.shields.io/hackage/v/ngx-export-distribution.svg?label=hackage%20%7C%20ngx-export-distribution&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/ngx-export-distribution)

This module allows for building simple shared libraries with Cabal.

###### An example

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
                           -package=base
                           -package=ngx-export
                           -package=bytestring
                           -package=aeson
```

All packages listed in *build-depends* get also wrapped inside options
*-package* in *ghc-options*: this is important when building them with
*cabal v2-build* and then using inside GHC *package environments*. However,
this duplication can be avoided if there is a method to get the package list
in the *ghc-options* programmatically. One of such methods is based on
collecting the direct dependencies with utility *cabal-plan*.

###### File *Setup.hs*

```haskell
import NgxExport.Distribution
main = defaultMain
```

The configuration step requires that utilities *patchelf* and
[hslibdeps](../../utils/README.md) were found in the paths of environment
variable *PATH*.

Building is a bit cumbersome: it expects explicit option *--prefix* at the
configuration step (which will be interpreted as the prefix part of the
*rpath* by utility *hslibdeps*) and explicit ghc option *-o* at the build
step which is as well used by *hslibdeps* as the name of the target library.

###### Building with cabal v1-commands

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
[2 of 2] Linking ./dist/setup/setup ...
Configuring ngx-distribution-test-0.1.0.0...
```

```ShellSession
$ cabal v1-build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
[1 of 2] Compiling NgxDistributionTest ( ngx_distribution_test.hs, ngx_distribution_test.o )
[2 of 2] Linking ngx_distribution_test.so ...
---> Collecting libraries
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSngx-export-1.7.5-FkCfFIq2kiq6MpFtZt6Wso-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSngx-export-1.7.5-FkCfFIq2kiq6MpFtZt6Wso-ghc9.4.1.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSmonad-loops-0.4.3-5HNgusEuKV7E9KDl2xfIIb-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSmonad-loops-0.4.3-5HNgusEuKV7E9KDl2xfIIb-ghc9.4.1.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSasync-2.2.4-BHmUTH2SmtgLLoxIOXNoMc-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSasync-2.2.4-BHmUTH2SmtgLLoxIOXNoMc-ghc9.4.1.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSaeson-2.1.0.0-79sgaqQ0msAJaL6HuNRLaK-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSaeson-2.1.0.0-79sgaqQ0msAJaL6HuNRLaK-ghc9.4.1.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSwitherable-0.4.2-1AWCu2zvFImLTaoXk8CRkT-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSwitherable-0.4.2-1AWCu2zvFImLTaoXk8CRkT-ghc9.4.1.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1/libHSuuid-types-1.0.5-BduubbeXxFCF9me5IkbXLU-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSuuid-types-1.0.5-BduubbeXxFCF9me5IkbXLU-ghc9.4.1.so'

   ...

'/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1/libHSghc-bignum-1.3-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSghc-bignum-1.3-ghc9.4.1.so'
'/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1/libHSghc-prim-0.9.0-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSghc-prim-0.9.0-ghc9.4.1.so'
'/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1/libHSrts-1.0.2_thr-ghc9.4.1.so' -> 'x86_64-linux-ghc-9.4.1/libHSrts-1.0.2_thr-ghc9.4.1.so'

---> Patching ngx_distribution_test.so
/var/lib/nginx/x86_64-linux-ghc-9.4.1:/home/lyokha/.cabal/lib/x86_64-linux-ghc-9.4.1:/usr/lib64/ghc-9.4.1/lib/../lib/x86_64-linux-ghc-9.4.1

---> Archiving artifacts
ngx_distribution_test.so
x86_64-linux-ghc-9.4.1/
x86_64-linux-ghc-9.4.1/libHSrts-1.0.2_thr-ghc9.4.1.so
x86_64-linux-ghc-9.4.1/libHSwitherable-0.4.2-1AWCu2zvFImLTaoXk8CRkT-ghc9.4.1.so
x86_64-linux-ghc-9.4.1/libHSsplitmix-0.1.0.4-HUWpFUIlsWJ8kN1EGcaWa2-ghc9.4.1.so
x86_64-linux-ghc-9.4.1/libHSscientific-0.3.7.0-C7AyvjqJeHGGsm9gk5kZlS-ghc9.4.1.so

   ...

x86_64-linux-ghc-9.4.1/libHSunix-2.7.3-ghc9.4.1.so
x86_64-linux-ghc-9.4.1/libHSpretty-1.1.3.6-ghc9.4.1.so
x86_64-linux-ghc-9.4.1/libHSdeepseq-1.4.8.0-ghc9.4.1.so
```

Notes about the value of *--ghc-options* in command *cabal v1-build*.

- In ghc older than *8.10.6*, option *-threaded* must be replaced with option
  *-lHSrts_thr-ghc&dollar;(ghc --numeric-version)* because ghc option
  *-flink-rts*, which is passed by the module internally, has first appeared in
  the said release,
- clause *ghc-options* in the Cabal file is a better place for such a generic
  option as *-threaded*.
- if the base name of the source file (*__ngx_distribution_test__.hs*) had
  exactly matched the package name (*__ngx-distribution-test__*), then
  options *ngx_distribution_test.hs -o ngx_distribution_test.so* could have
  been omitted.

Now the current working directory contains new files
*ngx_distribution_test.so* and *ngx-distribution-test-0.1.0.0.tar.gz* and a
new directory *x86_64-linux-ghc-9.4.1*. The tar-file contains the patched
shared library and the directory with dependent libraries: it is ready for
installation in directory */var/lib/nginx* at the target system.

###### Building with Setup.hs commands

For building custom artifacts, options of *hslibdeps* must be accessed
directly. For this, commands *runhaskell Setup.hs configure / build* can be
used instead of *cabal v1-configure / v1-build*. Let's change the names of
the directory with dependent libraries and the tar-file to *deps/* and
*deps.tar.gz* respectively, and also define the *rpath* directory without
using option *--prefix*.

```ShellSession
$ runhaskell Setup.hs configure --user
```

```ShellSession
$ runhaskell Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded" --hslibdeps-options="-t/var/lib/nginx/deps -ddeps -adeps"
```

###### Building dependencies with cabal v2-build

Nowadays, Cabal recommends building packages using *Nix-style local builds*.
This means that dependent packages do not get installed in places known to
GHC. However, they can be built inside GHC *package environments*. Let's
build dependencies and put them in a package environment in the current
working directory.

```ShellSession
$ cabal v2-install --lib --only-dependencies --package-env .
```

```ShellSession
$ cabal v2-install --lib ngx-export-distribution --package-env .
```

```ShellSession
$ sed -i 's/\(^package-id \)/--\1/' .ghc.environment.x86_64-linux-$(ghc --numeric-version)
```

This *sed* command comments out all lines that start with word *package-id*
in file *.ghc.environment.x86_64-linux-9.4.1* which has been created by the
former commands. This prevents the target library from linking against
libraries belonging to packages listed in those lines, thus making the
overall number and the size of dependent libraries as small as possible. If
this command breaks the following steps, some of the commented lines can be
selectively uncommented.

```ShellSession
$ ADD_CABAL_STORE=$(sed -n 's/^\(package-db\)\s\+/--\1=/p' .ghc.environment.x86_64-linux-$(ghc --numeric-version))
$ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs configure --package-db=clear --package-db=global $ADD_CABAL_STORE --prefix=/var/lib/nginx
```

Shell variable *&dollar;ADD_CABAL_STORE* wraps all *package-db* records found in
the GHC environment file into the list of options suitable for passing to the
*configure* command. Normally, this list shall contain only one directory
*&dollar;HOME/.cabal/store/ghc-&dollar;(ghc --numeric-version)/package.db* with
all packages ever built by *cabal v2-build*.

If the direct dependencies were not listed in the Cabal file, they must be
collected inside the GHC environment file.

```ShellSession
$ . cabal-plan-direct-deps.sh >> .ghc.environment.x86_64-linux-$(ghc --numeric-version)
```

See details about collecting direct dependencies in the next section.

```ShellSession
$ runhaskell --ghc-arg=-package=base --ghc-arg=-package=ngx-export-distribution Setup.hs build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so -threaded"
```

This should build library *ngx_distribution_test.so* and link it against
Haskell libraries found in the global package db and the Cabal's global
package store.

###### Collecting direct dependencies with cabal-plan

We listed build dependencies in both *build-depends* and *ghc-options*
clauses in the Cabal file to let Cabal find dependencies built with
*cabal v2-build* at the *configure* step and expose them to ghc at the
*build* step. This approach is tedious and error-prone. Fortunately, there is
utility [cabal-plan](https://hackage.haskell.org/package/cabal-plan) which is
aimed to figure out dependencies of built packages. Particularly, with
*cabal-plan* we can remove those *-package=...* lines from the *ghc-options*
clause in the Cabal file and, instead, collect the direct dependencies
programmatically and put them as *package-id* records in the GHC environment
file.

The following bash script collects all direct dependencies reported by
*cabal-plan*.

###### File *cabal-plan-direct-deps.sh*

```bash
#!/usr/bin/env bash

CABAL_PLAN=$(cabal-plan info --ascii)
UNIT_ID="^UnitId\s\+\""
while IFS= read -r pkg
do sed -n "/$UNIT_ID$pkg/s/$UNIT_ID\(.*\)\"\$/package-id \1/p" <<< "$CABAL_PLAN"
done < <(sed -n '/^CompNameLib$/,/^$/s/^\s\+//p' <<< "$CABAL_PLAN")
unset CABAL_PLAN UNIT_ID
```

After running this as

```ShellSession
$ . cabal-plan-direct-deps.sh >> .ghc.environment.x86_64-linux-$(ghc --numeric-version)
```

four lines looking similar to

```
package-id aeson-2.1.0.0-9b19e87ee2a82567866c50e13806427068fd4bcc78cedb01ecad7389791f6761
package-id base-4.17.0.0
package-id bytestring-0.11.3.1
package-id ngx-export-1.7.5-17b83e3ac354cc52614227ba662f8c23a8ddd4e08f2a1a02b0d6b51b2dd849ea
```

will appear at the end of file *.ghc.environment.x86_64-linux-9.4.1*. This
shall expose the four dependent packages at the *build* step.

###### Drawbacks

With all the building approaches shown above, the following list of drawbacks
must be taken into account.

- Utility *hslibdeps* collects only libraries prefixed with *libHS*,
- clean commands such as *cabal v1-clean* do not delete build artifacts in
  the current working directory,
- behavior of Cabal commands other than *configure*, *build* and *clean* is
  not well defined.

