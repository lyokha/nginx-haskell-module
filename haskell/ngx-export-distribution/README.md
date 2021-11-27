#### Module *NgxExport.Distribution*

This module allows for building simple shared libraries with Cabal.

Below is a simple example.

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

The configuration step requires that utilities *patchelf* and
[hslibdeps](../../utils/README.md) were found in the paths of environment
variable *PATH*.

Building is a bit cumbersome: it expects explicit option *--prefix* at the
configuration step (which will be interpreted as the first part of the
*rpath* by utility *hslibdeps*), and requires explicit ghc option *-o* at
the build step which is as well used by *hslibdeps* as the name of the
target library.

Let's build the example with commands *cabal v1-configure* and
*cabal v1-build* (the *v2-* commands should probably work as well).

```ShellSession
$ cabal v1-configure --prefix=/var/lib/nginx
Resolving dependencies...
[1 of 1] Compiling Main             ( dist/setup/setup.hs, dist/setup/Main.o )
Linking ./dist/setup/setup ...
Configuring ngx-distribution-test-0.1.0.0...
```

```ShellSession
$ cabal v1-build --ghc-options="ngx_distribution_test.hs -o ngx_distribution_test.so"
[1 of 1] Compiling NgxDistributionTest ( ngx_distribution_test.hs, ngx_distribution_test.o )
Linking ngx_distribution_test.so ...
---> Collecting libraries
'/usr/lib64/libHSrts-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSrts-ghc8.10.5.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSngx-export-1.7.5-JzTEmHewqdC9gGi6rzcAtt-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSngx-export-1.7.5-JzTEmHewqdC9gGi6rzcAtt-ghc8.10.5.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSmonad-loops-0.4.3-8Lx5Hn3pTtO62yOPdPW77x-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSmonad-loops-0.4.3-8Lx5Hn3pTtO62yOPdPW77x-ghc8.10.5.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSasync-2.2.4-ENjuIeC23kaKyMVDRYThP3-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSasync-2.2.4-ENjuIeC23kaKyMVDRYThP3-ghc8.10.5.so'
'/usr/lib64/libHSstm-2.5.0.1-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSstm-2.5.0.1-ghc8.10.5.so'
'/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5/libHSaeson-1.5.6.0-6XeGmWHoO3vJYEUW5PXPgC-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSaeson-1.5.6.0-6XeGmWHoO3vJYEUW5PXPgC-ghc8.10.5.so'

   ...

'/usr/lib64/libHSbase-4.14.2.0-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSbase-4.14.2.0-ghc8.10.5.so'
'/usr/lib64/libHSinteger-gmp-1.0.3.0-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSinteger-gmp-1.0.3.0-ghc8.10.5.so'
'/usr/lib64/libHSghc-prim-0.6.1-ghc8.10.5.so' -> 'x86_64-linux-ghc-8.10.5/libHSghc-prim-0.6.1-ghc8.10.5.so'

---> Patching ngx_distribution_test.so
/var/lib/nginx/x86_64-linux-ghc-8.10.5:/home/lyokha/.cabal/lib/x86_64-linux-ghc-8.10.5:/usr/lib64:/usr/lib64/ghc-8.10.5/rts
```

Now the current working directory contains new files
*ngx_distribution_test.so* and *ngx-distribution-test-0.1.0.0.tar.gz* and a
new directory *x86_64-linux-ghc-8.10.5*. The tar-file contains the patched
shared library and the directory with dependent libraries: it is ready for
installation in directory */var/lib/nginx* at the target system.

With this building approach, the following list of drawbacks must be taken
into account.

- Utility *hslibdeps* collects only libraries prefixed with *libHS*,
- command *cabal v1-clean* only deletes directory *dist*, it does not delete
  build artifacts in the current working directory,
- behavior of Cabal commands other than *configure*, *build* and *clean* is
  not well defined.

