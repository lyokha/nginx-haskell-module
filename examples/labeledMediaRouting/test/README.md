#### Benchmarking various serialization approaches using Criterion

##### Build

```ShellSession
$ make -C.. bench
```

##### Run

```ShellSession
$ ./lmr-bench -o lmr-bench.html
```

#### Generating sample objects of various types in GHCi

```ShellSession
$ ghci -fobject-code lmr-objgen.hs ../lmr.hs
Loaded package environment from ...
GHCi, version 9.6.2: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from ...
Ok, two modules loaded
Prelude LMRObjGen> import LabeledMediaRouter
Prelude LabeledMediaRouter LMRObjGen> d <- genCollectedData 3
Prelude LabeledMediaRouter LMRObjGen> pPrint d
 ...
Prelude LabeledMediaRouter LMRObjGen> pPrintJSON d
 ...
Prelude LabeledMediaRouter LMRObjGen> pPrint $ toRoutes d
 ...
Prelude LabeledMediaRouter LMRObjGen> pPrintJSON $ toRoutes d
 ...
Prelude LabeledMediaRouter LMRObjGen> ld <- genGeneric 3 :: IO LabelData
Prelude LabeledMediaRouter LMRObjGen> pPrint ld
 ...
```

