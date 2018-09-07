#### Benchmarking various serialization approaches using Criterion

##### Compile

```ShellSession
$ ghc --make -O2 lmr-bench.hs ../lmr.hs
```

##### Run

```ShellSession
$ ./lmr-bench -o lmr-bench.html
```

#### Generating sample objects of various types in GHCi

```ShellSession
$ ghci -fobject-code lmr-objgen.hs ../lmr.hs
GHCi, version 8.4.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/lyokha/.ghci
[1 of 2] Compiling LabeledMediaRouter ( ../lmr.hs, ../lmr.o )
[2 of 2] Compiling LMRObjGen        ( lmr-objgen.hs, lmr-objgen.o )
Ok, two modules loaded.
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

