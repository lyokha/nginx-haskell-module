### 0.5.4.0

- *nhm-tool*: command *init* now generates file *hie.yaml* for using with
  *haskell-language-server* when editing the Haskell source file(s). The
  generated *Makefile* skips configuration of the GHC environment if this can
  be omitted.

### 0.5.3.2

- *nhm-tool*: actually apply changes from *0.5.3.1*, they were not applied due
  to an oversight.

### 0.5.3.1

- *nhm-tool*: pass option *--builddir="&dollar;(BUILDDIR)"* to command *cabal
  list-bin* in generated *Makefile*. This fixes builds when package
  *ngx-export-distribution* is being built inplace and *nhm-tool* hasn't been
  explicitly installed.

### 0.5.3.0

- *nhm-tool*: command *deps* now accepts option *-d* which expects the
  *builddir* where the build plan is located. Command *init* now generates
  *Makefile* which builds Cabal artifacts in directory *dist-nhm*. Parsing of
  command-line arguments was refactored.

### 0.5.2.2

- *nhm-tool*: command *init* generates *Makefile* which searches *nhm-tool* via
  *cabal list-bin ngx-export-distribution* if the tool cannot be found in known
  directories. Generated cabal files require *cabal-version 2.0* because now
  they use *build-tool-depends: ngx-export-distribution:nhm-tool* in the
  library stanza.

### 0.5.2.1

- Fix import modules after update to *Cabal* *3.10.3.0*.

### 0.5.2.0

- Docs: suggest using precise direct dependencies at the configure step. This
  should fix warnings *This package indirectly depends on multiple versions of
  the same package* which used to print out very long lists of inconsistencies.
  Additionally, this seems to speed up builds when there are multiple versions
  of dependent packages in the Cabal store.
- *nhm-tool*: implement the suggestion in *nhm-tool init*.

### 0.5.1.3

- Refactored to avoid using *head* and *tail* as it triggers *x-partial*
  warnings in GHC *9.8.1*.

### 0.5.1.2

- Parse *ldd* outputs of older formats where the separator may occur between the
  library and the address like in *linux-vdso.so.1 =>  (0x00007fffd33f2000)*.

### 0.5.1.1

- Collect *libffi* library unconditionally.

### 0.5.1.0

- Collect *libffi* library if it was shipped with GHC.

### 0.5.0.4

- *nhm-tool*: use GHC option *-flink-rts* only in GHC *9.x*.

### 0.5.0.3

- *nhm-tool*: replace dependency on *prettyprinter* by *ansi-terminal*.

### 0.5.0.2

- *nhm-tool*: get rid of dependency on package *extra* and other improvements.

### 0.5.0.1

- More detailed help for *nhm-tool*.

### 0.5.0.0

- Build executable *nhm-tool* to replace utilities *hslibdeps* and *nhm-init*.

### 0.4.1.0

- Test GHC version to figure out whether to use option *-flink-rts* in run-time.
- Note on where to get *hslibdeps* when build fails due to its unavailability.

### 0.4.0.0

- Get verbosity level for all program invocations from configuration and build
  flags.

### 0.3.2.4

- Improved docs.

### 0.3.2.3

- Fixed docs on using *cabal-plan*.

### 0.3.2.0

- Added docs on building with dependencies installed by *cabal v2-build*.

### 0.3.1.0

- Use option *-flink-rts* only with GHC supporting this (GHC *8.10.6* and
  newer).
- Archive artifacts using the new option *-a* of *hslibdeps*.
- Added docs on how to use options of *hslibdeps* directly.

### 0.3.0.0

- GHC option *-flink-rts* was added to the list of default GHC options.
- Add GHC options *&dollar;pkg.hs -o &dollar;pkg.so* when building without
  option *-o* specified.
- More robust naming of paths to build artifacts.

### 0.2.0.0

- More robust processing of build and configuration flags.
- Export several internal functions from the module.

### 0.1.1.0

- Support only Cabal versions >= *3.0.0.0*.

### 0.1.0.0

- Initial version.

