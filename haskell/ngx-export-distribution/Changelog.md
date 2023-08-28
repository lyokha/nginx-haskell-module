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

