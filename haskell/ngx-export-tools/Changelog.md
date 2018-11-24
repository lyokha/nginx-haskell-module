### 0.3.1.0

- Names of the storages for custom types now contain the name of the service
  which makes it possible to use the same configuration type in multiple
  services.

### 0.3.0.0

- Added readers of custom types consuming the Nginx request pointer at first.
- Extended docs and examples.

### 0.2.1.1

- Fixed examples of simple services handlers.

### 0.2.1.0

- Derive *TimeInterval* from *Show*.
- Extended docs and examples.

### 0.2.0.0

- Added functions *readFromByteString* and *readFromByteStringAsJSON*.
- Many bug fixes in the builder of simple services.

### 0.1.2.0

- Added function *ngxRequestPtr*.

### 0.1.1.0

- Re-export *CInt* and *CUInt* like in module *NgxExport* instead of
  *unsafePerformIO*.

### 0.1.0.0

- Initial version.

