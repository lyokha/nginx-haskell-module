### 0.7.0.0

Further optimizations.

- Poke single-chunked lazy bytestrings directly into passed from Nginx buffers.
- Specialize polymorphic functions and inline trivial functions.

### 0.6.1.1

- Added *-Wno-incomplete-patterns* in *ghc-options* for older ghc versions.

### 0.6.1.0

- Constructors of *NgxExport* were lifted to type level for all handlers to
  mitigate warnings on incomplete patterns.
- *COMPLETE* pragmas for pattern synonyms were added for the same reason.
- Other warnings were fixed too. Now the module builds with *ghc-options*
  *-Wall -Wno-unrecognised-pragmas*.

### 0.6.0.0

- Lazy bytestrings contents are no longer copied when passed back to Nginx.
  Instead, they are passed directly along with a StablePtr to original
  bytestrings. This must improve performance and lower memory consumption for
  content handlers with large outputs.

### 0.5.0.0

- All synchronous variable and content handlers were made exception safe.
- Bugfix: evaluate results of IO handlers strictly to make sure that all
  exceptions will be caught.

### 0.4.1.0

- Bugfix: keep calling *fdWriteBuf* until it writes complete buffer.

### 0.4.0.0

- All asynchronous handlers can now report events via eventfd channels.
- New dependency on package *binary* (>= 0.8.1.0).

### 0.3.2.2

- Stricter constraints for packages *base* (>= 4.8 && < 5) and
  *template-haskell* (>= 2.11.0.0).

### 0.3.2.0

- Versioning security: get number of required version parts from C code.

### 0.3.1.0

- Added versioning support to test for compatibility in C code.

### 0.3.0.0

- Added an asynchronous client request body handler exported with
  *ngxExportAsyncOnReqBody* for using in a new Nginx directive
  *haskell_run_async_on_request_body*.

### 0.2.5.1

- Added signatures for type-checkers and exporters to prevent warnings when
  reifying in a user code with *-Wmissing-signatures* or *-Wall* enabled.

### 0.2.5.0

- Added a synchronous handler in IO Monad exported with *ngxExportIOYY*.

