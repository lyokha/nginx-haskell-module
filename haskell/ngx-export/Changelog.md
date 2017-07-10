### 0.5.0.0

- All synchronous variable and content handlers were made exception safe.

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

