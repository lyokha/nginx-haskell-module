### 0.3.1.0

- added versioning support to test for compatibility in C code.

### 0.3.0.0

- added an asynchronous client request body handler exported with
  *ngxExportAsyncOnReqBody* for using in a new Nginx directive
  *haskell_run_async_on_request_body*.

### 0.2.5.1

- added signatures for type-checkers and exporters to prevent warnings when
  reifying in a user code with *-Wmissing-signatures* or *-Wall* enabled.

### 0.2.5.0

- added a synchronous handler in IO Monad exported with *ngxExportIOYY*.

