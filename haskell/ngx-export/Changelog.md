### 1.7.9

- Reify types of internal handlers from their signatures.
- Dropped support for GHC versions older than *8.10* (due to *reifyType*).

### 1.7.8

- Refactored to avoid using *head* and *tail* as it triggers *x-partial*
  warnings in GHC *9.8.1*.

### 1.7.7.1

- Added a minimal example of using exporters.

### 1.7.7

- Cleanup declarations which still had explicit support for GHC *7.x*.

### 1.7.6

- Fancier, de-duplicated type annotations by leveraging type synonyms.
- Applied hints proposed by *hlint*.

### 1.7.5

- More robust pattern exhaustion check after inclusion of option
  *-Wincomplete-uni-patterns* into *-Wall* in GHC *9.2.1*.

### 1.7.4

- Function *safeWaitToSetLock* was moved into a new internal module
  *NgxExport.Internal.SafeFileLock*.
- Implemented run-time choice of the best available file-lock implementation
  (i.e. standard POSIX *F_SETLKW* or *F_OFD_SETLKW* which is a better choice as
  it does not involve deadlock detection and thus better matches our purposes).
- Catch *EDEADLK* inside *safeWaitToSetLock* to make file-locks with *F_SETLKW*
  behave more nicely.
- Calculate positions of fields in *struct flock* correctly using *hsc2hs*
  directives.

### 1.7.3

- Further fixes for inactive shared services waiting on file-locks.

### 1.7.2

- More robust masking in async handlers. As this uses function *interruptible*
  from module *Control.Exception*, support of GHC versions *7.x* was dropped.
- Fixed waiting on file-locks of inactive shared services.

### 1.7.1

- Added function *ngxCachedPid* to return the PID of the current worker process
  cached in Nginx.

### 1.7.0

- Now basic synchronous and all asynchronous content handlers are expected to
  return a list of custom response headers in the *4th* field of the
  *ContentHandlerResult*.
- All asynchronous handlers get proper masking against asynchronous exceptions,
  which is especially important for service handlers in presence of asynchronous
  exceptions from service hooks.
- Now a new StablePtr for the content type gets returned only when its value is
  not empty.

### 1.6.4

- Added exception *WorkerProcessIsExiting* to signal services that Nginx is
  shutting down. This has replaced *ThreadKilled* that was used formerly in
  order to prevent erroneous handling of unexpectedly thrown *ThreadKilled*.
- All API exceptions now derive *Eq*.

### 1.6.3

- Added API exceptions *RestartWorkerProcess* and *FinalizeHTTPRequest*.

### 1.6.2

- Added exception *TerminateWorkerProcess* that can be used for a little bit
  more graceful termination of the worker process than bare exit(2).

### 1.6.1

- Upgraded Cabal version constraint.

### 1.6.0

- Implemented type and role disambiguation helper to prevent segfaults in
  Nginx when a handler of an appropriate type (say, IOYY with a role of a
  synchronous handler) gets used in a wrong role (say, as an async handler).

### 1.5.0

- Services are now terminated with *cancelWith ThreadKilled* because *cancel*
  throws *AsyncCancelled* starting from *async-2.2*.

### 1.4.2

- Minor internal improvements (using tuple sections).

### 1.4.1

- Corrected type of *ngxCachedTimePtr*.

### 1.4.0

- Added a number of opaque pointers to Nginx global objects for using them in
  C plugins.

### 1.3.0

- Export function *ngxExportInstallSignalHandler* to ignore SIGINT.

### 1.2.2

- Returning a special error code (*2*) on exception *ServiceHookInterrupt*.

### 1.2.1

- Bugfix: put *unsigned* integers in event channels (important for *eventfd*).

### 1.2.0

- Added exporter *ngxExportServiceHook* for service hooks and exception
  *ServiceHookInterrupt* for interrupting services.

### 1.1.0

- Added an asynchronous content handler with direct access to request body
  chunks exported with *ngxExportAsyncHandlerOnReqBody*.
- Reading buffered request body from a temporary file when it's provided.

### 1.0.1

- Using deepseq for really deep evaluation of resulting lazy bytestrings to
  prevent leaks of exceptions outside of protected code.

### 1.0

- Added an asynchronous content handler exported with *ngxExportAsyncHandler*.
- Content type in all content handlers gets packed in a strict bytestring now,
  which allows for using static strings without dynamic memory allocation.
- Package stability tag was promoted to stable.

### 0.9.1.1

- yY handler must be strict against exceptions in safeYYHandler.

### 0.9.1.0

- Do not write into service event channel when Nginx is exiting.

### 0.9.0.0

- Using interruptible FFI for safeWaitToSetLock to re-enable graceful shutdown
  with hs_exit() for shared services.

### 0.8.1.0

- Function waitSetToLock from package *unix* calls C fcntl() unsafely which
  caused hangs of the whole haskell RTS when it was waiting for lock release.
  So this functions was reimplemented using *safe* semantics as
  safeWaitToSetLock.
- Exported function ngxExportReleaseLockedByteString was removed because Nginx
  can call hs_free_stable_ptr() directly.

### 0.8.0.3

- A better solution for ghc warnings set.

### 0.8.0.2

- Removed constraint on *template-haskell* via a CPP macro (this should repair
  compilation for *ghc-7.10*).

### 0.8.0.0

- Implemented shared services using waitToSetLock to get exclusive access for
  servicing.
- New dependency on package *monad-loops* (>= 0.4.2).

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

