### 1.2.3.2

- Suggest the synchronous initialization hook in documentation on *voidService*.

### 1.2.3.1

- Stick to the original polymorphic type signature of *voidHandler'* to avoid
  breakage of existing dependent code.

### 1.2.3

- Added a void service *voidService* which helps to load global data from Nginx
  configurations in a more concise and declarative way.

### 1.2.2.1

- Use *NgxExportService* in declarations of split services.

### 1.2.2

- Added a new module *NgxExport.Tools.Types* which exports a new type
  *NgxExportService* for writing a fancier declarations of services.

### 1.2.1

- Added function *voidHandler'* in module *NgxExport.Tools.Combinators*.

### 1.2.0

- Added a new module *NgxExport.Tools.Combinators* which exports a new function
  *voidHandler* and the whole module *NgxExport.Tools.SplitService*.

### 1.1.0

- Use polymorphic return types in functions *terminateWorkerProcess*,
  *restartWorkerProcess*, and *finalizeHTTPRequest* which allows throwing the
  corresponding exceptions in any context.

### 1.0

- The whole package was split into five separate sub-modules.
- Package stability tag was promoted to stable.

### 0.4.9.0

- Exposed function *skipRPtr* which can be useful in many contexts.

### 0.4.8.0

- Correct implementation of *Eq* and *Ord* instances for *TimeInterval*.
- A new value *Unset* equal to *Sec 0* added in *TimeInterval*.

### 0.4.7.0

- Derive *TimeInterval* from *Eq*.

### 0.4.6.0

- Added function *workerProcessIsExiting* to check quickly in an exception
  handler whether a Haskell service has been interrupted because the worker
  process is exiting.

### 0.4.5.0

- Added function *ngxPid* to return the PID of the current worker process
  cached in Nginx.

### 0.4.4.0

- Using *WorkerProcessIsExiting* instead of *ThreadKilled* as required in
  *ngx-export 1.6.4*.

### 0.4.3.0

- Function *exitWorkerProcess* was renamed to *restartWorkerProcess*.
- A new function *finalizeHTTPRequest* was added.
- Functions *terminateWorkerProcess* and *restartWorkerProcess* were
  reimplemented using API exceptions from package *ngx-export 1.6.3*, and
  therefore now they can be effectively used only in Haskell services.

### 0.4.2.3

- Using *TypeApplications* instead of *PartialTypeSignatures* in examples.

### 0.4.2.0

- More improvements in code generation of simple services.
- Using *PartialTypeSignatures* in examples.

### 0.4.1.0

- Improvements in code generation of simple services.

### 0.4.0.0

- Now services with *SingleShotService* strategy run exactly two times during
  the lifetime of a worker process: this lets using them in initialization /
  cleanup flow. Accordingly, the *first-run* flag is passed to them again.
- Added a number of combinators to facilitate creation of specialized services.
  They allow distinguishing between *ignition* and *deferred* services.

### 0.3.3.0

- In case of unreadable configuration, terminate the worker process by throwing
  exception *TerminateWorkerProcess* which makes Nginx log the supplied message
  and exit. To use this exception, the constraint on the version of the package
  *ngx-export* was increased.

### 0.3.2.0

- Do not pass the *first-run* flag to simple services with the
  *SingleShotService* sleeping strategy.

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

