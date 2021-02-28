See details
[*here*](http://lin-techdet.blogspot.com/2017/01/nginx-haskell-module-labeled-media.html).

Additional remarks.

- Nginx directive *error_page* inevitably substitutes *HTTP* *POST* method for
  *GET*. This is basically not acceptable for *write* requests as they
  presumably must be *POSTed*. Fortunately, original *HTTP* methods can be
  restored with directive *proxy_method* when needed.

- The *Read*/*Show* serialization that used in *getMsg* proved to be very slow
  due to expensive *ByteString* to *String* unpacking when *reading* into data.
  *JSON* serialization using *Aeson* should have much better performance.
  [*Here*](https://lyokha.github.io/nginx-haskell-module/examples/labeledMediaRouting/test/lmr-bench.html)
  is benchmarking results using Criterion.

