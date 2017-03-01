See details
[*here*](http://lin-techdet.blogspot.com/2017/01/nginx-haskell-module-labeled-media.html).

Additional remarks.

- Nginx directive *error_page* inevitably substitutes *HTTP* *POST* method for
  *GET*. This is basically not acceptable for *write* requests as they
  presumably must be *POSTed*. Fortunately, original *HTTP* methods can be
  restored with directive *proxy_method* when needed.

- The *Read*/*Show* serialization that used in *getMsg* proved to be very slow
  due to expensive *ByteString* to *String* unpacking. *JSON* serialization
  using *Aeson* should have much better performance.
