There is a good [*Nginx module for dynamic
upstreams*](http://github.com/cubicdaiya/ngx_dynamic_upstream). It provides API
for dynamic changes of servers and their parameters in static server groups aka
upstreams. *Nginx Plus* has similar functionality. Imagine that we want alike
feature, however the server discovery must be run actively by Nginx itself: it
can poll a data provider synchronously using timeouts or be subscribed to
updates asynchronously. In this example the first synchronous approach is used.

Let the data provider respond on a request from our service with a *JSON* object
with upstreams containing plain lists of their servers. Plain lists of servers
means that we won't need to set specific weights and other parameters of the
servers. Below is an example of the *JSON* response.

```json
{
  "utest":
     ["127.0.0.1:8020"
     ,"127.0.0.1:8030"
     ,"127.0.0.1:8040"
     ],
  "utest_hash":
     ["127.0.0.1:8020"
     ,"127.0.0.1:8030"
     ,"127.0.0.1:8040"
     ]
}
```

When a haskell service receives new data it must update its service variable and
call a *callback* function that will make request to a location with a special
*upconf* handler that will read the variable and update all the affected
upstreams with the new list of servers.

The *upconf* module is mostly adopted from the *dynamic upstreams module* and
provides two directives.

- *upconf* ``$var`` &mdash; Installs a content handler which will read value of
  the ``$var`` and update the upstreams listed in it. Allowed on the *location*
  configuration level. It corresponds to directive *dynamic_upstream* from the
  dynamic upstream module but gets data from a variable.

- *upconf_hash* ``$var`` ``consistent`` &mdash; Allows using *hash* value for
  defining the target server. Allowed on the *upstream* configuration level.

As soon as *upconf* must parse a *JSON* object, the module requires an external
library [*jsmn*](http://github.com/zserge/jsmn) for that. The *hash*
functionality was mostly adopted from *the upstream hash module* which is
shipped with the standard Nginx distribution, however it was enabled with
special *checkers* for synchronization between worker processes.

The main configuration lays in [nginx-upconf.conf](nginx-upconf.conf). File
[nginx-upconf-backends.conf](nginx-upconf-backends.conf) contains the data
provider emulator and the backends. To test how it all works, the main
configuration and the backends must be run in separate nginx instances.

```ShellSession
# /usr/local/nginx/sbin/nginx -c /absolute/path/to/examples/dynamicUpstreams/nginx-upconf-backends.conf
# /usr/local/nginx/sbin/nginx -c /absolute/path/to/examples/dynamicUpstreams/nginx-upconf.conf
```

Run some *curl* tests.

```ShellSession
$ curl -s 'http://localhost:8010/'
[30890] In 8020
$ curl -s 'http://localhost:8010/'
[30890] In 8030
$ curl -s 'http://localhost:8010/'
[30890] In 8040
$ curl -s 'http://localhost:8010/hash?$a=hello'
[30890] In 8030
$ curl -s 'http://localhost:8010/hash?$a=hello'
[30890] In 8030
$ curl -s 'http://localhost:8010/hash?$a=hello'
[30890] In 8030
```

Then you can add or remove the backends in *nginx-upconf-backends.conf* and
change the *JSON* value in *location /data* from server *data*, restart the
corresponding *backends* nginx instance,

```ShellSession
# pkill -HUP -f '/usr/local/nginx/sbin/nginx.*nginx-upconf-backends.conf'
```

wait at most *20 sec* (the value is configured in *nginx-upconf.conf*) and run
*curl* tests again to check if the new values were set.

