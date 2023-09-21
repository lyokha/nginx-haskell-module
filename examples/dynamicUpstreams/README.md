There is a good [*Nginx module for dynamic
upstreams*](http://github.com/cubicdaiya/ngx_dynamic_upstream). It provides API
for dynamic changes of servers and their parameters in static server groups aka
upstreams. *Nginx Plus* has similar functionality. Imagine that we want a
similar feature, however the server discovery must be run actively by Nginx: it
could poll a data provider synchronously using timeouts or be subscribed to
updates asynchronously. In this example, the first synchronous approach will be
used.

Let the data provider respond on a request from our service with a *JSON* object
with upstreams containing lists of their servers with optional parameters
including *weight*, *max_fails*, and *file_timeout*. Below is an example of such
a response.

```json
{
  "utest":
     [{"addr": "127.0.0.1:8020"}
     ,{"addr": "127.0.0.1:8030", "host": "my.server:8030"}
     ,{"addr": "127.0.0.1:8040", "weight": 2}
     ],
  "utest_hash":
     [{"addr": "127.0.0.1:8020" ,"max_fails": 2, "fail_timeout": 60}
     ,{"addr": "127.0.0.1:8030", "host": "my.server:8030"}
     ,{"addr": "127.0.0.1:8040"}
     ]
}
```

When a haskell service receives new data, it must update its service variable
and then call a *callback* function to make a request to a location with a
special *upconf* handler that will read the variable and update all the affected
upstreams with the new list of servers.

The *upconf* module is mostly adopted from the *dynamic upstreams module* and
provides three directives.

- *upconf* ``$var`` &mdash; Installs a content handler to read value of the
  ``$var`` and update the upstreams listed in it. Allowed at the *location*
  configuration level. It corresponds to directive *dynamic_upstream* from the
  dynamic upstream module but gets data from a variable.

- *upconf_round_robin* &mdash; Installs safe callbacks for round robin load
  balancing in dynamic upstreams. This directive is mandatory in upstreams
  managed by *upconf*. Allowed at the *upstream* configuration level.

- *upconf_hash* ``$var`` ``consistent`` &mdash; An alternative to
  *upconf_round_robin*. Expects an expression to calculate a *hash* value for
  searching the target server. Allowed at the *upstream* configuration level.

As soon as *upconf* must parse a *JSON* object, the module requires an external
library [*jsmn*](http://github.com/zserge/jsmn) for that. The *hash*
functionality was mostly adopted from *the upstream hash module* which is
shipped with the standard Nginx distribution, however it was enabled with
special *checkers* for synchronization between worker processes.

The main configuration resides in [nginx-upconf.conf](nginx-upconf.conf). File
[nginx-upconf-backends.conf](nginx-upconf-backends.conf) contains the data
provider emulator and the backends. To test how this all works, the main
configuration and the backends must be run in separate Nginx instances.

```ShellSession
# /usr/local/nginx/sbin/nginx -c /absolute/path/to/examples/dynamicUpstreams/nginx-upconf-backends.conf
# /usr/local/nginx/sbin/nginx -c /absolute/path/to/examples/dynamicUpstreams/nginx-upconf.conf
```

Run some *curl* tests.

```ShellSession
$ curl 'http://localhost:8010/'
[30890] In 8040
$ curl 'http://localhost:8010/'
[30890] In 8020
$ curl 'http://localhost:8010/'
[30890] In 8030
$ curl 'http://localhost:8010/'
[30890] In 8040
$ curl 'http://localhost:8010/hash?$a=hello'
[30890] In 8030
$ curl 'http://localhost:8010/hash?$a=hello'
[30890] In 8030
$ curl 'http://localhost:8010/hash?$a=hello'
[30890] In 8030
```

Then you can add or remove backends in *nginx-upconf-backends.conf* and
change the *JSON* value in *location /data* from server *data*, restart the
corresponding *backends* instance of Nginx,

```ShellSession
# pkill -HUP -f '/usr/local/nginx/sbin/nginx.*nginx-upconf-backends.conf'
```

wait at most *20 sec* (the value is configured in *nginx-upconf.conf*) and run
*curl* tests again to check if the new values have been set.

The *upconf* module does not support *backup* settings in the list of server
options. However, data providers such as *DNS* *SRV* records may provide several
layers of backup servers (say, via *priority* fields). It would be nice to use
such a great feature. And this is really possible with the *upstrands* from
[*Nginx combined upstreams
module*](http://github.com/lyokha/nginx-combined-upstreams-module). The
*upstrands* can be seen as upstreams with multiple backup layers. For the sake
of our example, let upstream *utest* be the primary layer of servers while
upstream *utest_hash* be the backup layer. Then the upstrand (say, *utest*) can
be declared as

```nginx
    upstrand utest {
        upstream utest;
        upstream utest_hash;
        order per_request;
        next_upstream_statuses error timeout 5xx;
        next_upstream_timeout 60s;
    }
```

The upstrand's layers of upstreams can be transparently accessed in directive
*proxy_pass* via the dedicated variable.

```nginx
            proxy_pass http://$upstrand_utest;
```

