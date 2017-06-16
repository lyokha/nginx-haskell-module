In this example client requests get proxied to destinations which values are
retrieved from a remote database according to *key/value* pairs found in the
request: e.g. GET arguments, the request body, cookies, headers etc. Handler
*fromRemoteDB* accepts address of the database and a list of key/value pairs.
It traverses the list from its head: if a key is empty then it returns the
non-empty value as is, if the key and the value are not empty then it asks
destination from the remote database, otherwise if a value is empty then it
skips to the next pair. If the remote database returns empty string then it also
skips to the next pair.

The database is emulated with a virtual server listening on port *9000* in
*nginx.conf*. It has two records for key *user*: with values *peter* and *max*.
User *peter* belongs to backend *ubackend1*, and user *max* belongs to backend
*ubackend2*. If a *key/value* pair is not found then the database returns
backend *ufailover*.

Here is the rule for searching in database.

```nginx
            haskell_run_async_on_request_body reqFld $hs_rb_fld_user user;
            haskell_run_async fromRemoteDB $hs_async_dst
                    '["localhost:9000"
                     , [ [""    , "$cookie_dst"    ]
                       , ["user", "$arg_user"      ]
                       , ["user", "$hs_rb_fld_user"]
                       , [""    , "ufailover" ]
                       ]
                     ]';
```

If *cookie* *dst* is present in the request then it gets proxied to the value of
the cookie, otherwise if *arg_user* or field *user* from a *POSTed* form are
present in the request then the destination is asked from the remote database,
if all of them fail (i.e. are not present in the request in this case) then the
request gets proxied to backend *failover*.

Let's run some *curl* tests.

```ShellSession
$ curl 'http://localhost:8010/some/resource'
In failover backend!
$ curl 'http://localhost:8010/some/resource?user=peter'
In backend1!
$ curl 'http://localhost:8010/some/resource?user=pete'
In failover backend!
$ curl -d'a=some&user=max' 'http://localhost:8010/some/resource?user=pete'
In failover backend!
$ curl -d'a=some&user=max' 'http://localhost:8010/some/resource'
In backend2!
curl -b'dst=ubackend1' -d'a=some&user=max' 'http://localhost:8010/some/resource'
In backend1!
```

Notice that when argument *user* is present in *URI* but its value (e.g. *pete*)
is not found in the database, then *fromRemoteDB* returns *ufailover* without
checking against the next *key/value* pairs (e.g. *user=max* from the *POSTed*
form). If you want that *fromRemoteDB* tries to check other pairs, then the
database should return an empty string on not found data. This is easy to
achieve: simply set the variable ``$failover`` in *nginx.conf* to empty string
and then you'll get a different response.

```ShellSession
$ curl -d'a=some&user=max' 'http://localhost:8010/some/resource?user=pete'
In backend2!
```

