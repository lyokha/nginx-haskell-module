#### Automatic tests with prove

Run automatic tests with command *prove* from Perl module *Test::Harness*.
Tests require Perl module *Test::Nginx::Socket*.

##### Build and install Haskell library

```ShellSession
$ cd t
$ make
$ sudo make install
```

##### Run tests

Tests can be run by a regular user.

```ShellSession
$ prove t/basic.t
```

Add option *-v* for verbose output. Before run, you may need to adjust
environment variable *PATH* to point to a specific Nginx build.

