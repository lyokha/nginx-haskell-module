Run unit tests in directory *t/* using command *prove* from Perl module
*Test::Harness*. Tests require additionally module *Test::Nginx::Socket*. They
can be run by a regular user.

```ShellSession
$ prove -r -v t
```

Before run, you may need to adjust environment variable *PATH* to include
desirable Nginx build to test.

