Run automatic tests in directory *t/* using command *prove* from Perl module
*Test::Harness*. Tests require additionally Perl module *Test::Nginx::Socket*.
They can be run by a regular user.

```ShellSession
$ prove -r t
```

Add option *-v* for verbose output. Before run, you may need to adjust
environment variable *PATH* to set up a specific Nginx build for testing.

