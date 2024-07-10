#### Module *NgxExport*

[![Hackage](https://img.shields.io/hackage/v/ngx-export.svg?label=hackage%20%7C%20ngx-export&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/ngx-export)

##### Build and install with Cabal v1-commands

Note that Cabal *v1-commands* are deprecated.

###### Configure and build

```ShellSession
$ cabal v1-configure
$ cabal v1-build
```

###### Install

```ShellSession
$ cabal v1-install
```

The module is also available at
[*hackage.haskell.org*](http://hackage.haskell.org/package/ngx-export), so you
can simply install it from there with

```ShellSession
$ cabal v1-install ngx-export
```

##### Build as a dependency in a Nix-style local build aka Cabal v2-build

```ShellSession
$ cabal build
```

The module can be loaded in the *REPL* with

```ShellSession
$ cabal repl --repl-options=-fobject-code
```

