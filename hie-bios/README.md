# hie-bios

`hie-bios` is the way which `hie` sets up a GHC API session.

Its design is motivated by the guiding principle:

> It is the responsibility of the build tool to describe the environment
>  which a package should be built in.

This means that it is possible
to easily support a wide range of tools including `cabal-install`, `stack`,
`rules_haskell`, `hadrian` and `obelisk` without major contortions.
`hie-bios` does not depend on the `Cabal` library nor does not
read any complicated build products and so on.

How does a tool specify a session? A session is fully specified by a set of
standard GHC flags. Most tools already produce this information if they support
a `repl` command. Launching a repl is achieved by calling `ghci` with the
right flags to specify the package database. `hie-bios` needs a way to get
these flags and then it can set up GHC API session correctly.

Futher it means that any failure to set up the API session is the responsibility
of the build tool. It is up to them to provide the correct information if they
want HIE to work correctly.

## Explicit Configuration

The user can place a `hie.dhall` file in the root of the workspace which
describes how to setup the environment. For example, to explicitly state
that you want to use `stack` then the configuration file would look like:

```
{ cradle = Cradle.Stack {=} }
```

If you use `cabal` then you probably need to specify which component you want
to use.

```
{ cradle = Cradle.Cabal { component = Some "lib:haskell-ide-engine" } }
```

Or you can explicitly state the program which should be used to collect
the options by supplying the path to the program. It is interpreted
relative to the current working directory if it is not an absolute path.

```
{ cradle = Cradle.Bios { prog = ".hie-bios" } }
```

The complete dhall configuration is described by the following type

```
< cradle :
< Cabal : { component : Optional Text }
  | Stack : {}
  | Bazel : {}
  | Obelisk : {}
  | Bios : { prog : Text}
  | Default : {} > >
```

## Implicit Configuration

There are several built in modes which captures most common Haskell development
scenarios. If no `hie.dhall` configuration file is found then an implicit
configuration is searched for.

### Priority

The targets are searched for in following order.

1. A specific `hie-bios` file.
2. An `obelisk` project
3. A `rules_haskell` project
4. A `stack` project
4. A `cabal` project
5. The default cradle which has no specific options.

### `cabal-install`

The workspace root is the first folder containing a `cabal.project` file.

The arguments are collected by running `cabal v2-repl`.

If `cabal v2-repl` fails, then the user needs to configure the correct
target to use by writing a `hie.dhall` file.

### `rules_haskell`

The workspace root is the folder containing a `WORKSPACE` file.

The options are collected by querying `bazel`.

### `obelisk`

The workspace root is the folder containing a `.obelisk` directory.

The options are collected by running `ob ide-args`.

### `bios`

The most general form is the `bios` mode which allows a user to specify themselves
which flags to provide.

In this mode, an executable file called `.hie-bios` is placed in the root
of the workspace directory. The script takes one argument, the filepath
to the current file we want to load into the session. The script returns
the correct arguments in order to load that file successfully.

A good guiding specification for this file is that the following command
should work for any file in your project.

```
ghci $(./hie-bios /path/to/foo.hs) /path/to/foo.hs
```

This is useful if you are designing a new build system or the other modes
fail to setup the correct session for some reason. For example, this is
how hadrian (GHC's build system) is integrated into HIE.


## Relationship with `ghcid`

The design of `hie-bios` is inspired by `ghcid`. Like `ghcid`, it does not depend
on any of the tools it supports. The success of `ghcid` is that it works reliably
in many situations. This is because of the fact that it delegates complicated
decisions about a build to the build tool.

`ghcid` could be implemented using `hie-bios` using the `ghci $(./hie-bios Main.hs) Main.hs`
idiom described earlier.

