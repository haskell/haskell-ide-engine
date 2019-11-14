## Build system

`haskell-ide-engine` is built using the library `shake`. The build descriptions are defined in the file `install.hs`.

Previously, `haskell-ide-engine` was built using a `Makefile` on Unix systems and a `PowerShell` script on Windows. By replacing both scripts by a Haskell-based solution, the build process is unified.

### Design goals

The design of the build system has the following main goals:

* works identically on every platform
* has minimal run-time dependencies:
    - `stack`
    - `git`
* is completely functional right after a simple `git clone` and after every `git pull`
* one-stop-shop for building and naming all executables required for using `hie` in IDEs.
* prevents certain build failures by either identifying a failed precondition (such as wrong `stack` version) or by performing the necessary steps so users can't forget them (such as invoking `git` to update submodules)


* is able to modify the environment such that `hie` can be run
    - setup `hoogle` database
    - setup `hlint` data-files

See the project's `README` for detailed information about installing `hie`.

### Targets

The build script `install.hs` defines several targets using the `shake` build system. The targets are roughly:

* `hie-*`: builds and installs the `hie` binaries. Also renames the binaries to contain the correct version-number.
* `build-lastest`: builds ad installs `hie` for the last available and supported `ghc` version.
* `build-all`: builds and installs `hie` binaries for all supported `ghc` versions. This option may take a long time and computer resources so use it with caution.
* `build-data`: builds the hoogle-db required by `hie`
* `build`:  builds ad installs `hie` for the last supported `ghc` version (like `build-lastest`) and the hoogle-db (like `build-data`)
* `cabal-*`: execute the same task as the original target, but with `cabal` instead of `stack`

Each `stack-*.yaml` contains references to packages in the submodules. Calling `stack` with one of those causes the build to fail if the submodules have not been initialized already. The file `shake.yaml` solves this issue invoking the `git` binary itself to update the submodules. Moreover, it specifies the correct version of `shake` and is used for installing all run-time dependencies such as `cabal` and `hoogle` if necessary.

### Run-time dependencies

`hie` depends on a correct environment in order to function properly:

* `cabal-install`: This dependency is required by `hie` to handle correctly projects that are not `stack` based (without `stack.yaml`). You can install an appropiate version using `stack` with the `stack-install-cabal` target.
* The `hoogle` database: `hoogle generate` needs to be called with the most-recent `hoogle` version.

### Steps to build `hie`

Installing `hie` is a multi-step process:

1. `git submodule sync && git submodule update --init`
2. `hoogle generate` (`hoogle>=5.0.17` to be safe)
3. `stack --stack-yaml=stack-<X>.yaml install` or `cabal new-install -w ghc-<X>`
4. rename `hie` binary to `hie-<X>` in `$HOME/.local/bin`, where `<X>` is the GHC version used
5. repeat step 4 and 5 for all desired GHC versions

This ensures that a complete install is always possible after each `git pull` or a `git clone`.

#### Building `hie` with profiling support

To build `hie` with profiling enabled `cabal new-install` needs to be used instead of `stack`.

Configure `cabal` to enable profiling by setting `profiling: True` in `cabal.project.local` for all packages. If that file does not already exist, create it as follows:

```bash
cat << EOF > cabal.project.local
package *
  profiling: True
EOF
```

Then `hie` can be compiled for a specific GHC version:

```bash
export GHCP=<path-to-ghc-binary>
cabal new-install exe:hie -w $GHCP \
  --write-ghc-environment-files=never --symlink-bindir=$HOME/.local/bin \
  --overwrite-policy=always --reinstall
```

The final step is to configure the `hie` client to use a custom `hie-wrapper` script that enables the runtime options for profiling. Such a script could look like this:

```bash
#!/bin/sh
~/.local/bin/hie-wrapper "$@" +RTS -xc
```

(Note: If no profiling information is shown when using `hie` with a certain project, it may help to build that project itself with profiling support, e.g. `stack build --profile`.)

### Safety checks

The `install.hs` script performs some checks to ensure that a correct installation is possible and provide meaningful error messages for known issues.

* `stack` needs to be up-to-date. Version `1.9.3` is required
* `cabal` needs to be up-to-date. Version `3.0.0.0` is required for windows systems and `2.4.1.0` for other ones.
* `ghc-8.6.3` is broken on windows. Trying to install `hie-8.6.3` on windows is not possible.
* When the build fails, an error message, that suggests to remove `.stack-work` directory, is displayed.

### Tradeoffs

#### `stack` is a build dependency

Currently, `stack` is needed even if you run the script with `cabal` to get the path where install the binaries but there are plans to remove that dependency (see #1380).

#### run `install.hs` with `stack` installs a GHC before running

Before the code in `install.hs` can be executed, `stack` installs a `GHC`, depending on the `resolver` field in `shake.yaml`. This is necessary if `install.hs` should be completely functional right after a fresh `git clone` without further configuration.

This may lead to an extra `GHC` to be installed by `stack` if not all versions of `haskell-ide-engine` are installed.
