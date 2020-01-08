## Build system

`haskell-ide-engine` is built using the library `shake`. The build descriptions are defined in the file `install.hs`.

Previously, `haskell-ide-engine` was built using a `Makefile` on Unix systems and a `PowerShell` script on Windows. By replacing both scripts by a Haskell-based solution, the build process is unified.

### Design goals

The design of the build system has the following main goals:

* works identically on every platform
* has minimal run-time dependencies:
    - `stack` or `cabal`
    - `git`
* is completely functional right after a simple `git clone` and after every `git pull`
* prevents certain build failures by either identifying a failed precondition (such as wrong `stack` version) or by performing the necessary steps so users can't forget them (such as invoking `git` to update submodules)


* is able to modify the environment such that `hie` can be run
    - setup `hoogle` database
    - setup `hlint` data-files

See the project's `README` for detailed information about installing `hie`.

### Targets

The build script `install.hs` defines several targets using the `shake` build system. The targets are roughly:

* `hie-*`: builds and installs the `hie` binaries. Also renames the binaries to contain the correct version-number.
* `latest`: builds and installs `hie` for the latest available and supported `ghc` version.
* `data`: builds the hoogle-db required by `hie`
* `hie`:  builds and installs `hie` for the latest supported `ghc` version (like `latest`) and the hoogle-db (like `data`)

Each `stack-*.yaml` contains references to packages in the submodules. Calling `stack` with one of those causes the build to fail if the submodules have not been initialized already. The file `shake.yaml` solves this issue invoking the `git` binary itself to update the submodules. Moreover, it specifies the correct version of `shake` and is used for installing all run-time dependencies such as `hoogle` if necessary.

### Run-time dependencies

`hie` depends on a correct environment in order to function properly:

* `cabal-install`: This dependency is required by `hie` to handle correctly projects that are not `stack` based. You can install it using one of the methods listed here: https://www.haskell.org/cabal/#install-upgrade
* The `hoogle` database: `hoogle generate` needs to be called with the most-recent `hoogle` version.

### Steps to build `hie`

Installing `hie` is a multi-step process:

1. `git submodule sync && git submodule update --init`
2. `hoogle generate` (`hoogle>=5.0.17` to be safe)
3. `stack --stack-yaml=stack-<X>.yaml install` or `cabal v2-install -w ghc-<X>`
4. rename `hie` binary to `hie-<X>` in `$HOME/.local/bin`, where `<X>` is the GHC version used
5. repeat step 3 and 4 for all desired GHC versions

This ensures that a complete install is always possible after each `git pull` or a `git clone`.

#### Building `hie` with profiling support

To build `hie` with profiling enabled `cabal v2-install` needs to be used instead of `stack`.

Configure `cabal` to enable profiling by setting `profiling: True` in `cabal.project.local` for all packages. If that file does not already exist, create it as follows:

```bash
cat << EOF > cabal.project.local
package *
  profiling: True
EOF
```

Then `hie` can be compiled for a specific GHC version:

* For cabal prior to 3.0.0.0
```bash
export GHCP=<path-to-ghc-binary>
cabal v2-install exe:hie -w $GHCP \
  --write-ghc-environment-files=never --symlink-bindir=$HOME/.local/bin \
  --overwrite-policy=always --reinstall
```
* For cabal 3.0.0.0 or newer
```bash
export GHCP=<path-to-ghc-binary>
cabal v2-install exe:hie -w $GHCP \
  --write-ghc-environment-files=never --installdir=$HOME/.local/bin \
  --overwrite-policy=always --reinstall
```
* For windows you will need cabal 3.0.0.0 and add the argument `--install-method=copy`

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

#### run `install.hs` with `stack` installs a GHC before running

Before the code in `install.hs` can be executed, `stack` installs a `GHC`, depending on the `resolver` field in `shake.yaml`. This is necessary if `install.hs` should be completely functional right after a fresh `git clone` without further configuration.

This may lead to an extra `GHC` to be installed by `stack` if not all versions of `haskell-ide-engine` are installed.

However, you always could change the resolver in `shake.yaml` to match the appropiate one.
