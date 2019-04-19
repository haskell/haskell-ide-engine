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
* fails with meaningful error messages in known circumstances
* is able to modify the environment such that `hie` can be run
    - setup `hoogle` database
    - setup `hlint` data-files

See the project's `README` for detailed information about installing `hie`.

### Targets

The build script `install.hs` defines several targets using the `shake` build system. The targets are roughly:

* `hie-*`: builds and installs the `hie` binaries. Also renames the binaries to contain the correct version-number.
* `build`: builds and installs `hie` binaries for all supported `ghc` versions.
* `build-doc`: builds the hoogle-db required by `hie`
* `cabal-*`: execute the same task as the original target, but with `cabal` instead of `stack`

Each `stack-*.yaml` contains references to packages in the submodules. Calling `stack` with one of those causes the build to fail if the submodules have not been initialized already. The file `shake.yaml` solves this issue. Moreover, it specifies the correct version of `shake` and is used for installing all run-time dependencies such as `cabal` and `hoogle` if necessary.

### Run-time dependencies

`hie` depends on a correct environment in order to function properly:

* `cabal-install`: If no `cabal` executable can be found or has an outdated version, `cabal-install` is installed via `stack`.
* The `hoogle` database: `hoogle generate` needs to be called with the most-recent `hoogle` version.

### Steps to build `hie`

Installing `hie` is a multi-step process:

1. `git submodule sync && git submodule update --init`
2. `hoogle generate` (`hoogle>=5.0.17` to be safe)
3. ensure that `cabal-install` is installed in the correct version
4. `stack --stack-yaml=stack-<X>.yaml install` or `cabal new-install -w ghc-<X>`
5. rename `hie` binary to `hie-<X>` in `$HOME/.local/bin`, where `<X>` is the GHC version used
6. repeat step 4 and 5 for all desired GHC versions

This ensures that a complete install is always possible after each `git pull` or a `git clone`.

### Safety checks

The `install.hs` script performs some checks to ensure that a correct installation is possible and provide meaningful error messages for known issues.

* `stack` needs to be up-to-date. Version `1.9.3` is required
* `ghc-8.6.3` does not work in windows. Trying to install `hie-8.6.3` on windows is not possible
* `cabal new-build` does not work on windows at the moment. All `cabal-*` targets exit with an error message about that.
* When the build fails, an error message, that suggests to remove `.stack-work` directory, is displayed.

### Tradeoffs

#### `stack` is a build dependency

Currently, it is not possible to build all `hie-*` executables automatically without `stack`, since the `install.hs` script is executed by `stack`.

We are open to suggestions of other build systems that honor the requirements above, but are executable without `stack`.

#### `install.hs` installs a GHC before running

Before the code in `install.hs` can be executed, `stack` installs a `GHC`, depending on the `resolver` field in `shake.yaml`. This is necessary if `install.hs` should be completely functional right after a fresh `git clone` without further configuration.

This may lead to an extra `GHC` to be installed by `stack` if not all versions of `haskell-ide-engine` are installed.
