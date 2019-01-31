## Build system

`haskell-ide-engine` is built using the library `shake`. The build descriptions are defined in the file `install.hs`.

### Design decisions

The design of the build system follows 2 main goals:

* works same on every platform
* has minimal run-time dependencies: `stack` and `git`
* is completely functional right after simple `git clone`
* one-stop-shop for all building

Previously, `haskell-ide-engine` was built using a `Makefile` on Unix systems and a `PowerShell` script on Windows. By replacing both scripts by a Haskell-based solution, the scripts can be can be replaced by a single script.

### Tradeoffs

#### `shake.yaml`

A `shake.yaml` is required for executing the `install.hs` file.

* It contains the required version of `shake`.
* In contrast to the other `*.yaml` it does not contain the submodules, which is necessary for `stack` to work even before the submodules have been initialized.

It is necessary to update the `resolver` field of the `shake.yaml` if the .

#### `install.hs` installs a GHC

Before the code in `install.hs` can be executed, `stack` installs a `GHC`, depending on the `resolver` field in `shake.yaml`. This is a necessary if the `install.hs` should be totally functional right after a fresh `git clone` without further configuration.

This may lead to an extra `GHC` to be installed by `stack` if not all versions of `haskell-ide-engine` are installed.
