export CABAL_DIR="D:\cabal"
export GHCS_PATH=$(cygpath $ProgramData)/chocolatey/lib/ghc/tools
export GHC_PATH=$GHCS_PATH/ghc-$GHC_VERSION
export CABAL_ROOT=$(cygpath $CABAL_DIR)
export Z3_BIN_PATH=/usr/local/z3-4.8.5-x64-win/bin
export PATH=$CABAL_ROOT/bin:$GHC_PATH/bin:$Z3_BIN_PATH:$PATH
