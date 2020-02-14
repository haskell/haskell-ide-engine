if [ -z "$CABAL_DIR" ]; then
  CABAL_DIR="$APPDATA\\cabal"
fi
export LOCAL_BIN_PATH=$(cygpath $APPDATA\\local\\bin)
export CABAL_ROOT=$(cygpath $CABAL_DIR)
export Z3_BIN_PATH=/usr/local/z3-4.8.5-x64-win/bin
export PATH=$Z3_BIN_PATH:$LOCAL_BIN_PATH:$PATH
