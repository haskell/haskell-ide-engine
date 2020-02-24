if [ -z "$PROJECT_FILE" ]; then
  export PROJECT_FILE="cabal.project"
  if [ -f "cabal.project-$GHC_VERSION" ]; then
    export PROJECT_FILE="cabal.project-$GHC_VERSION"
  fi
fi
export CABAL_ROOT=$HOME/.cabal
export PATH=$CABAL_ROOT/bin:$PATH
