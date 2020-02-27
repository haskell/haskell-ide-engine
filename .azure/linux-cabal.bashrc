if [ -z "$PROJECT_FILE" ]; then
  export PROJECT_FILE="cabal.project"
  if [ -f "cabal.project-$GHC_VERSION" ]; then 
    export PROJECT_FILE="cabal.project-$GHC_VERSION"
  fi
fi
export PATH=$HOME/.cabal/bin:/opt/cabal/$CABAL_VERSION/bin:/opt/ghc/$GHC_VERSION/bin:$HOME/.local/bin:$PATH
