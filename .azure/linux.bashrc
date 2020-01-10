if [ -z "$GHC_VERSION" ]; then
   export GHC_VERSION=${YAML_FILE:6:5}
fi
export PATH=$HOME/.cabal/bin:/opt/cabal/$CABAL_VERSION/bin:/opt/ghc/$GHC_VERSION/bin:$HOME/.local/bin:$PATH
