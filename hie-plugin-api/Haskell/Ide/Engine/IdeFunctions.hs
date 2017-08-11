{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Functions that act within the IdeM monad

module Haskell.Ide.Engine.IdeFunctions
  (
    getPlugins
  ) where

import           Control.Monad.State.Strict
import           Haskell.Ide.Engine.MonadTypes

getPlugins :: IdeM IdePlugins
getPlugins = lift $ lift $ idePlugins <$> get
