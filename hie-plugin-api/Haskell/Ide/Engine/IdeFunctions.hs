{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Functions that act within the IdeGhcM monad

module Haskell.Ide.Engine.IdeFunctions
  (
    getPlugins
  ) where

import           Haskell.Ide.Engine.MonadTypes

getPlugins :: (MonadMTState IdeState m) => m IdePlugins
getPlugins = idePlugins <$> readMTS
