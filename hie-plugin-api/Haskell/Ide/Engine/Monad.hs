{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Haskell.Ide.Engine.Monad where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
-- ---------------------------------------------------------------------

runIdeM :: GM.Options -> IdeState -> IdeM a -> IO a
runIdeM ghcModOptions s0 f = do
    ((eres, _),_s) <- runStateT (GM.runGhcModT ghcModOptions f) s0
    case eres of
        Left err -> liftIO $ throwIO err
        Right res -> return res



-- EOF
