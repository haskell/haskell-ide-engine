module Haskell.Ide.Engine.Monad where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified GhcMod.Monad                  as GM
import qualified GhcMod.Types                  as GM
import           Haskell.Ide.Engine.MonadTypes
-- ---------------------------------------------------------------------

-- | runIdeM with Cradle found from the current directory
runIdeM :: GM.Options -> IdeState -> IdeM a -> IO a
runIdeM ghcModOptions s0 f = do
    ((eres, _),_s) <- runStateT (GM.runGhcModT ghcModOptions f) s0
    case eres of
        Left err  -> liftIO $ throwIO err
        Right res -> return res

