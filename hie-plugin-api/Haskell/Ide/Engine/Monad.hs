module Haskell.Ide.Engine.Monad where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified GhcMod.Monad                  as GM
import qualified GhcMod.Types                  as GM
import           Haskell.Ide.Engine.MonadTypes
-- ---------------------------------------------------------------------

-- | runIdeGhcM with Cradle found from the current directory
runIdeGhcM :: GM.Options -> IdeState -> IdeGhcM a -> IO a
runIdeGhcM ghcModOptions s0 f = do
    (eres, _) <- runMTState (GM.runGhcModT ghcModOptions f) s0
    case eres of
        Left err  -> liftIO $ throwIO err
        Right res -> return res

