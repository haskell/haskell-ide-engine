module Haskell.Ide.Engine.Monad where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified GhcMod.Monad                  as GM
import qualified GhcMod.Types                  as GM
import           Haskell.Ide.Engine.MonadTypes
import           Language.Haskell.LSP.Types.Capabilities

-- ---------------------------------------------------------------------

-- | runIdeGhcM with Cradle found from the current directory
runIdeGhcM :: GM.Options -> ClientCapabilities -> IdeState -> IdeGhcM a -> IO a
runIdeGhcM ghcModOptions caps s0 f = do
    (eres, _) <- flip runMTState s0 $ flip runReaderT caps $ GM.runGhcModT ghcModOptions f
    case eres of
        Left err  -> liftIO $ throwIO err
        Right res -> return res

