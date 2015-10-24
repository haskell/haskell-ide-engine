module Haskell.Ide.Monad where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.IORef
import           Haskell.Ide.Plugin
import           Haskell.Ide.Types
import qualified Language.Haskell.GhcMod.LightGhc as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           Module (mkModuleName)
import           Options.Applicative.Simple
import qualified Paths_haskell_ide as Meta
import           System.Directory

runIdeM :: IdeM a -> IO a
runIdeM f = do
    initializedRef <- newIORef False
    let inner = GM.runGmOutT opts $ GM.runGhcModT opts $ do
            liftIO $ writeIORef initializedRef True
            f
        opts = GM.defaultOptions
    (eres, _) <- inner `catch` \ex -> case ex of
        GM.GMEWrongWorkingDirectory projDir _ -> do
            -- Only switch dirs if the exception occurs during
            -- initialization. This way we don't mysteriously restart
            -- execution if the exception happens later.
            initialized <- readIORef initializedRef
            if initialized
                then throwIO ex
                else do
                    old <- getCurrentDirectory
                    bracket (setCurrentDirectory projDir)
                            (\_ -> setCurrentDirectory old)
                            (\_ -> inner)
        _ -> throwIO ex
    case eres of
        Left err -> throwIO err
        Right res -> return res

setTargets :: [Either FilePath GM.ModuleName] -> IdeM ()
setTargets targets = GM.runGmlT targets (return ())
