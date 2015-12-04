{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskell.Ide.Engine.Monad where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.IORef
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           System.Directory

-- ---------------------------------------------------------------------

runIdeM :: IdeState -> IdeM a -> IO a
runIdeM initState f = do
    initializedRef <- newIORef False
    let inner' = GM.runGmOutT opts $ GM.runGhcModT opts $ do
            liftIO $ writeIORef initializedRef True
            (unIdeM f)
        inner = runStateT inner' initState
        opts = GM.defaultOptions
    ((eres, _),_s) <- inner `catch` \ex -> case ex of
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

-- ---------------------------------------------------------------------

setTargets :: [Either FilePath GM.ModuleName] -> IdeM ()
setTargets targets = IdeM $ GM.runGmlT targets (return ())



-- EOF
