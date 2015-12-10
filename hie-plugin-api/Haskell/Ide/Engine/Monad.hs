{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Haskell.Ide.Engine.Monad where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Char
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import qualified Language.Haskell.GhcMod.Utils as GM
import qualified Language.Haskell.GhcMod.Debug as GM
import           System.Directory
import           System.IO.Unsafe
-- ---------------------------------------------------------------------

runLock :: MVar ThreadId
runLock = unsafePerformIO $ newEmptyMVar
{-# NOINLINE runLock #-}

runIdeM :: IdeState -> IdeM a -> IO a
runIdeM s0 f = do
    let errorIO e = liftIO $ throwIO $ ErrorCall e

    -- FIXME: this is very racy do some fancy stuff with masking
    -- _ <- liftIO $ (\case Just tid -> errorIO $ "locked by " ++ show tid)
    --     =<< tryReadMVar runLock
    -- liftIO $ putMVar runLock =<< myThreadId

    -- root <- either (error "could not get project root") (GM.dropWhileEnd isSpace) . fst
    --           <$> GM.runGhcModT GM.defaultOptions GM.rootInfo

    -- liftIO $ setCurrentDirectory root

    ((eres, _),_s) <- flip runStateT s0 (GM.runGhcModT GM.defaultOptions f)
    case eres of
        Left err -> liftIO $ throwIO err
        Right res -> return res

-- EOF
