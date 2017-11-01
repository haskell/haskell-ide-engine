{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Map                              as Map
import qualified Data.Set                              as S
import           GHC
import qualified GhcMod.ModuleLoader                   as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import qualified Haskell.Ide.GhcModPlugin              as GhcMod
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

data DispatcherEnv = DispatcherEnv
  { cancelReqsTVar     :: TVar (S.Set J.LspId)
  , wipReqsTVar        :: TVar (S.Set J.LspId)
  , docVersionTVar     :: TVar (Map.Map Uri Int)
  , docModuleCacheTVar :: TVar (Map.Map Uri GM.CachedModule)
  }

dispatcherP :: forall void. DispatcherEnv -> TChan PluginRequest -> IdeM void
dispatcherP DispatcherEnv{..} pin = forever $ do
  debugm "dispatcherP: top of loop"
  (PReq context mver mid callback action) <- liftIO $ atomically $ readTChan pin
  debugm $ "got request with id: " ++ show mid

  let runner = case context of
        Nothing -> GM.runActionWithContext Nothing
        Just uri -> case uriToFilePath uri of
          Just fp -> GM.runActionWithContext (Just $ GM.filePathToUri fp)
          Nothing -> \act -> do
            debugm "Got malformed uri, running action with default context"
            GM.runActionWithContext Nothing act

  let runWithCallback = do
        r <- runner action

        -- get cached module and put it in the tvar
        case context of
          Nothing -> return ()
          Just uri -> do
            mm <- GM.getCachedModule (uri2fileUri uri)
            case mm of
              Nothing -> return ()
              Just cm -> do
                liftIO $ atomically $ modifyTVar' docModuleCacheTVar $
                  \m -> Map.insert uri cm m

        liftIO $ callback r

  let runIfVersionMatch = case mver of
        Nothing -> runWithCallback
        Just (uri, reqver) -> do
          curver <- liftIO $ atomically $ Map.lookup uri <$> readTVar docVersionTVar
          if Just reqver /= curver then
            debugm "not processing request as it is for old version"
          else do
            debugm "Processing request as version matches"
            runWithCallback

  case mid of
    Nothing -> runIfVersionMatch
    Just lid -> do
      cancelReqs <- liftIO $ atomically $ do
        modifyTVar' wipReqsTVar (S.delete lid)
        readTVar cancelReqsTVar
      if S.member lid cancelReqs
        then do
          debugm $ "cancelling request: " ++ show lid
          liftIO $ atomically $ modifyTVar' cancelReqsTVar (S.delete lid)
        else do
          debugm $ "processing request: " ++ show lid
          runIfVersionMatch

