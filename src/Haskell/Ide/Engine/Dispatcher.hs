{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent
import           Control.Logging
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- |Listen on a Chan for ChannelRequest from the assorted listeners, and route
-- them through to the appropriate plugin for processing.
dispatcher :: Chan ChannelRequest -> IdeM ()
dispatcher cin = do
  plugins <- getPlugins
  forever $ do
    debugm "run:top of loop"
    req <- liftIO $ readChan cin
    debugm $ "main loop:got:" ++ show req
    r <- case Map.lookup (cinPlugin req) plugins of
      Nothing -> return (IdeResponseError (String $ T.pack $ "No plugin found for:" ++ cinPlugin req ))
      -- Just (PluginReg desc disp) -> disp (cinReq req)
      Just desc -> doDispatch (cinPlugin req) desc (cinReq req)
    let cr = CResp (cinPlugin req) (cinReqId req) r
    liftIO $ writeChan (cinReplyChan req) cr

-- ---------------------------------------------------------------------

doDispatch :: PluginId -> PluginDescriptor -> Dispatcher
doDispatch pn desc req = do
  plugins <- getPlugins
  liftIO $ debug $ T.pack $ "doDispatch:desc=" ++ show desc
  liftIO $ debug $ T.pack $ "doDispatch:req=" ++ show req
  case Map.lookup (pn,ideCommand req) (pluginCache plugins) of
    Nothing -> return (IdeResponseError (String $ T.pack $ "No such command:" ++ ideCommand req))
    Just uic -> (uiFunc uic) req

-- TODO: perhaps use this in IdeState instead
pluginCache :: Plugins -> Map.Map (String,String) UiCommand
pluginCache plugins = Map.fromList r
  where
    doOne :: String -> PluginDescriptor -> [((String,String),UiCommand)]
    doOne pn pd = map (\uic -> ((pn,uiCmdName uic),uic)) $ pdUiCommands pd

    r = concatMap (\(pn,pd) -> doOne pn pd) $ Map.toList plugins

