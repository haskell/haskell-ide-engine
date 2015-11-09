{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text as T
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
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
      Nothing -> return (IdeResponseError (toJSON $ "No plugin found for:" <> cinPlugin req ))
      Just desc -> doDispatch (cinPlugin req) desc [] (cinReq req)
    let cr = CResp (cinPlugin req) (cinReqId req) r
    liftIO $ writeChan (cinReplyChan req) cr

-- ---------------------------------------------------------------------

doDispatch :: PluginId -> PluginDescriptor -> CommandFunc
doDispatch pn desc _ req = do
  plugins <- getPlugins
  debugm $ "doDispatch:desc=" ++ show desc
  debugm $ "doDispatch:req=" ++ show req
  case Map.lookup (pn,ideCommand req) (pluginCache plugins) of
    Nothing -> return (IdeResponseError (toJSON $ "No such command:" <> ideCommand req))
    Just cmd -> do
      case validateContexts (cmdDesc cmd) req of
        Left err -> return err
        Right ctxs -> (cmdFunc cmd) ctxs req

-- ---------------------------------------------------------------------

-- TODO: perhaps use this in IdeState instead
pluginCache :: Plugins -> Map.Map (T.Text,T.Text) Command
pluginCache plugins = Map.fromList r
  where
    doOne :: T.Text -> PluginDescriptor -> [((T.Text,T.Text),Command)]
    doOne pn pd = map (\cmd -> ((pn,cmdName (cmdDesc cmd)),cmd)) $ pdCommands pd

    r = concatMap (\(pn,pd) -> doOne pn pd) $ Map.toList plugins

-- ---------------------------------------------------------------------

-- |Return list of valid contexts for the given 'CommandDescriptor' and
-- 'IdeRequest'
validateContexts :: CommandDescriptor -> IdeRequest -> Either IdeResponse [AcceptedContext]
validateContexts cd req = r
  where
    ectxs = mapEithers (\c -> validContext c (ideParams req)) $ cmdContexts cd
    r = case ectxs of
          Left err -> Left err
          Right [] -> Left $ IdeResponseFail (toJSON $ T.pack $ "no valid context found, expecting one of:" ++ show (cmdContexts cd))
          Right ctxs -> case checkParams (cmdAdditionalParams cd) (ideParams req) of
                  Right _  -> Right (concat ctxs)
                  Left err -> Left err

validContext :: AcceptedContext -> ParamMap -> Either IdeResponse [AcceptedContext]
validContext ctx params = checkParams (contextMapping ctx) params

checkParams :: [ParamDecription] -> ParamMap -> Either IdeResponse [AcceptedContext]
checkParams = error "not implemented"
