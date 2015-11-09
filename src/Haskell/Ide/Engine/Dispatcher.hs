{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either
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
    resp <- doDispatch plugins req
    let cr = CResp (cinPlugin req) (cinReqId req) resp
    liftIO $ writeChan (cinReplyChan req) cr

-- ---------------------------------------------------------------------

doDispatch :: Plugins -> ChannelRequest -> IdeM IdeResponse
doDispatch plugins creq = do
  case Map.lookup (cinPlugin creq) plugins of
    Nothing -> return (IdeResponseError (toJSON $ "No plugin found for:" <> cinPlugin creq ))
    Just desc -> do
      let pn  = cinPlugin creq
          req = cinReq creq
      debugm $ "doDispatch:desc=" ++ show desc
      debugm $ "doDispatch:req=" ++ show req
      case Map.lookup (pn,ideCommand req) (pluginCache plugins) of
        Nothing -> return (IdeResponseError (toJSON $ "No such command:" <> ideCommand req))
        Just cmd -> do
          case validateContexts (cmdDesc cmd) req of
            Left err   -> return err
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
    (errs,oks) = partitionEithers $ map (\c -> validContext c (ideParams req)) $ cmdContexts cd
    r = case oks of
          [] -> case errs of
            [] -> Left $ IdeResponseFail (toJSON $ T.pack $ "no valid context found, expecting one of:"
                                          ++ show (cmdContexts cd))
            (e:_) -> Left e
          ctxs -> case checkParams (cmdAdditionalParams cd) (ideParams req) of
                    Right _  -> Right ctxs
                    Left err -> Left err

validContext :: AcceptedContext -> ParamMap -> Either IdeResponse AcceptedContext
validContext ctx params =
  case checkParams (contextMapping ctx) params of
    Left err -> Left err
    Right _  -> Right ctx

-- |If all listed 'ParamDescripion' values are present return a Right, else
-- return an error.
checkParams :: [ParamDecription] -> ParamMap -> Either IdeResponse [()]
checkParams pds params = mapEithers checkOne pds
  where
    checkOne :: ParamDecription -> Either IdeResponse ()
    checkOne (OP pn _ph pt) = checkParamOP pn pt
    checkOne (RP pn _ph pt) = checkParamRP pn pt

    checkParamOP pn pt =
      case Map.lookup pn params of
        Nothing -> Right ()
        Just p  -> checkParamMatch pt p

    checkParamRP pn pt =
      case Map.lookup pn params of
        Nothing -> Left (IdeResponseFail (String $ T.pack $ "missing parameter '"++ show pn ++"'"))
        Just p  -> checkParamMatch pt p

    checkParamMatch pt' p' =
      if paramMatches pt' p'
        then Right ()
        else Left (IdeResponseFail (String $ T.pack $ "parameter type mismatch, expected "
                                                ++ show pt' ++ " but got "++ show p'))


    paramMatches PtText (ParamText _) = True
    paramMatches PtFile (ParamFile _) = True
    paramMatches PtPos  (ParamPos _)  = True
    paramMatches _       _            = False
