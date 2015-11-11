{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

-- |Manage the process of looking up the request in the known plugins,
-- validating the parameters passed and handing off to the appropriate
-- 'CommandFunc'
doDispatch :: Plugins -> ChannelRequest -> IdeM IdeResponse
doDispatch plugins creq = do
  case Map.lookup (cinPlugin creq) plugins of
    Nothing -> return (IdeResponseError (IdeError
                UnknownPlugin ("No plugin found for:" <> cinPlugin creq )
                (Just $ toJSON $ cinPlugin creq)))
    Just desc -> do
      let pn  = cinPlugin creq
          req = cinReq creq
      debugm $ "doDispatch:desc=" ++ show desc
      debugm $ "doDispatch:req=" ++ show req
      case Map.lookup (pn,ideCommand req) (pluginCache plugins) of
        Nothing -> return (IdeResponseError (IdeError
                    UnknownCommand ("No such command:" <> ideCommand req )
                    (Just $ toJSON $ ideCommand req)))
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
            [] -> Left $ IdeResponseFail (IdeError InvalidContext
                      (T.pack $ "no valid context found, expecting one of:"
                                          ++ show (cmdContexts cd)) Nothing)
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
checkParams :: [ParamDescription] -> ParamMap -> Either IdeResponse [()]
checkParams pds params = mapEithers checkOne pds
  where
    checkOne :: ParamDescription -> Either IdeResponse ()
    checkOne (OP pn _ph pt) = checkParamOP pn pt
    checkOne (RP pn _ph pt) = checkParamRP pn pt

    checkParamOP :: ParamId -> ParamType -> Either IdeResponse ()
    checkParamOP pn pt =
      case Map.lookup pn params of
        Nothing -> Right ()
        Just p  -> checkParamMatch pn pt p

    checkParamRP :: ParamId -> ParamType -> Either IdeResponse ()
    checkParamRP pn pt =
      case Map.lookup pn params of
        Nothing -> Left $ missingParameter pn
        Just p  -> checkParamMatch pn pt p

    checkParamMatch :: T.Text -> ParamType -> ParamValP -> Either IdeResponse ()
    checkParamMatch pn' pt' p' =
      if paramMatches pt' p'
        then Right ()
        else Left $ incorrectParameter pn' pt' p'


    paramMatches :: ParamType -> ParamValP -> Bool
    paramMatches PtText (ParamTextP _) = True
    paramMatches PtFile (ParamFileP _) = True
    paramMatches PtPos  (ParamPosP _)  = True
    paramMatches _       _            = False
