{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
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
dispatcher :: TChan ChannelRequest -> IdeM ()
dispatcher cin = do
  plugins <- getPlugins
  forever $ do
    debugm "run:top of loop"
    req <- liftIO $ atomically $ readTChan cin
    debugm $ "main loop:got:" ++ show req
    mresp <- doDispatch plugins req
    case mresp of
      Nothing -> return ()
      Just resp -> liftIO $ sendResponse req resp

-- ---------------------------------------------------------------------

-- | Send a response from the plugin to the designated reply channel
sendResponse :: (ValidResponse a) => ChannelRequest -> IdeResponse a -> IO ()
sendResponse req resp = do
  let cr = CResp (cinPlugin req) (cinReqId req) (fmap jsWrite resp)
  liftIO $ atomically $ writeTChan (cinReplyChan req) cr

-- ---------------------------------------------------------------------

-- | Manage the process of looking up the request in the known plugins,
-- validating the parameters passed and handing off to the appropriate
-- 'CommandFunc'
doDispatch :: Plugins -> ChannelRequest -> IdeM (Maybe (IdeResponse Object))
doDispatch plugins creq = do
  case Map.lookup (cinPlugin creq) plugins of
    Nothing -> return $ Just (IdeResponseError (IdeError
                UnknownPlugin ("No plugin found for:" <> cinPlugin creq )
                (Just $ toJSON $ cinPlugin creq)))
    Just desc -> do
      let pn  = cinPlugin creq
          req = cinReq creq
      debugm $ "doDispatch:desc=" ++ show desc
      debugm $ "doDispatch:req=" ++ show req
      case Map.lookup (pn,ideCommand req) (pluginCache plugins) of
        Nothing -> return $ Just (IdeResponseError (IdeError
                    UnknownCommand ("No such command:" <> ideCommand req )
                    (Just $ toJSON $ ideCommand req)))
        Just (Command cdesc cfunc) -> do
          case validateContexts cdesc req of
            Left err   -> return (Just err)
            Right ctxs -> case cfunc of
              CmdSync  f -> do
                r <- f ctxs req
                let r2 = fmap jsWrite r
                return (Just r2)
              CmdAsync f -> do
                f (sendResponse creq) ctxs req
                return Nothing

-- ---------------------------------------------------------------------

data IDEResponseRef = forall a .(ValidResponse a) => IDEResponseRef (IdeResponse a)

-- TODO: perhaps use this in IdeState instead
pluginCache :: Plugins -> Map.Map (T.Text,T.Text) Command
pluginCache plugins = Map.fromList r
  where
    doOne :: T.Text -> PluginDescriptor -> [((T.Text,T.Text),Command)]
    doOne pn (PluginDescriptor _ _ cmds _ _) =
        map (\cmd -> ((pn,cmdName (cmdDesc cmd)),cmd)) cmds

    r = concatMap (\(pn,pd) -> doOne pn pd) $ Map.toList plugins

-- ---------------------------------------------------------------------

-- |Return list of valid contexts for the given 'CommandDescriptor' and
-- 'IdeRequest'
validateContexts :: forall a .(ValidResponse a) => CommandDescriptor -> IdeRequest -> Either (IdeResponse a) [AcceptedContext]
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

validContext :: forall a .(ValidResponse a) => AcceptedContext -> ParamMap -> Either (IdeResponse a) AcceptedContext
validContext ctx params =
  case checkParams (contextMapping ctx) params of
    Left err -> Left err
    Right _  -> Right ctx


-- |If all listed 'ParamDescripion' values are present return a Right, else
-- return an error.
checkParams :: forall a .(ValidResponse a) => [ParamDescription] -> ParamMap -> Either (IdeResponse a) [()]
checkParams pds params = mapEithers checkOne pds
  where
    checkOne :: forall a .(ValidResponse a)
             => ParamDescription -> Either (IdeResponse a) ()
    checkOne (OP pn _ph pt) = checkParamOP pn pt
    checkOne (RP pn _ph pt) = checkParamRP pn pt

    checkParamOP :: forall a .(ValidResponse a)
                 => ParamId -> ParamType -> Either (IdeResponse a) ()
    checkParamOP pn pt =
      case Map.lookup pn params of
        Nothing -> Right ()
        Just p  -> checkParamMatch pn pt p

    checkParamRP :: forall a .(ValidResponse a)
                 => ParamId -> ParamType -> Either (IdeResponse a) ()
    checkParamRP pn pt =
      case Map.lookup pn params of
        Nothing -> Left $ missingParameter pn
        Just p  -> checkParamMatch pn pt p

    checkParamMatch :: forall a .(ValidResponse a)
                    => T.Text -> ParamType -> ParamValP -> Either (IdeResponse a) ()
    checkParamMatch pn' pt' p' =
      if paramMatches pt' p'
        then Right ()
        else Left $ incorrectParameter pn' pt' p'


    paramMatches :: ParamType -> ParamValP -> Bool
    paramMatches PtText (ParamTextP _) = True
    paramMatches PtFile (ParamFileP _) = True
    paramMatches PtPos  (ParamPosP _)  = True
    paramMatches _       _            = False
