{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.Engine.Dispatcher where

import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import           Exception
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types

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
  debugm $ "sendResponse (req,resp)=" ++ show (req,fmap jsWrite resp)
  let cr = CResp (cinPlugin req) (cinReqId req) (fmap jsWrite resp)
  liftIO $ atomically $ writeTChan (cinReplyChan req) cr

-- ---------------------------------------------------------------------

-- | Manage the process of looking up the request in the known plugins,
-- validating the parameters passed and handing off to the appropriate
-- 'CommandFunc'
doDispatch :: Plugins -> ChannelRequest -> IdeM (Maybe (IdeResponse Object))
doDispatch plugins creq = do
  case Map.lookup (cinPlugin creq) plugins of
    Nothing ->
      return $ Just $ IdeResponseError IdeError
        { ideCode = UnknownPlugin
        , ideMessage = "No plugin found for:" <> cinPlugin creq
        , ideInfo = toJSON $ cinPlugin creq
        }
    Just desc -> do
      let pn  = cinPlugin creq
          req = cinReq creq
      debugm $ "doDispatch:desc=" ++ show desc
      debugm $ "doDispatch:req=" ++ show req
      case Map.lookup (pn,ideCommand req) (pluginCache plugins) of
        Nothing ->
          return $ Just $ IdeResponseError IdeError
            { ideCode = UnknownCommand
            , ideMessage = "No such command:" <> ideCommand req
            , ideInfo = toJSON $ ideCommand req
            }
        Just (Command cdesc cfunc) -> do
          case validateContexts cdesc req of
            Left err   -> return (Just err)
            Right ctxs -> case cfunc of
              CmdSync  f -> do
                r <- f ctxs req
                       `gcatch` (\(e::SomeException) ->
                                  pure $ IdeResponseError
                                           (IdeError PluginError
                                                     (T.pack (show e))
                                                     Null))
                let r2 = fmap jsWrite r
                return (Just r2)
              CmdAsync f -> do
                f (sendResponse creq) ctxs req
                  `gcatch` (\(e::SomeException) ->
                             liftIO $ sendResponse creq
                                      (IdeResponseError
                                         (IdeError PluginError
                                                   (T.pack (show e))
                                                   Null) :: IdeResponse ()))
                return Nothing

-- ---------------------------------------------------------------------

data IDEResponseRef = forall a .(ValidResponse a) => IDEResponseRef (IdeResponse a)

-- TODO: perhaps use this in IdeState instead
pluginCache :: Plugins -> Map.Map (T.Text,T.Text) UntaggedCommand
pluginCache = Map.fromList . concatMap go . Map.toList
  where
    go :: (T.Text, UntaggedPluginDescriptor) -> [((T.Text, T.Text), UntaggedCommand)]
    go (pn, PluginDescriptor _ _ cmds _ _) =
      map (\cmd -> ((pn,cmdName (cmdDesc cmd)),cmd)) cmds

-- ---------------------------------------------------------------------

-- |Return list of valid contexts for the given 'CommandDescriptor' and
-- 'IdeRequest'
validateContexts :: UntaggedCommandDescriptor -> IdeRequest -> Either (IdeResponse a) [AcceptedContext]
validateContexts cd req = r
  where
    (errs,oks) = partitionEithers $ map (\c -> validContext c (ideParams req)) $ cmdContexts cd
    r = case (oks, errs) of
      ([], e:_) -> Left e
      ([], []) -> Left $ IdeResponseFail IdeError
        { ideCode = InvalidContext
        , ideMessage = T.pack ("no valid context found, expecting one of:" ++ show (cmdContexts cd))
        , ideInfo = Null
        }
      (ctxs, _) ->
        case checkParams (cmdAdditionalParams cd) (ideParams req) of
          Left e -> Left e
          Right _ -> Right ctxs

validContext :: AcceptedContext -> ParamMap -> Either (IdeResponse a) AcceptedContext
validContext ctx params =
  case checkParams (contextMapping ctx) params of
    Left err -> Left err
    Right _  -> Right ctx


-- |If all listed 'ParamDescripion' values are present return a Right, else
-- return an error.
checkParams :: [ParamDescription] -> ParamMap -> Either (IdeResponse a) [()]
checkParams pds params = mapEithers checkOne pds
  where
    checkOne :: ParamDescription -> Either (IdeResponse a) ()
    checkOne (ParamDesc pn _ph pt Optional) = checkParamOP pn pt
    checkOne (ParamDesc pn _ph pt Required) = checkParamRP pn pt

    checkParamOP :: ParamId -> ParamType -> Either (IdeResponse a) ()
    checkParamOP pn pt =
      case Map.lookup pn params of
        Nothing -> Right ()
        Just p  -> checkParamMatch pn pt p

    checkParamRP :: ParamId -> ParamType -> Either (IdeResponse a) ()
    checkParamRP pn pt =
      case Map.lookup pn params of
        Nothing -> Left $ missingParameter pn
        Just p  -> checkParamMatch pn pt p

    checkParamMatch :: T.Text -> ParamType -> ParamValP -> Either (IdeResponse a) ()
    checkParamMatch pn' pt' p' =
      if paramMatches pt' p'
        then Right ()
        else Left $ incorrectParameter pn' pt' p'


    paramMatches :: ParamType -> ParamValP -> Bool
    paramMatches PtText (ParamTextP _) = True
    paramMatches PtFile (ParamFileP _) = True
    paramMatches PtPos  (ParamPosP _)  = True
    paramMatches _       _            = False
