{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Ide.Engine.Transport.LspStdio
  (
    lspStdioTransport
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Either.Utils
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as GUI
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import           System.Exit
import           System.IO
import qualified System.Log.Logger as L


-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport :: TChan ChannelRequest -> IO ()
lspStdioTransport cin = do
  run cin >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: TChan ChannelRequest -> IO Int
run cin = flip E.catches handlers $ do

  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  U.logs $ "\n\n*************************about to fork responseHandler"
  rhpid <- forkIO $ responseHandler cout rin
  U.logs $ "\n\n*********************forked responseHandler:rhpid=" ++ show rhpid
  rpid <- forkIO $ reactor (ReactorState Nothing) cin cout rin
  U.logs $ "\n\n*************************forked reactor:rpid=" ++ show rpid

  flip E.finally finalProc $ do
    CTRL.run rin hieHandlers hieOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

responseHandler :: TChan ChannelResponse -> TChan ReactorInput -> IO ()
responseHandler cout cr = do
  -- U.logm $ "\n\n************responseHandler starting up"
  forever $ do
    r@(CResp pid rid res) <- atomically $ readTChan cout
    -- U.logs $ "\n\n***********responseHandler: got :" ++ show r
    atomically $ writeTChan cr (DispatcherResponse r)

-- ---------------------------------------------------------------------

data ReactorInput = DispatcherResponse ChannelResponse
                  | HandlerRequest (BSL.ByteString -> IO ()) GUI.OutMessage

data ReactorState =
  ReactorState
    { sender :: Maybe (BSL.ByteString -> IO ())
    }

reactor :: ReactorState -> TChan ChannelRequest -> TChan ChannelResponse -> TChan ReactorInput -> IO ()
reactor s cin cout inp = do
  inval <- liftIO $ atomically $ readTChan inp
  s' <- case inval of
    HandlerRequest sf (GUI.RspFromClient rm) -> do
      U.logs $ "reactor:got RspFromClient:" ++ show rm
      return s {sender = Just sf}
    HandlerRequest sf (GUI.NotDidSaveTextDocument notification) -> do
      U.logm $ "\n****** reactor: processing NotDidSaveTextDocument"
      let J.TextDocumentIdentifier doc = J.textDocumentDidSaveTextDocumentParams $ fromJust $ J.paramsNotificationMessage notification
          fileName = drop (length ("file://"::String)) doc
      U.logs $ "\n********* doc=" ++ show doc
      let req = CReq "applyrefact" 1 (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
      atomically $ writeTChan cin req
      yield
      return s {sender = Just sf}
    HandlerRequest sf om -> do
      U.logs $ "reactor:got HandlerRequest:" ++ show om
      return s {sender = Just sf}
    DispatcherResponse rsp@(CResp pid rid res)-> do
      U.logs $ "reactor:got DispatcherResponse:" ++ show rsp
      -- CTRL.sendResponseMessage $ GUI.makeResponseMessage rid res
      let smr = J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just res)
      U.logs $ "\n\n***********responseHandler: smr :" ++ show smr
      reactorSend s smr
      U.logs $ "\n\n***********responseHandler: smr sent"
      return s
  reactor s' cin cout inp

-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a) => ReactorState -> a -> IO ()
reactorSend s msg =
  case sender s of
    Nothing -> error "reactorSend: send function not initialised yet"
    Just sf -> sf (J.encode msg)

-- ---------------------------------------------------------------------

hieOptions :: GUI.Options
hieOptions = def
-- hieOptions = def { GUI.textDocumentSync = Just J.TdSyncFull
--                  , GUI.codeLensProvider = Just def
--                  }

hieHandlers :: GUI.Handlers (TChan ReactorInput)
hieHandlers
  = def { GUI.renameHandler   = Just renameRequestHandler
        -- , GUI.codeLensHandler = Just codeLensHandler
        , GUI.didOpenTextDocumentNotificationHandler = Just didOpenTextDocumentNotificationHandler
        , GUI.didSaveTextDocumentNotificationHandler = Just didSaveTextDocumentNotificationHandler
        , GUI.responseHandler                        = Just responseHandlerCb
        }

-- ---------------------------------------------------------------------

renameRequestHandler :: GUI.Handler (TChan ReactorInput) J.RenameRequest
renameRequestHandler rin sf (J.RequestMessage _ origId _ _) = do
  let loc = def :: J.Location
      res  = GUI.makeResponseMessage origId loc
  sf (J.encode res)

-- ---------------------------------------------------------------------

codeLensHandler :: GUI.Handler (TChan ReactorInput) J.CodeLensRequest
codeLensHandler rin sf (J.RequestMessage _ origId _ _) = do
  let
    lens = J.CodeLens (J.Range (J.Position 1 1) (J.Position 1 10)) (Just (J.Command "codeLensCmd" "actionCmd" Nothing)) Nothing
    lenses = [lens]
    res = GUI.makeResponseMessage origId lenses
  sf (J.encode res)

-- ---------------------------------------------------------------------

didOpenTextDocumentNotificationHandler :: GUI.Handler (TChan ReactorInput) J.DidOpenTextDocumentNotification
didOpenTextDocumentNotificationHandler rin sf notification = do
  U.logm "\n******** got didOpenTextDocumentNotificationHandler, ignoring"

-- ---------------------------------------------------------------------

didSaveTextDocumentNotificationHandler :: GUI.Handler (TChan ReactorInput)
                                                      J.DidSaveTextDocumentNotification
didSaveTextDocumentNotificationHandler rin sf notification = do
  U.logm $ "\n****** didSaveTextDocumentNotificationHandler: processing"
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.NotDidSaveTextDocument notification))
  -- let J.TextDocumentIdentifier doc = J.textDocumentDidSaveTextDocumentParams $ fromJust $ J.paramsNotificationMessage notification
  --     fileName = drop (length ("file://"::String)) doc
  -- U.logs $ "\n********* doc=" ++ show doc
  -- -- let req = CReq "ghcmod" 1 (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
  -- let req = CReq "applyrefact" 1 (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
  -- atomically $ writeTChan cin req

-- ---------------------------------------------------------------------

responseHandlerCb :: GUI.Handler (TChan ReactorInput) J.BareResponseMessage
responseHandlerCb rin sf resp = do
  U.logs $ "\n******** got ResponseMessage, ignoring:" ++ show resp

