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
import           Control.Monad.Trans.State.Lazy
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
  rpid <- forkIO $ reactor def cin cout rin
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
    { sender :: !(Maybe (BSL.ByteString -> IO ()))
    , reqId  :: !RequestId
    , wip    :: Map.Map RequestId GUI.OutMessage
    }

instance Default ReactorState where
  def = ReactorState Nothing 0 Map.empty

-- ---------------------------------------------------------------------

type R a = StateT ReactorState IO a

-- ---------------------------------------------------------------------


reactor :: ReactorState -> TChan ChannelRequest -> TChan ChannelResponse -> TChan ReactorInput -> IO ()
reactor st cin cout inp = do
  (flip evalStateT) st $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of
      HandlerRequest sf (GUI.RspFromClient rm) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm


      HandlerRequest sf n@(GUI.NotDidOpenTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm $ "\n****** reactor: processing NotDidOpenTextDocument"
        -- TODO: learn enough lens to do the following more cleanly
        let doc = J.uriTextDocumentItem $ J.textDocumentDidOpenTextDocumentNotificationParams
                                        $ fromJust $ J.paramsNotificationMessage notification
            fileName = drop (length ("file://"::String)) doc
        liftIO $ U.logs $ "\n********* doc=" ++ show doc
        rid <- nextReqId
        let req = CReq "applyrefact" rid (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
        liftIO $ atomically $ writeTChan cin req
        keepOriginal rid n

      HandlerRequest sf n@(GUI.NotDidSaveTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm $ "\n****** reactor: processing NotDidSaveTextDocument"
        let J.TextDocumentIdentifier doc = J.textDocumentDidSaveTextDocumentParams $ fromJust $ J.paramsNotificationMessage notification
            fileName = drop (length ("file://"::String)) doc
        liftIO $ U.logs $ "\n********* doc=" ++ show doc
        rid <- nextReqId
        let req = CReq "applyrefact" rid (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
        liftIO $ atomically $ writeTChan cin req
        keepOriginal rid n

      HandlerRequest sf r@(GUI.ReqRename req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let params = fromJust $ J.paramsRequestMessage req
            J.TextDocumentIdentifier doc = J.textDocumentRenameRequestParams params
            fileName = drop (length ("file://"::String)) doc
            J.Position l c = J.positionRenameRequestParams params
            newName  = J.newNameRenameRequestParams params
        rid <- nextReqId
        let hreq = CReq "hare" rid (IdeRequest "rename" (Map.fromList
                                                    [("file",     ParamFileP (T.pack fileName))
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (l,c)))
                                                    ,("name",     ParamValP $ ParamText (T.pack newName))
                                                    ])) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r


      HandlerRequest sf om -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om

      DispatcherResponse rsp@(CResp pid rid res)-> do
        liftIO $ U.logs $ "reactor:got DispatcherResponse:" ++ show rsp
        morig <- lookupOriginal rid
        case morig of
          Nothing -> do
            let smr = J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just res)
            reactorSend smr
          Just orig -> do
            liftIO $ U.logs $ "reactor: original was:" ++ show orig


-- ---------------------------------------------------------------------

setSendFunc :: (BSL.ByteString -> IO ()) -> R ()
setSendFunc sf = modify' (\s -> s {sender = Just sf})

-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a) => a -> R ()
reactorSend msg = do
  s <- get
  case sender s of
    Nothing -> error "reactorSend: send function not initialised yet"
    Just sf -> liftIO $ sf (J.encode msg)

-- ---------------------------------------------------------------------

nextReqId :: R RequestId
nextReqId = do
  s <- get
  let r = reqId s
  put s { reqId = r + 1}
  return r

-- ---------------------------------------------------------------------

keepOriginal :: RequestId -> GUI.OutMessage -> R ()
keepOriginal rid om = modify' (\s -> s { wip = Map.insert rid om (wip s)})

-- ---------------------------------------------------------------------

lookupOriginal :: RequestId -> R (Maybe GUI.OutMessage)
lookupOriginal rid = do
  w <- gets wip
  return $ Map.lookup rid w

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
renameRequestHandler rin sf req@(J.RequestMessage _ origId _ _) = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.ReqRename req))
  -- let loc = def :: J.Location
  --     res  = GUI.makeResponseMessage origId loc
  -- sf (J.encode res)

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
  U.logm "\n******** got didOpenTextDocumentNotificationHandler, processing"
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.NotDidOpenTextDocument notification))

-- ---------------------------------------------------------------------

didSaveTextDocumentNotificationHandler :: GUI.Handler (TChan ReactorInput)
                                                      J.DidSaveTextDocumentNotification
didSaveTextDocumentNotificationHandler rin sf notification = do
  U.logm $ "\n****** didSaveTextDocumentNotificationHandler: processing"
  atomically $ writeTChan rin (HandlerRequest sf (GUI.NotDidSaveTextDocument notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: GUI.Handler (TChan ReactorInput) J.BareResponseMessage
responseHandlerCb rin sf resp = do
  U.logs $ "\n******** got ResponseMessage, ignoring:" ++ show resp

