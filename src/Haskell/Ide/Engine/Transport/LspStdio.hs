{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import qualified Data.Aeson.Types as J
import           Data.Algorithm.DiffOutput
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Either
import           Data.List
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as GUI
import qualified Language.Haskell.LSP.TH.ClientCapabilities as C
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import           System.Directory
import           System.Exit
import qualified System.Log.Logger as L

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport :: IO () -> TChan ChannelRequest -> IO ()
lspStdioTransport hieDispatcherProc cin = do
  run hieDispatcherProc cin >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: IO () -> TChan ChannelRequest -> IO Int
run dispatcherProc cin = flip E.catches handlers $ do

  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  _rhpid <- forkIO $ responseHandler cout rin
  _rpid  <- forkIO $ reactor def cin cout rin

  let
    dp capabilities = do
      atomically $ writeTChan rin (ClientCapabilities capabilities)
      dispatcherProc
      return Nothing

  flip E.finally finalProc $ do
    GUI.setupLogger "/tmp/hie-vscode.log" L.DEBUG
    CTRL.run dp (hieHandlers rin) hieOptions

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
  forever $ do
    r <- atomically $ readTChan cout
    atomically $ writeTChan cr (DispatcherResponse r)

-- ---------------------------------------------------------------------

data ReactorInput = DispatcherResponse ChannelResponse
                  | HandlerRequest (BSL.ByteString -> IO ()) GUI.OutMessage
                  | ClientCapabilities C.ClientCapabilities

data ReactorState =
  ReactorState
    { sender             :: !(Maybe (BSL.ByteString -> IO ()))
    , hieReqId           :: !RequestId
    , lspReqId           :: !J.LspId
    , wip                :: !(Map.Map RequestId GUI.OutMessage)
    , clientCapabilities :: !(Maybe C.ClientCapabilities)
    }

instance Default ReactorState where
  def = ReactorState Nothing 0 (J.IdInt 0) Map.empty Nothing

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R a = StateT ReactorState IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

setSendFunc :: (BSL.ByteString -> IO ()) -> R ()
setSendFunc sf = modify' (\s -> s {sender = Just sf})

setClientCapabilities :: C.ClientCapabilities -> R ()
setClientCapabilities c = modify' (\s -> s {clientCapabilities = Just c})

-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a) => a -> R ()
reactorSend msg = do
  s <- get
  case sender s of
    Nothing -> error "reactorSend: send function not initialised yet"
    Just sf -> liftIO $ sf (J.encode msg)

reactorSend' :: ((BSL.ByteString -> IO ()) -> IO ()) -> R ()
reactorSend' f = do
  msf <- gets sender
  case msf of
    Nothing -> error "reactorSend': send function not initialised yet"
    Just sf -> liftIO $ f sf

-- ---------------------------------------------------------------------

nextReqId :: R RequestId
nextReqId = do
  s <- get
  let r = hieReqId s
  put s { hieReqId = r + 1}
  return r

-- ---------------------------------------------------------------------

nextLspReqId :: R J.LspId
nextLspReqId = do
  s <- get
  let i@(J.IdInt r) = lspReqId s
  put s { lspReqId = J.IdInt (r + 1) }
  return i

-- ---------------------------------------------------------------------

keepOriginal :: RequestId -> GUI.OutMessage -> R ()
keepOriginal rid om = modify' (\s -> s { wip = Map.insert rid om (wip s)})

-- ---------------------------------------------------------------------

lookupOriginal :: RequestId -> R (Maybe GUI.OutMessage)
lookupOriginal rid = do
  w <- gets wip
  return $ Map.lookup rid w

-- ---------------------------------------------------------------------

sendErrorResponse :: J.LspId -> J.ErrorCode -> String -> R ()
sendErrorResponse origId err msg
  = reactorSend' (\sf -> GUI.sendErrorResponseS sf (J.responseId origId) err msg)

sendErrorLog :: String -> R ()
sendErrorLog  msg = reactorSend' (\sf -> GUI.sendErrorLogS  sf msg)

-- sendErrorShow :: String -> R ()
-- sendErrorShow msg = reactorSend' (\sf -> GUI.sendErrorShowS sf msg)

-- ---------------------------------------------------------------------
-- reactor monad functions end
-- ---------------------------------------------------------------------


-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and hie dispatcher
reactor :: ReactorState -> TChan ChannelRequest -> TChan ChannelResponse -> TChan ReactorInput -> IO ()
reactor st cin cout inp = do
  flip evalStateT st $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of
      ClientCapabilities capabilities -> do
        liftIO $ U.logs $ "reactor:got Client capabilities:" ++ show capabilities
        setClientCapabilities capabilities

      HandlerRequest sf (GUI.RspFromClient rm) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      HandlerRequest sf n@(GUI.NotInitialized notification) -> do
        setSendFunc sf
        liftIO $ U.logm $ "****** reactor: processing Initialized Notification"
        -- Server is ready, register any specific capabilities we need

         {-
         Example:
         {
                 "method": "client/registerCapability",
                 "params": {
                         "registrations": [
                                 {
                                         "id": "79eee87c-c409-4664-8102-e03263673f6f",
                                         "method": "textDocument/willSaveWaitUntil",
                                         "registerOptions": {
                                                 "documentSelector": [
                                                         { "language": "javascript" }
                                                 ]
                                         }
                                 }
                         ]
                 }
         }
        -}
        let
          registration = J.Registration "lsp-demote-id" "workspace/executeCommand" Nothing
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId

        -- Current vscode implementation has the wrong name in it:
        -- https://github.com/Microsoft/vscode-languageserver-node/issues/199
        let smr = J.RequestMessage "2.0" rid "client/registerFeature"  (Just registrations)
        -- let smr = J.RequestMessage "2.0" rid "client/registerCapability"  (Just registrations)

        reactorSend smr

      -- -------------------------------

      HandlerRequest sf n@(GUI.NotDidOpenTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm $ "****** reactor: processing NotDidOpenTextDocument"
        -- TODO: learn enough lens to do the following more cleanly
        -- let doc = J._uri $ J._textDocument $ fromJust $ J._params notification
        let
            params  = fromJust $ J._params (notification :: J.DidOpenTextDocumentNotification)
            textDoc = J._textDocument (params :: J.DidOpenTextDocumentNotificationParams)
            doc     = J._uri (textDoc :: J.TextDocumentItem)
            fileName = drop (length ("file://"::String)) doc
        liftIO $ U.logs $ "********* doc=" ++ show doc
        rid <- nextReqId
        let req = CReq "applyrefact" rid (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
        liftIO $ atomically $ writeTChan cin req
        keepOriginal rid n

      -- -------------------------------

      HandlerRequest sf n@(GUI.NotDidSaveTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            params = fromJust $ J._params (notification :: J.NotificationMessage J.DidSaveTextDocumentParams)
            J.TextDocumentIdentifier doc = J._textDocument (params :: J.DidSaveTextDocumentParams)
            fileName = drop (length ("file://"::String)) doc
        liftIO $ U.logs $ "********* doc=" ++ show doc
        rid <- nextReqId
        let req = CReq "applyrefact" rid (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
        liftIO $ atomically $ writeTChan cin req
        keepOriginal rid n

      HandlerRequest sf (GUI.NotDidChangeTextDocument _notification) -> do
        setSendFunc sf
        liftIO $ U.logm "****** reactor: NOT processing NotDidChangeTextDocument"

      -- -------------------------------

      HandlerRequest sf r@(GUI.ReqRename req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let params = fromJust $ J._params (req :: J.RenameRequest)
            J.TextDocumentIdentifier doc = J._textDocument (params :: J.RenameRequestParams)
            fileName = drop (length ("file://"::String)) doc
            J.Position l c = J._position (params :: J.RenameRequestParams)
            newName  = J._newName params
        rid <- nextReqId
        let hreq = CReq "hare" rid (IdeRequest "rename" (Map.fromList
                                                    [("file",     ParamFileP (T.pack fileName))
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
                                                    ,("name",     ParamValP $ ParamText (T.pack newName))
                                                    ])) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r


      -- -------------------------------

      HandlerRequest sf r@(GUI.ReqHover req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let J.TextDocumentPositionParams doc pos = fromJust $ J._params (req :: J.HoverRequest)
            fileName = drop (length ("file://"::String)) $ J._uri (doc :: J.TextDocumentIdentifier)
            J.Position l c = pos
        rid <- nextReqId
        let hreq = CReq "ghcmod" rid (IdeRequest "type" (Map.fromList
                                                    [("file",     ParamFileP (T.pack fileName))
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
                                                    ])) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r

      -- -------------------------------

      HandlerRequest sf (GUI.ReqCodeAction req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = fromJust $ J._params (req :: J.CodeActionRequest)
            doc = J._textDocument (params :: J.CodeActionParams)
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            J.CodeActionContext (J.List diags) = J._context (params :: J.CodeActionParams)

        let
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "hlint") _m  ) = [J.Command title cmd cmdparams]
            where
              title = "Apply hint:" ++ head (lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See hieOptions above
              cmd = "applyrefact:applyOne"
              -- need 'file' and 'start_pos'
              args = J.Array$ V.fromList
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m  ) = []
        let body = concatMap makeCommand diags
        let rspMsg = GUI.makeResponseMessage (J.responseId $ J._id (req :: J.CodeActionRequest)) body
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest sf r@(GUI.ReqExecuteCommand req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" -- ++ show req
        cwd <- liftIO getCurrentDirectory
        -- liftIO $ U.logs $ "reactor:cwd:" ++ cwd
        let params = fromJust $ J._params (req :: J.ExecuteCommandRequest)
            command = J._command (params :: J.ExecuteCommandParams)
            margs = J._arguments (params :: J.ExecuteCommandParams)

        -- liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
        cmdparams <- case margs of
              Nothing -> return []
              Just (J.List os) -> do
                let (lts,rts) = partitionEithers $ map convertParam os
                -- TODO:AZ: return an error if any parse errors found.
                when (not $ null lts) $
                  liftIO $ U.logs $ "\n\n****reactor:ExecuteCommandRequest:error converting params=" ++ show lts ++ "\n\n"
                return rts

        rid <- nextReqId
        let (plugin,cmd) = break (==':') command
        let hreq = CReq (T.pack plugin) rid (IdeRequest (T.pack $ tail cmd) (Map.fromList
                                                    cmdparams)) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r

      -- -------------------------------

      HandlerRequest sf om -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om


      -- ---------------------------------------------------

      DispatcherResponse rsp@(CResp _pid rid res)-> do
        liftIO $ U.logs $ "reactor:got DispatcherResponse:" ++ show rsp
        morig <- lookupOriginal rid
        case morig of
          Nothing -> do
            sendErrorLog $ "reactor:could not find original LSP message for: " ++ show rsp
          Just orig -> do
            liftIO $ U.logs $ "reactor: original was:" ++ show orig
            case orig of
              GUI.NotDidOpenTextDocument _ ->
                case res of
                  IdeResponseFail  err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseError err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseOk r -> do
                    reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)

              GUI.NotDidSaveTextDocument _ -> do
                case res of
                  IdeResponseFail  err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseError err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseOk r -> do
                    let smr = J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
                    reactorSend smr

              GUI.ReqRename req -> hieResponseHelper req res $ \r -> do
                let J.Success vv = J.fromJSON (J.Object r) :: J.Result RefactorResult
                let we = refactorResultToWorkspaceEdit vv
                let rspMsg = GUI.makeResponseMessage (J.responseId $ J._id (req :: J.RenameRequest)) we
                reactorSend rspMsg

              GUI.ReqHover req -> hieResponseHelper req res $ \r -> do
                let
                  J.Success (TypeInfo mtis) = J.fromJSON (J.Object r) :: J.Result TypeInfo
                  ht = case mtis of
                    []  -> J.Hover [] Nothing
                    tis -> J.Hover ms (Just range)
                      where
                        ms = map (\ti -> J.MarkedString "haskell" (T.unpack $ trText ti)) tis
                        tr = head tis
                        range = J.Range (posToPosition $ trStart tr) (posToPosition $ trEnd tr)
                  rspMsg = GUI.makeResponseMessage (J.responseId $ J._id (req :: J.HoverRequest) ) ht
                reactorSend rspMsg

              GUI.ReqExecuteCommand req -> hieResponseHelper req res $ \r -> do
                let
                  reply v = reactorSend $ GUI.makeResponseMessage (J.responseId $ J._id (req :: J.ExecuteCommandRequest)) v
                -- When we get a RefactorResult or HieDiff, we need to send a
                -- separate WorkspaceEdit Notification
                liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
                case toWorkspaceEdit r of
                  Just we -> do
                    reply (J.Object mempty)
                    lid <- nextLspReqId
                    reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
                  Nothing ->
                    reply (J.Object r)

              other -> do
                sendErrorLog $ "reactor:not processing for original LSP message : " ++ show other

-- ---------------------------------------------------------------------

-- TODO: this could probably be done with Aeson combinators somehow
toWorkspaceEdit :: J.Object -> Maybe J.ApplyWorkspaceEditParams
toWorkspaceEdit r = v
  where
    refactorResult =
      case J.fromJSON (J.Object r) :: J.Result RefactorResult of
        J.Success vv -> [J.ApplyWorkspaceEditParams $ refactorResultToWorkspaceEdit vv]
        _            -> []

    hieDiff =
      case J.parse jsRead r :: J.Result HieDiff of
        J.Success vv -> [J.ApplyWorkspaceEditParams $ refactorResultToWorkspaceEdit (RefactorResult [vv])]
        _            -> []

    v = case refactorResult ++ hieDiff of
      [we] -> Just we
      _    -> Nothing

-- ---------------------------------------------------------------------

convertParam :: J.Value -> Either String (ParamId, ParamValP)
convertParam (J.Object hm) = case H.toList hm of
  [(k,v)] -> case (J.fromJSON v) :: J.Result LspParam of
             J.Success pv -> Right (k, lspParam2ParamValP pv)
             J.Error errStr -> Left $ "convertParam: could not decode parameter value for "
                               ++ show k ++ ", err=" ++ errStr
  _       -> Left $ "convertParam: expecting a single key/value, got:" ++ show hm
convertParam v = Left $ "convertParam: expecting Object, got:" ++ show v

lspParam2ParamValP :: LspParam -> ParamValP
lspParam2ParamValP (LspTextDocument (TextDocumentIdentifier u)) = ParamFileP (T.drop (length ("file://"::String)) u)
lspParam2ParamValP (LspPosition     (Position l c))             = ParamPosP (Pos (Line (l+1)) (Col (c+1)))
lspParam2ParamValP (LspRange        (Range (Position l c) _to)) = ParamPosP (Pos (Line (l+1)) (Col (c+1)))

data LspParam
  = LspTextDocument TextDocumentIdentifier
  | LspPosition     Position
  | LspRange        Range
  deriving (Read,Show,Eq)

instance J.FromJSON LspParam where
  parseJSON (J.Object hm) =
    case H.toList hm of
      [("textDocument",v)] -> LspTextDocument <$> J.parseJSON v
      [("position",v)]     -> LspPosition     <$> J.parseJSON v
      [("range-pos",v)]    -> LspRange        <$> J.parseJSON v
      _ -> fail $ "FromJSON.LspParam got:" ++ show hm
  parseJSON _ = mempty

-- ---------------------------------------------------------------------

-- | Manage the boilerplate for passing on any errors found in the IdeResponse
hieResponseHelper :: forall a t. J.RequestMessage a -> IdeResponse t -> (t -> R ()) -> R ()
hieResponseHelper req res action =
  case res of
    IdeResponseFail  err -> sendErrorResponse (J._id (req :: J.RequestMessage a)) J.InternalError (show err)
    IdeResponseError err -> sendErrorResponse (J._id (req :: J.RequestMessage a)) J.InternalError (show err)
    IdeResponseOk r -> action r

-- ---------------------------------------------------------------------

posToPosition :: Pos -> J.Position
posToPosition (Pos (Line l) (Col c)) = J.Position (l-1) (c-1)

-- ---------------------------------------------------------------------

hieOptions :: GUI.Options
-- hieOptions = def
-- hieOptions = def { GUI.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["hie-command"]))
--                  }
-- hieOptions = def { GUI.textDocumentSync = Just J.TdSyncNone
--                  , GUI.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-demote-id"]))
--                  }
hieOptions = def { GUI.textDocumentSync = Just J.TdSyncFull
                 , GUI.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["applyrefact:applyOne"]))
                 }


hieHandlers :: TChan ReactorInput -> GUI.Handlers
hieHandlers rin
  = def { GUI.initializedHandler                       = Just $ initializedHandler rin
        , GUI.renameHandler                            = Just $ renameRequestHandler rin
        , GUI.hoverHandler                             = Just $ hoverRequestHandler rin
        , GUI.didOpenTextDocumentNotificationHandler   = Just $ didOpenTextDocumentNotificationHandler rin
        , GUI.didSaveTextDocumentNotificationHandler   = Just $ didSaveTextDocumentNotificationHandler rin
        , GUI.didChangeTextDocumentNotificationHandler = Just $ didChangeTextDocumentNotificationHandler rin
        , GUI.didCloseTextDocumentNotificationHandler  = Just $ didCloseTextDocumentNotificationHandler rin
        , GUI.cancelNotificationHandler                = Just $ cancelNotificationHandler rin
        , GUI.responseHandler                          = Just $ responseHandlerCb rin
        , GUI.codeActionHandler                        = Just $ codeActionHandler rin
        , GUI.executeCommandHandler                    = Just $ executeCommandHandler rin
        }

-- ---------------------------------------------------------------------

hoverRequestHandler :: TChan ReactorInput -> GUI.Handler J.HoverRequest
hoverRequestHandler rin sf req = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.ReqHover req))

-- ---------------------------------------------------------------------

renameRequestHandler :: TChan ReactorInput -> GUI.Handler J.RenameRequest
renameRequestHandler rin sf req = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.ReqRename req))

-- ---------------------------------------------------------------------

codeActionHandler :: TChan ReactorInput -> GUI.Handler J.CodeActionRequest
codeActionHandler rin sf req = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.ReqCodeAction req))

-- ---------------------------------------------------------------------

executeCommandHandler :: TChan ReactorInput -> GUI.Handler J.ExecuteCommandRequest
executeCommandHandler rin sf req = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.ReqExecuteCommand req))

-- ---------------------------------------------------------------------

initializedHandler :: TChan ReactorInput -> GUI.Handler J.InitializedNotification
initializedHandler rin sf notification = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.NotInitialized notification))

-- ---------------------------------------------------------------------

didOpenTextDocumentNotificationHandler :: TChan ReactorInput -> GUI.Handler J.DidOpenTextDocumentNotification
didOpenTextDocumentNotificationHandler rin sf notification = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.NotDidOpenTextDocument notification))

-- ---------------------------------------------------------------------

didSaveTextDocumentNotificationHandler :: TChan ReactorInput -> GUI.Handler J.DidSaveTextDocumentNotification
didSaveTextDocumentNotificationHandler rin sf notification = do
  atomically $ writeTChan rin (HandlerRequest sf (GUI.NotDidSaveTextDocument notification))

-- ---------------------------------------------------------------------

didChangeTextDocumentNotificationHandler :: TChan ReactorInput -> GUI.Handler J.DidChangeTextDocumentNotification
didChangeTextDocumentNotificationHandler rin sf notification = do
  atomically $ writeTChan rin (HandlerRequest sf (GUI.NotDidChangeTextDocument notification))

-- ---------------------------------------------------------------------

didCloseTextDocumentNotificationHandler :: TChan ReactorInput -> GUI.Handler J.DidCloseTextDocumentNotification
didCloseTextDocumentNotificationHandler rin sf notification = do
  atomically $ writeTChan rin (HandlerRequest sf (GUI.NotDidCloseTextDocument notification))

-- ---------------------------------------------------------------------

cancelNotificationHandler :: TChan ReactorInput -> GUI.Handler J.CancelNotification
cancelNotificationHandler rin sf notification = do
  atomically $ writeTChan rin  (HandlerRequest sf (GUI.NotCancelRequest notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> GUI.Handler J.BareResponseMessage
responseHandlerCb _rin _sf resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------

-- TODO: perhaps move this somewhere else, for general use
refactorResultToWorkspaceEdit :: RefactorResult -> J.WorkspaceEdit
refactorResultToWorkspaceEdit (RefactorResult diffs) = J.WorkspaceEdit (Just r) Nothing
  where
    r = H.fromList $ map hieDiffToLspEdit diffs

-- TODO: perhaps move this somewhere else, for general use
hieDiffToLspEdit :: HieDiff -> (T.Text,[J.TextEdit])
hieDiffToLspEdit (HieDiff f _ d) = (T.pack ("file://" ++ f),r)
  where
    pd = parsePrettyDiffs d
    r = map diffOperationToTextEdit pd

diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
diffOperationToTextEdit (Change fm to) = J.TextEdit r nt
  where
    sl = fst $ lrNumbers fm
    sc = 0
    s = J.Position (sl - 1) sc -- Note: zero-based lines
    el = snd $ lrNumbers fm
    ec = 100000 -- TODO: unsavoury, but not sure how else to do it. Perhaps take the length of the last element of the text to be replaced.
    e = J.Position (el - 1) ec -- Note: zero-based lines
    r = J.Range s e
    nt = intercalate "\n" $ lrContents to

-- diffOperationToTextEdit (Deletion fm _) = J.TextEdit r ""
--   where
{-

Turn

[Change (LineRange {lrNumbers = (3,4), lrContents = ["foo :: Int","foo = 5"]})
        (LineRange {lrNumbers = (3,4), lrContents = ["foo1 :: Int","foo1 = 5"]})]

-- | Diff Operation  representing changes to apply
data DiffOperation a = Deletion a LineNo
            | Addition a LineNo
            | Change a a
            deriving (Show,Read,Eq,Ord)

into

interface TextEdit {
    /**
     * The range of the text document to be manipulated. To insert
     * text into a document create a range where start === end.
     */
    range: Range;

    /**
     * The string to be inserted. For delete operations use an
     * empty string.
     */
    newText: string;
}


data TextEdit =
  TextEdit
    { rangeTextEdit   :: Range
    , newTextTextEdit :: String
    } deriving (Show,Read,Eq)

-}

