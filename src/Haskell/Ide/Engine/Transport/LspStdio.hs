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
import qualified Data.HashMap.Strict as H
-- import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.TH.ClientCapabilities as C
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import           Language.Haskell.LSP.VFS
import           System.Directory
import           System.Exit
import qualified System.Log.Logger as L
import           Text.Parsec
-- import qualified Yi.Rope as Yi

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
    dp capabilities sendFunc = do
      atomically $ writeTChan rin (InitializeCallBack capabilities sendFunc)
      dispatcherProc
      return Nothing

  flip E.finally finalProc $ do
    Core.setupLogger "/tmp/hie-vscode.log" L.DEBUG
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

data ReactorInput
  = DispatcherResponse ChannelResponse
  | InitializeCallBack C.ClientCapabilities Core.SendFunc
  | HandlerRequest
      (J.Uri -> IO (Maybe VirtualFile))
      (BSL.ByteString -> IO ())
      Core.OutMessage
      -- ^ injected into the reactor input by each of the individual callback handlers


data ReactorState =
  ReactorState
    { sender             :: !(Maybe (BSL.ByteString -> IO ()))
    , hieReqId           :: !RequestId
    , lspReqId           :: !J.LspId
    , wip                :: !(Map.Map RequestId Core.OutMessage)
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

keepOriginal :: RequestId -> Core.OutMessage -> R ()
keepOriginal rid om = modify' (\s -> s { wip = Map.insert rid om (wip s)})

-- ---------------------------------------------------------------------

lookupOriginal :: RequestId -> R (Maybe Core.OutMessage)
lookupOriginal rid = do
  w <- gets wip
  return $ Map.lookup rid w

-- ---------------------------------------------------------------------

sendErrorResponse :: J.LspId -> J.ErrorCode -> String -> R ()
sendErrorResponse origId err msg
  = reactorSend' (\sf -> Core.sendErrorResponseS sf (J.responseId origId) err msg)

sendErrorLog :: String -> R ()
sendErrorLog  msg = reactorSend' (\sf -> Core.sendErrorLogS  sf msg)

-- sendErrorShow :: String -> R ()
-- sendErrorShow msg = reactorSend' (\sf -> Core.sendErrorShowS sf msg)

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
      InitializeCallBack capabilities sf -> do
        liftIO $ U.logs $ "reactor:got Client capabilities:" ++ show capabilities
        setSendFunc sf
        setClientCapabilities capabilities

      HandlerRequest _vf sf (Core.RspFromClient rm) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      HandlerRequest _vf sf (Core.NotInitialized _notification) -> do
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
          options = J.Object $ H.fromList [("documentSelector", J.Object $ H.fromList [("language",J.String "haskell")])]
          registration = J.Registration "hare:demote" "workspace/executeCommand" (Just options)
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId

        -- Current vscode implementation has the wrong name in it:
        -- https://github.com/Microsoft/vscode-languageserver-node/issues/199
        let smr = J.RequestMessage "2.0" rid "client/registerFeature"  (Just registrations)
        -- let smr = J.RequestMessage "2.0" rid "client/registerCapability"  (Just registrations)

        reactorSend smr

      -- -------------------------------

      HandlerRequest _vf sf n@(Core.NotDidOpenTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm $ "****** reactor: processing NotDidOpenTextDocument"
        -- TODO: learn enough lens to do the following more cleanly
        -- let doc = J._uri $ J._textDocument $ fromJust $ J._params notification
        let
            params  = fromJust $ J._params (notification :: J.DidOpenTextDocumentNotification)
            textDoc = J._textDocument (params :: J.DidOpenTextDocumentNotificationParams)
            doc     = J._uri (textDoc :: J.TextDocumentItem)
            fileName = drop (length ("file://"::String)) doc

        requestDiagnostics cin cout fileName n

      -- -------------------------------

      HandlerRequest _vf sf n@(Core.NotDidSaveTextDocument notification) -> do
        setSendFunc sf
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            params = fromJust $ J._params (notification :: J.NotificationMessage J.DidSaveTextDocumentParams)
            J.TextDocumentIdentifier doc = J._textDocument (params :: J.DidSaveTextDocumentParams)
            fileName = drop (length ("file://"::String)) doc
        requestDiagnostics cin cout fileName n

      HandlerRequest _vf sf (Core.NotDidChangeTextDocument _notification) -> do
        setSendFunc sf
        liftIO $ U.logm "****** reactor: NOT processing NotDidChangeTextDocument"

      -- -------------------------------

      HandlerRequest _vf sf r@(Core.ReqRename req) -> do
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

      HandlerRequest _vf sf r@(Core.ReqHover req) -> do
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

      HandlerRequest _vf sf (Core.ReqCodeAction req) -> do
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
        let rspMsg = Core.makeResponseMessage (J.responseId $ J._id (req :: J.CodeActionRequest)) body
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest _vf sf r@(Core.ReqExecuteCommand req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" -- ++ show req
        -- cwd <- liftIO getCurrentDirectory
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

      HandlerRequest _vf sf (Core.ReqCompletion req) -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got CompletionRequest:" ++ show req
        let J.TextDocumentPositionParams doc pos = fromJust $ J._params (req :: J.CompletionRequest)
            fileName = drop (length ("file://"::String)) $ J._uri (doc :: J.TextDocumentIdentifier)
            J.Position l c = pos
        -- rid <- nextReqId
        -- let hreq = CReq "ghcmod" rid (IdeRequest "type" (Map.fromList
        --                                             [("file",     ParamFileP (T.pack fileName))
        --                                             ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
        --                                             ])) cout
        -- liftIO $ atomically $ writeTChan cin hreq
        -- keepOriginal rid r
        liftIO $ U.logs $ "****reactor:ReqCompletion:not immplemented=" ++ show (fileName,doc,l,c)

        let cr = J.Completions [] -- ( [] :: [J.CompletionListType])
        let rspMsg = Core.makeResponseMessage (J.responseId $ J._id (req :: J.CompletionRequest)) cr
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest _vf sf om -> do
        setSendFunc sf
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om

      -- ---------------------------------------------------

      DispatcherResponse rsp@(CResp pid rid res)-> do
        liftIO $ U.logs $ "reactor:got DispatcherResponse:" ++ show rsp
        morig <- lookupOriginal rid
        case morig of
          Nothing -> do
            sendErrorLog $ "reactor:could not find original LSP message for: " ++ show rsp
          Just orig -> do
            liftIO $ U.logs $ "reactor: original was:" ++ show orig
            case orig of
              Core.NotDidOpenTextDocument _ ->
                case res of
                  IdeResponseFail  err -> liftIO $ U.logs $ "NotDidOpenTextDocument:got err" ++ show err
                  IdeResponseError err -> liftIO $ U.logs $ "NotDidOpenTextDocument:got err" ++ show err
                  IdeResponseOk r -> publishDiagnostics pid r

              Core.NotDidSaveTextDocument _ -> do
                case res of
                  IdeResponseFail  err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseError err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseOk r -> publishDiagnostics pid r

              Core.ReqRename req -> hieResponseHelper req res $ \r -> do
                let J.Success vv = J.fromJSON (J.Object r) :: J.Result RefactorResult
                let we = refactorResultToWorkspaceEdit vv
                let rspMsg = Core.makeResponseMessage (J.responseId $ J._id (req :: J.RenameRequest)) we
                reactorSend rspMsg

              Core.ReqHover req -> hieResponseHelper req res $ \r -> do
                let
                  J.Success (TypeInfo mtis) = J.fromJSON (J.Object r) :: J.Result TypeInfo
                  ht = case mtis of
                    []  -> J.Hover [] Nothing
                    tis -> J.Hover ms (Just range)
                      where
                        ms = map (\ti -> J.MarkedString "haskell" (T.unpack $ trText ti)) tis
                        tr = head tis
                        range = J.Range (posToPosition $ trStart tr) (posToPosition $ trEnd tr)
                  rspMsg = Core.makeResponseMessage (J.responseId $ J._id (req :: J.HoverRequest) ) ht
                reactorSend rspMsg

              Core.ReqExecuteCommand req -> hieResponseHelper req res $ \r -> do
                let
                  reply v = reactorSend $ Core.makeResponseMessage (J.responseId $ J._id (req :: J.ExecuteCommandRequest)) v
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

requestDiagnostics :: TChan ChannelRequest -> TChan ChannelResponse -> String -> Core.OutMessage -> R ()
requestDiagnostics cin cout fileName req = do
  -- get hlint diagnostics
  ridl <- nextReqId
  let reql = CReq "applyrefact" ridl (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
  liftIO $ atomically $ writeTChan cin reql
  keepOriginal ridl req

  -- get GHC diagnostics
  ridg <- nextReqId
  let reqg = CReq "ghcmod" ridg (IdeRequest "check" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
  liftIO $ atomically $ writeTChan cin reqg
  keepOriginal ridg req

-- ---------------------------------------------------------------------

publishDiagnostics :: PluginId -> J.Object -> R ()
publishDiagnostics pid r = do
  let
    sendOne p =
      reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just p)
    mkDiag (f,ds) = do
      af <- liftIO $ makeAbsolute f
      return $ jsWrite $ FileDiagnostics ("file://" ++ af) ds
  -- liftIO $ U.logs $ "publishDiagnostics:pid=" ++ T.unpack pid
  -- cwd <- liftIO getCurrentDirectory
  -- liftIO $ U.logs $ "publishDiagnostics:cwd=" ++ cwd
  case pid of
    "applyrefact" -> sendOne r
    "ghcmod"      -> do
      case J.parse jsRead r of
        J.Success str -> do
          let pd = parseGhcDiagnostics str
          ds <- mapM mkDiag $ Map.toList $ Map.fromListWith (++) pd
          mapM_ sendOne ds
        _             -> return ()
    _ -> do
      liftIO $ U.logs $ "\n\npublishDiagnostics:not processing plugin=" ++ (T.unpack pid) ++ "\n\n"
      return ()


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

hieOptions :: Core.Options
hieOptions = def { Core.textDocumentSync = Just J.TdSyncIncremental
                 , Core.completionProvider = Just (J.CompletionOptions (Just True) Nothing)
                 , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["applyrefact:applyOne","hare:demote"]))
                 }


hieHandlers :: TChan ReactorInput -> Core.Handlers
hieHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin Core.NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin Core.ReqRename
        , Core.hoverHandler                             = Just $ passHandler rin Core.ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin Core.NotDidOpenTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin Core.NotDidSaveTextDocument
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin Core.NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin Core.NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin Core.NotCancelRequest
        , Core.responseHandler                          = Just $ responseHandlerCb rin
        , Core.codeActionHandler                        = Just $ passHandler rin Core.ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin Core.ReqExecuteCommand
        , Core.completionHandler                        = Just $ passHandler rin Core.ReqCompletion
        , Core.completionResolveHandler                 = Just $ passHandler rin Core.ReqCompletionItemResolve
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> Core.OutMessage) -> Core.Handler a
passHandler rin c vf sf notification = do
  atomically $ writeTChan rin (HandlerRequest vf sf (c notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin _vf _sf resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------

-- TODO: perhaps move this somewhere else, for general use
refactorResultToWorkspaceEdit :: RefactorResult -> J.WorkspaceEdit
refactorResultToWorkspaceEdit (RefactorResult diffs) = J.WorkspaceEdit (Just r) Nothing
  where
    r = H.fromList $ map hieDiffToLspEdit diffs

-- TODO: perhaps move this somewhere else, for general use
hieDiffToLspEdit :: HieDiff -> (T.Text, J.List (J.TextEdit))
hieDiffToLspEdit (HieDiff f _ d) = (T.pack ("file://" ++ f), J.List r)
  where
    pd = parsePrettyDiffs d
    r = map diffOperationToTextEdit pd

    {-
    hie: hieDiffToLspEdit:pd=
     [Change (LineRange {lrNumbers = (8,8), lrContents = ["baz = do"]})
             (LineRange {lrNumbers = (8,8), lrContents = ["baz ="]})]
    -}

    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = init $ unlines $ lrContents to

    diffOperationToTextEdit (Deletion fm _) = J.TextEdit range ""
      where
        range = calcRange fm

    diffOperationToTextEdit (Addition fm _) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = unlines $ lrContents fm


    calcRange fm = J.Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = J.Position (sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = length $ last $ lrContents fm
        e = J.Position (el - 1) ec  -- Note: zero-based lines
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
-- ---------------------------------------------------------------------
-- parsec parser for GHC error messages

type P = Parsec String ()

parseGhcDiagnostics :: T.Text -> [(FilePath,[Diagnostic])]
parseGhcDiagnostics str =
  case parse diagnostics "inp" (T.unpack str) of
    Left err -> error $ "parseGhcDiagnostics: got error" ++ show err
    Right ds -> ds

diagnostics :: P [(FilePath, [Diagnostic])]
diagnostics = (sepEndBy diagnostic (char '\n')) <* eof

diagnostic :: P (FilePath,[Diagnostic])
diagnostic = do
  fname <- many1 (noneOf ":")
  _ <- char ':'
  l <- number
  _ <- char ':'
  c <- number
  _ <- char ':'
  severity <- optionSeverity
  msglines <- sepEndBy (many1 (noneOf "\n\0")) (char '\0')
  let pos = (Position (l-1) (c-1))
  -- AZ:TODO: consider setting pprCols dflag value in the call, for better format on vscode
  return (fname,[Diagnostic (Range pos pos) (Just severity) Nothing (Just "ghcmod") (unlines msglines)] )

optionSeverity :: P DiagnosticSeverity
optionSeverity =
  (string "Warning:" >> return DsWarning)
  <|> (string "Error:" >> return DsError)
  <|> return DsError

number :: P Int
number = do
  s <- many1 digit
  return $ read s

-- ---------------------------------------------------------------------

