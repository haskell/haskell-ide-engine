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
import           Control.Lens ( (^.) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.State.Lazy
import qualified Data.Aeson as J
import           Data.Aeson ( (.=) )
import qualified Data.Aeson.Types as J
import           Data.Algorithm.DiffOutput
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Either
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as Core
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import           System.Directory
import           System.Exit
import           System.FilePath
import qualified System.Log.Logger as L
import           Text.Parsec
-- import qualified Yi.Rope as Yi

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport :: IO () -> TChan ChannelRequest -> FilePath -> IO ()
lspStdioTransport hieDispatcherProc cin origDir = do
  run hieDispatcherProc cin origDir >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: IO () -> TChan ChannelRequest -> FilePath -> IO Int
run dispatcherProc cin origDir = flip E.catches handlers $ do

  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  _rhpid <- forkIO $ responseHandler cout rin
  _rpid  <- forkIO $ reactor def cin cout rin

  let
    dp lf = do
      atomically $ writeTChan rin (InitializeCallBack lf)
      dispatcherProc
      return Nothing

  flip E.finally finalProc $ do
    tmpDir <- getTemporaryDirectory
    let logDir = tmpDir </> "hie-logs"
    createDirectoryIfMissing True logDir
    let dirStr = map (\c -> if c == pathSeparator then '-' else c) origDir
    -- (logFileName,handle) <- openTempFile logDir "hie-lsp.log"
    -- hClose handle -- Logger will open the file again
    let logFileName = logDir </> (dirStr ++ "-hie.log")
    Core.setupLogger logFileName L.DEBUG
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
  | InitializeCallBack Core.LspFuncs
  | HandlerRequest Core.LspFuncs Core.OutMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

data ReactorState =
  ReactorState
    { lspFuncs           :: !(Maybe Core.LspFuncs)
    , hieReqId           :: !RequestId
    , lspReqId           :: !J.LspId
    , wip                :: !(Map.Map RequestId Core.OutMessage)
    }

instance Default ReactorState where
  def = ReactorState Nothing 0 (J.IdInt 0) Map.empty

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R a = StateT ReactorState IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

setLspFuncs :: Core.LspFuncs -> R ()
setLspFuncs lf = modify' (\s -> s {lspFuncs = Just lf})

withLspFuncs :: (Core.LspFuncs -> R a) -> R a
withLspFuncs f = do
  s <- gets lspFuncs
  case s of
    Nothing -> error "reactorSend: send function not initialised yet"
    Just lf -> f lf

-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a) => a -> R ()
reactorSend msg = do
    withLspFuncs $ \lf -> liftIO $ (Core.sendFunc lf) (J.encode msg)

-- ---------------------------------------------------------------------

reactorSend' :: ((BSL.ByteString -> IO ()) -> IO ()) -> R ()
reactorSend' f = do
    withLspFuncs $ \lf -> liftIO $ f (Core.sendFunc lf) 

  -- msf <- gets sender
  -- case msf of
  --   Nothing -> error "reactorSend': send function not initialised yet"
  --   Just sf -> liftIO $ f sf

-- ---------------------------------------------------------------------

publishDiagnostics :: J.Uri -> Maybe J.TextDocumentVersion -> [J.Diagnostic] -> R ()
publishDiagnostics uri' mv diags = do
    withLspFuncs $ \lf -> liftIO $ (Core.publishDiagnosticsFunc lf) uri' mv diags

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
      InitializeCallBack lf@(Core.LspFuncs capabilities _sf _vf _dpf) -> do
        liftIO $ U.logs $ "reactor:got Client capabilities:" ++ show capabilities
        setLspFuncs lf

      HandlerRequest _lf (Core.RspFromClient rm) -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) (Core.NotInitialized _notification) -> do
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
          options = J.object ["documentSelector" .= J.object [ "language" .= J.String "haskell"]]
          registration = J.Registration "hare:demote" "workspace/executeCommand" (Just options)
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId

        reactorSend $ fmServerRegisterCapabilityRequest rid registrations

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) n@(Core.NotDidOpenTextDocument notification) -> do
        liftIO $ U.logm $ "****** reactor: processing NotDidOpenTextDocument"
        let
            doc = notification ^. J.params . J.textDocument . J.uri
            fileName = drop (length ("file://"::String)) doc

        requestDiagnostics cin cout fileName n

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) n@(Core.NotDidSaveTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            doc = notification ^. J.params . J.textDocument . J.uri
            fileName = drop (length ("file://"::String)) doc
        requestDiagnostics cin cout fileName n

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) (Core.NotDidChangeTextDocument _notification) -> do
        liftIO $ U.logm "****** reactor: NOT processing NotDidChangeTextDocument"

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) r@(Core.ReqRename req) -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            fileName = drop (length ("file://"::String)) doc
            J.Position l c = params ^. J.position
            newName  = params ^. J.newName
        rid <- nextReqId
        let hreq = CReq "hare" rid (IdeRequest "rename" (Map.fromList
                                                    [("file",     ParamFileP (T.pack fileName))
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
                                                    ,("name",     ParamValP $ ParamText (T.pack newName))
                                                    ])) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r


      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) r@(Core.ReqHover req) -> do
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let params = req ^. J.params 
            fileName = drop (length ("file://"::String)) $ params ^. J.textDocument . J.uri
            J.Position l c = params ^. J.position
        rid <- nextReqId
        let hreq = CReq "ghcmod" rid (IdeRequest "type" (Map.fromList
                                                    [("file",     ParamFileP (T.pack fileName))
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
                                                    ])) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) (Core.ReqCodeAction req) -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            (J.List diags) = params ^. J.context . J.diagnostics

        let
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "hlint") _m  ) = [J.Command title cmd cmdparams]
            where
              title = "Apply hint:" ++ head (lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See hieOptions above
              cmd = "applyrefact:applyOne"
              -- need 'file' and 'start_pos'
              args = J.Array$ V.fromList
                      [ J.object ["file" .= J.object ["textDocument" .= doc]]
                      , J.object ["start_pos" .= J.object ["position" .= start]]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m  ) = []
          -- TODO: make context specific commands for all sorts of things, such as refactorings
        let body = concatMap makeCommand diags
        let rspMsg = Core.makeResponseMessage (J.responseId $ req ^. J.id ) body
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) r@(Core.ReqExecuteCommand req) -> do
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" -- ++ show req
        -- cwd <- liftIO getCurrentDirectory
        -- liftIO $ U.logs $ "reactor:cwd:" ++ cwd
        let params = req ^. J.params
            command = params ^. J.command
            margs = params ^. J.arguments

        -- liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
        cmdparams <- case margs of
              Nothing -> return []
              Just (J.List os) -> do
                let (lts,rts) = partitionEithers $ map convertParam os
                -- TODO:AZ: return an error if any parse errors found.
                unless (null lts) $
                  liftIO $ U.logs $ "\n\n****reactor:ExecuteCommandRequest:error converting params=" ++ show lts ++ "\n\n"
                return rts

        rid <- nextReqId
        let (plugin,cmd) = break (==':') command
        let hreq = CReq (T.pack plugin) rid (IdeRequest (T.pack $ tail cmd) (Map.fromList
                                                    cmdparams)) cout
        liftIO $ atomically $ writeTChan cin hreq
        keepOriginal rid r

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) (Core.ReqCompletion req) -> do
        liftIO $ U.logs $ "reactor:got CompletionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            fileName = drop (length ("file://"::String)) $ doc ^. J.uri
            J.Position l c = params ^. J.position
        -- rid <- nextReqId
        -- let hreq = CReq "ghcmod" rid (IdeRequest "type" (Map.fromList
        --                                             [("file",     ParamFileP (T.pack fileName))
        --                                             ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
        --                                             ])) cout
        -- liftIO $ atomically $ writeTChan cin hreq
        -- keepOriginal rid r
        liftIO $ U.logs $ "****reactor:ReqCompletion:not immplemented=" ++ show (fileName,doc,l,c)

        let cr = J.Completions (J.List []) -- ( [] :: [J.CompletionListType])
        let rspMsg = Core.makeResponseMessage (J.responseId $ req ^. J.id ) cr
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) (Core.ReqDocumentHighlights req) -> do
        liftIO $ U.logs $ "reactor:got DocumentHighlightsRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            fileName = drop (length ("file://"::String)) $ doc ^. J.uri
            J.Position l c = params ^. J.position
        -- rid <- nextReqId
        -- let hreq = CReq "ghcmod" rid (IdeRequest "type" (Map.fromList
        --                                             [("file",     ParamFileP (T.pack fileName))
        --                                             ,("start_pos",ParamValP $ ParamPos (toPos (l+1,c+1)))
        --                                             ])) cout
        -- liftIO $ atomically $ writeTChan cin hreq
        -- keepOriginal rid r
        liftIO $ U.logs $ "****reactor:ReqDocumentHighlights:not immplemented=" ++ show (fileName,doc,l,c)

        let cr = J.List  ([] :: [J.DocumentHighlight])
        let rspMsg = Core.makeResponseMessage (J.responseId $ req ^. J.id ) cr
        reactorSend rspMsg

      -- -------------------------------

      HandlerRequest (Core.LspFuncs _c _sf _vf _pd) om -> do
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
                  IdeResponseOk r -> publishDiagnosticsToLsp pid r

              Core.NotDidSaveTextDocument _ -> do
                case res of
                  IdeResponseFail  err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseError err -> liftIO $ U.logs $ "NotDidSaveTextDocument:got err" ++ show err
                  IdeResponseOk r -> publishDiagnosticsToLsp pid r

              Core.ReqRename req -> hieResponseHelper req res $ \r -> do
                let J.Success vv = J.fromJSON (J.Object r) :: J.Result RefactorResult
                let we = refactorResultToWorkspaceEdit vv
                let rspMsg = Core.makeResponseMessage (J.responseId $ req ^. J.id ) we
                reactorSend rspMsg

              Core.ReqHover req -> hieResponseHelper req res $ \r -> do
                let
                  J.Success (TypeInfo mtis) = J.fromJSON (J.Object r) :: J.Result TypeInfo
                  ht = case mtis of
                    []  -> J.Hover (J.List []) Nothing
                    tis -> J.Hover (J.List ms) (Just range)
                      where
                        ms = map (\ti -> J.MarkedString "haskell" (T.unpack $ trText ti)) tis
                        tr = head tis
                        range = J.Range (posToPosition $ trStart tr) (posToPosition $ trEnd tr)
                  rspMsg = Core.makeResponseMessage ( J.responseId $ req ^. J.id ) ht
                reactorSend rspMsg

              Core.ReqExecuteCommand req -> hieResponseHelper req res $ \r -> do
                let
                  reply v = reactorSend $ Core.makeResponseMessage ( J.responseId $ req ^. J.id ) v
                -- When we get a RefactorResult or HieDiff, we need to send a
                -- separate WorkspaceEdit Notification
                liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
                case toWorkspaceEdit r of
                  Just we -> do
                    reply (J.Object mempty)
                    lid <- nextLspReqId
                    reactorSend $ fmServerApplyWorkspaceEditRequest lid we
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

publishDiagnosticsToLsp :: PluginId -> J.Object -> R ()
publishDiagnosticsToLsp pid r = do
  let
    sendOne (uri',ds) =
      publishDiagnostics uri' Nothing ds
    mkDiag (f,ds) = do
      af <- liftIO $ makeAbsolute f
      -- return $ jsWrite $ FileDiagnostics ("file://" ++ af) ds
      return ("file://" ++ af, ds)
  case pid of
    "applyrefact" -> do
      case J.fromJSON (J.Object r) of
        J.Success (J.PublishDiagnosticsParams fp (J.List ds)) -> sendOne (fp,ds)
        _  -> return ()
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
lspParam2ParamValP (LspText         txt                       ) = ParamTextP txt

data LspParam
  = LspTextDocument TextDocumentIdentifier
  | LspPosition     Position
  | LspRange        Range
  | LspText         T.Text
  deriving (Read,Show,Eq)

instance J.FromJSON LspParam where
  parseJSON (J.Object hm) =
    case H.toList hm of
      [("textDocument",v)] -> LspTextDocument <$> J.parseJSON v
      [("position",v)]     -> LspPosition     <$> J.parseJSON v
      [("range-pos",v)]    -> LspRange        <$> J.parseJSON v
      [("text",v)]         -> LspText         <$> J.parseJSON v
      _ -> fail $ "FromJSON.LspParam got:" ++ show hm
  parseJSON _ = mempty

-- ---------------------------------------------------------------------

-- | Manage the boilerplate for passing on any errors found in the IdeResponse
hieResponseHelper :: forall a t. J.RequestMessage a -> IdeResponse t -> (t -> R ()) -> R ()
hieResponseHelper req res action =
  case res of
    IdeResponseFail  err -> sendErrorResponse (req ^. J.id) J.InternalError (show err)
    IdeResponseError err -> sendErrorResponse (req ^. J.id) J.InternalError (show err)
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
        , Core.documentHighlightHandler                 = Just $ passHandler rin Core.ReqDocumentHighlights
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> Core.OutMessage) -> Core.Handler a
passHandler rin c lf notification = do
  atomically $ writeTChan rin (HandlerRequest lf (c notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin _lf resp = do
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

parseGhcDiagnostics :: T.Text -> [(FilePath,[J.Diagnostic])]
parseGhcDiagnostics str =
  case parse diagnostics "inp" (T.unpack str) of
    Left err -> error $ "parseGhcDiagnostics: got error" ++ show err
    Right ds -> ds

diagnostics :: P [(FilePath, [J.Diagnostic])]
diagnostics = (sepEndBy diagnostic (char '\n')) <* eof

diagnostic :: P (FilePath,[J.Diagnostic])
diagnostic = do
  fname <- many1 (noneOf ":")
  _ <- char ':'
  l <- number
  _ <- char ':'
  c <- number
  _ <- char ':'
  severity <- optionSeverity
  msglines <- sepEndBy (many1 (noneOf "\n\0")) (char '\0')
  let pos = (J.Position (l-1) (c-1))
  -- AZ:TODO: consider setting pprCols dflag value in the call, for better format on vscode
  return (fname,[J.Diagnostic (J.Range pos pos) (Just severity) Nothing (Just "ghcmod") (unlines msglines)] )

optionSeverity :: P J.DiagnosticSeverity
optionSeverity =
  (string "Warning:" >> return J.DsWarning)
  <|> (string "Error:" >> return J.DsError)
  <|> return J.DsError

number :: P Int
number = do
  s <- many1 digit
  return $ read s

-- ---------------------------------------------------------------------

