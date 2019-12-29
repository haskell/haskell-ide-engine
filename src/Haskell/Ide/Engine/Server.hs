{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Haskell.Ide.Engine.Server
  (
    server
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import qualified Control.FoldDebounce as Debounce
import           Control.Lens ( (^.) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Aeson as A
import           Control.Monad.STM
import           Data.Aeson ( (.=) )
import qualified Data.ByteString.Lazy as BL
import           Data.Coerce (coerce)
import           Data.Default
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Semigroup (Semigroup(..), Option(..), option)
import qualified Data.Set as S
import qualified Data.SortedList as SL
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Yaml as Yaml
import           Haskell.Ide.Engine.Cradle (findLocalCradle, cradleDisplay
                                           , isCabalCradle)
import           Haskell.Ide.Engine.Config
import qualified Haskell.Ide.Engine.Ghc   as HIE
import           Haskell.Ide.Engine.CodeActions
import qualified Haskell.Ide.Engine.Completions      as Completions
import           Haskell.Ide.Engine.Reactor
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Plugin.ApplyRefact   as ApplyRefact
-- import qualified Haskell.Ide.Engine.Plugin.HaRe          as HaRe
import qualified Haskell.Ide.Engine.Support.Hoogle        as Hoogle
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Scheduler            as Scheduler
import qualified Haskell.Ide.Engine.Support.HieExtras     as Hie
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.Version
import qualified Language.Haskell.LSP.Control            as CTRL
import qualified Language.Haskell.LSP.Core               as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types              as J
import           Language.Haskell.LSP.Types.Capabilities as C
import qualified Language.Haskell.LSP.Types.Lens         as J
import qualified Language.Haskell.LSP.Utility            as U
import qualified Language.Haskell.LSP.VFS                as VFS
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))
import           System.Exit
import qualified System.Log.Logger                       as L
import GHC.Conc

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
{-# ANN module ("hlint: ignore Use tuple-section" :: String) #-}
-- ---------------------------------------------------------------------

server
  :: Scheduler.Scheduler R
  -> FilePath
  -> IdePlugins
  -> Maybe FilePath
  -> IO ()
server scheduler origDir plugins captureFp = do
  run scheduler origDir plugins captureFp >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

-- | A request to compile a run diagnostics on a file
data DiagnosticsRequest = DiagnosticsRequest
  { trigger         :: DiagnosticTrigger
    -- ^ The type of event that is triggering the diagnostics

  , trackingNumber  :: TrackingNumber
    -- ^ The tracking identifier for this request

  , file         :: Uri
    -- ^ The file that was change and needs to be checked

  , documentVersion :: J.TextDocumentVersion
    -- ^ The current version of the document at the time of this request
  }

-- | Represents the most recent occurrence of a certin event. We use this
-- to diagnostics requests and only dispatch the most recent one.
newtype MostRecent a = MostRecent a

instance Semigroup (MostRecent a) where
  _ <> b = b

run
  :: Scheduler.Scheduler R
  -> FilePath
  -> IdePlugins
  -> Maybe FilePath
  -> IO Int
run scheduler _origDir plugins captureFp = flip E.catches handlers $ do

  rin        <- atomically newTChan :: IO (TChan ReactorInput)
  commandIds <- allLspCmdIds plugins

  let onStartup lf = do
        diagIn      <- atomically newTChan
        let react = runReactor lf scheduler diagnosticProviders hps sps fps plugins
            reactorFunc = react $ reactor rin diagIn

        let errorHandler :: Scheduler.ErrorHandler
            errorHandler (Just lid) code e =
              Core.sendErrorResponseS (Core.sendFunc lf) (J.responseId lid) code e
            errorHandler Nothing _code e =
              Core.sendErrorShowS (Core.sendFunc lf) e

            callbackHandler :: Scheduler.CallbackHandler R
            callbackHandler f x = react $ f x

        -- This is the callback the debouncer executes at the end of the timeout,
        -- it executes the diagnostics for the most recent request.
        let dispatchDiagnostics :: Option (MostRecent DiagnosticsRequest) -> R ()
            dispatchDiagnostics req = option (pure ()) (requestDiagnostics . coerce) req

        -- Debounces messages published to the diagnostics channel.
        let diagnosticsQueue tr = forever $ do
              inval <- liftIO $ atomically $ readTChan diagIn
              Debounce.send tr (coerce . Just $ MostRecent inval)

        -- Debounce for (default) 350ms.
        debounceDuration <- diagnosticsDebounceDuration . fromMaybe def <$> Core.config lf
        tr <- Debounce.new
          (Debounce.forMonoid $ react . dispatchDiagnostics)
          (Debounce.def { Debounce.delay = debounceDuration, Debounce.alwaysResetTimer = True })


        let lspRootDir = Core.rootPath lf
        currentDir <- liftIO getCurrentDirectory

        -- Check for mismatching GHC versions
        let dummyCradleFile = fromMaybe currentDir lspRootDir </> "File.hs"
        debugm $ "Dummy Cradle file result: " ++ dummyCradleFile
        cradleRes <- liftIO $ E.try (findLocalCradle dummyCradleFile)
        let sf = Core.sendFunc lf

        case cradleRes of
          Right cradle -> do
            projGhcVersion <- liftIO $ getProjectGhcVersion cradle
            when (projGhcVersion /= hieGhcVersion) $ do
              let msg = T.pack $ "Mismatching GHC versions: " ++ cradleDisplay cradle ++
                        " is " ++ projGhcVersion ++ ", HIE is " ++ hieGhcVersion
                        ++ "\nYou may want to use hie-wrapper. Check the README for more information"
              sf $ NotShowMessage $ fmServerShowMessageNotification J.MtWarning msg
              sf $ NotLogMessage $ fmServerLogMessageNotification J.MtWarning msg

              -- Check cabal is installed
              when (isCabalCradle cradle) $ do
                hasCabal <- liftIO checkCabalInstall
                unless hasCabal $ do
                  let cabalMsg = T.pack "cabal-install is not installed. Check the README for more information"
                  sf $ NotShowMessage $ fmServerShowMessageNotification J.MtWarning cabalMsg
                  sf $ NotLogMessage $ fmServerLogMessageNotification J.MtWarning cabalMsg

          Left (e :: Yaml.ParseException) -> do
            logm $ "Failed to parse `hie.yaml`: " ++ show e
            sf $ NotShowMessage
               $ fmServerShowMessageNotification
                  J.MtError
                    (  "Couldn't parse hie.yaml: \n"
                    <> T.pack (Yaml.prettyPrintParseException e)
                    )

        let mcradle = case cradleRes of
              Left _ -> Nothing
              Right c -> Just c

        -- haskell lsp sets the current directory to the project root in the InitializeRequest
        -- We launch the dispatcher after that so that the default cradle is
        -- recognized properly by ghc-mod
        flip labelThread "scheduler" =<<
            (forkIO (
              Scheduler.runScheduler scheduler errorHandler callbackHandler lf mcradle
              `E.catch` \(e :: E.SomeException) ->
              (errorm $ "Scheduler thread exited unexpectedly: " ++ show e)
            ))
        flip labelThread "reactor" =<<
            (forkIO (
              reactorFunc
              `E.catch` \(e :: E.SomeException) ->
              (errorm $ "Reactor thread exited unexpectedly: " ++ show e)
            ))
        flip labelThread "diagnostics" =<<
            (forkIO (
              diagnosticsQueue tr
              `E.catch` \(e :: E.SomeException) ->
              (errorm $ "Diagnostic thread exited unexpectedly: " ++ show e)
            ))

        return Nothing

      diagnosticProviders :: Map.Map DiagnosticTrigger [(PluginId,DiagnosticProviderFunc)]
      diagnosticProviders = Map.fromListWith (++) $ concatMap explode providers
        where
          explode :: (PluginId,DiagnosticProvider) -> [(DiagnosticTrigger,[(PluginId,DiagnosticProviderFunc)])]
          explode (pid,DiagnosticProvider tr f) = map (\t -> (t,[(pid,f)])) $ S.elems tr

          providers :: [(PluginId,DiagnosticProvider)]
          providers = concatMap pp $ Map.toList (ipMap plugins)

          pp (p,pd) = case pluginDiagnosticProvider pd of
            Nothing -> []
            Just dpf -> [(p,dpf)]

      hps :: [HoverProvider]
      hps = mapMaybe pluginHoverProvider $ Map.elems $ ipMap plugins

      sps :: [SymbolProvider]
      sps = mapMaybe pluginSymbolProvider $ Map.elems $ ipMap plugins

      fps :: Map.Map PluginId FormattingProvider
      fps = Map.mapMaybe pluginFormattingProvider $ ipMap plugins

      initCallbacks :: Core.InitializeCallbacks Config
      initCallbacks = Core.InitializeCallbacks getInitialConfig getConfigFromNotification onStartup

  flip E.finally finalProc $ do
    CTRL.run initCallbacks (hieHandlers rin) (hieOptions commandIds) captureFp
 where
  handlers  = [E.Handler ioExcept, E.Handler someExcept]
  finalProc = L.removeAllHandlers
  ioExcept (e :: E.IOException) = print e >> return 1
  someExcept (e :: E.SomeException) = print e >> return 1

-- ---------------------------------------------------------------------

type ReactorInput
  = FromClientMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------

configVal :: (Config -> c) -> R c
configVal field = field <$> getClientConfig

-- ---------------------------------------------------------------------

getPrefixAtPos :: (MonadIO m, MonadReader REnv m)
  => Uri -> Position -> m (Maybe Hie.PosPrefixInfo)
getPrefixAtPos uri pos = do
  mvf <- liftIO =<< asksLspFuncs Core.getVirtualFileFunc <*> pure (J.toNormalizedUri uri)
  case mvf of
    Just vf -> VFS.getCompletionPrefix pos vf
    Nothing -> return Nothing

-- ---------------------------------------------------------------------


-- TODO: generalise this and move it to GhcMod.ModuleLoader
updatePositionMap :: Uri -> [J.TextDocumentContentChangeEvent] -> IdeGhcM (IdeResult ())
updatePositionMap uri changes = pluginGetFile "updatePositionMap: " uri $ \file ->
  ifCachedInfo file (IdeResultOk ()) $ \info -> do
    let n2oOld = newPosToOld info
        o2nOld = oldPosToNew info
        (n2o,o2n) = foldl' go (n2oOld, o2nOld) changes
        go (n2o', o2n') (J.TextDocumentContentChangeEvent (Just r) _ txt) =
          (n2o' <=< newToOld r txt, oldToNew r txt <=< o2n')
        go _ _ = (const Nothing, const Nothing)
    let info' = info {newPosToOld = n2o, oldPosToNew = o2n}
    cacheInfoNoClear file info'
    return $ IdeResultOk ()
  where
    f (+/-) (J.Range (Position sl sc) (Position el ec)) txt p@(Position l c)

      -- pos is before the change - unaffected
      | l < sl = Just p
      -- pos is somewhere after the changed line,
      -- move down the pos to keep it the same
      | l > el = Just $ Position l' c

      {-
          LEGEND:
          0-9   char index
          x     untouched char
          I/i   inserted/replaced char
          .     deleted char
          ^     pos to be converted
      -}

      {-
          012345  67
          xxxxxx  xx
           ^
          0123456789
          xxIIIIiixx
           ^

          pos is unchanged if before the edited range
      -}
      | l == sl && c <= sc = Just p

      {-
          01234  56
          xxxxx  xx
            ^
          012345678
          xxIIIiixx
                 ^
          If pos is in the affected range move to after the range
      -}
      | l == sl && l == el && c <= nec && newL == 0 = Just $ Position l ec

      {-
          01234  56
          xxxxx  xx
                 ^
          012345678
          xxIIIiixx
                 ^
          If pos is after the affected range, update the char index
          to keep it in the same place
      -}
      | l == sl && l == el && c > nec && newL == 0 = Just $ Position l (c +/- (nec - sc))

      -- oh well we tried ¯\_(ツ)_/¯
      | otherwise = Nothing
         where l' = l +/- dl
               dl = newL - oldL
               oldL = el-sl
               newL = T.count "\n" txt
               nec -- new end column
                | newL == 0 = sc + T.length txt
                | otherwise = T.length $ last $ T.lines txt
    oldToNew = f (+)
    newToOld = f (-)

-- ---------------------------------------------------------------------

publishDiagnostics :: (MonadIO m, MonadReader REnv m)
  => Int -> J.NormalizedUri -> J.TextDocumentVersion -> DiagnosticsBySource -> m ()
publishDiagnostics maxToSend uri' mv diags = do
  lf <- asks lspFuncs
  liftIO $ Core.publishDiagnosticsFunc lf maxToSend uri' mv diags

-- ---------------------------------------------------------------------

flushDiagnosticsBySource :: (MonadIO m, MonadReader REnv m)
  => Int -> Maybe J.DiagnosticSource -> m ()
flushDiagnosticsBySource maxToSend msource = do
  lf <- asks lspFuncs
  liftIO $ Core.flushDiagnosticsBySourceFunc lf maxToSend msource

-- ---------------------------------------------------------------------

nextLspReqId :: (MonadIO m, MonadReader REnv m)
  => m J.LspId
nextLspReqId = do
  f <- asksLspFuncs Core.getNextReqId
  liftIO f

-- ---------------------------------------------------------------------

sendErrorLog :: (MonadIO m, MonadReader REnv m)
  => T.Text -> m ()
sendErrorLog msg = reactorSend' (`Core.sendErrorLogS` msg)

-- sendErrorShow :: String -> R ()
-- sendErrorShow msg = reactorSend' (\sf -> Core.sendErrorShowS sf msg)

-- ---------------------------------------------------------------------
-- reactor monad functions end
-- ---------------------------------------------------------------------


-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and hie dispatcher
reactor :: forall void. TChan ReactorInput -> TChan DiagnosticsRequest -> R void
reactor inp diagIn = do
  -- forever $ do
  let
    loop :: TrackingNumber -> R void
    loop tn = do
      liftIO $ U.logs $ "****** reactor: top of loop"
      inval <- liftIO $ atomically $ readTChan inp
      liftIO $ U.logs $ "****** reactor: got message number:" ++ show tn

      case inval of
        RspFromClient resp@(J.ResponseMessage _ _ _ merr) -> do
          liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show resp
          case merr of
            Nothing -> return ()
            Just _ -> sendErrorLog $ "Got error response:" <> decodeUtf8 (BL.toStrict $ A.encode resp)

        -- -------------------------------

        NotInitialized _notification -> do
          liftIO $ U.logm "****** reactor: processing Initialized Notification"
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

          -- TODO: Register all commands?
          hareId <- mkLspCmdId "hare" "demote"
          let
            options = A.object ["documentSelector" .= A.object [ "language" .= A.String "haskell"]]
            registrationsList =
              [ J.Registration hareId J.WorkspaceExecuteCommand (Just options)
              ]
          let registrations = J.RegistrationParams (J.List registrationsList)

          -- Do not actually register a command, but keep the code in
          -- place so we know how to do it when we actually need it.
          when False $ do
            rid <- nextLspReqId
            reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations

          reactorSend $ NotLogMessage $
                  fmServerLogMessageNotification J.MtLog $ "Using hie version: " <> T.pack hieVersion

          renv <- ask
          let hreq = GReq tn "init-hoogle" Nothing Nothing Nothing callback Nothing $ IdeResultOk <$> Hoogle.initializeHoogleDb
              callback Nothing = flip runReaderT renv $
                reactorSend $ NotShowMessage $
                  fmServerShowMessageNotification J.MtWarning "No hoogle db found. Check the README for instructions to generate one"
              callback (Just db) = flip runReaderT renv $ do
                reactorSend $ NotLogMessage $
                  fmServerLogMessageNotification J.MtLog $ "Using hoogle db at: " <> T.pack db
          makeRequest hreq

        -- -------------------------------

        NotDidOpenTextDocument notification -> do
          liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
          let
              td  = notification ^. J.params . J.textDocument
              uri = td ^. J.uri
              ver = td ^. J.version
          updateDocument uri ver
          -- We want to execute diagnostics for a newly opened file as soon as possible
          requestDiagnostics $ DiagnosticsRequest DiagnosticOnOpen tn uri (Just ver)

        -- -------------------------------

        NotDidChangeWatchedFiles _notification -> do
          liftIO $ U.logm "****** reactor: not processing NotDidChangeWatchedFiles"

        -- -------------------------------

        NotWillSaveTextDocument _notification -> do
          liftIO $ U.logm "****** reactor: not processing NotWillSaveTextDocument"

        -- -------------------------------

        NotDidSaveTextDocument notification -> do
          -- This notification is redundant, as we get the NotDidChangeTextDocument
          liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
          let
              td  = notification ^. J.params . J.textDocument
              uri = td ^. J.uri
          updateDocument uri 0
          -- don't debounce/queue diagnostics when saving
          requestDiagnostics (DiagnosticsRequest DiagnosticOnSave tn uri Nothing)

        -- -------------------------------

        NotDidChangeTextDocument notification -> do
          liftIO $ U.logm "****** reactor: processing NotDidChangeTextDocument"
          let
              params = notification ^. J.params
              vtdi = params ^. J.textDocument
              uri  = vtdi ^. J.uri
              ver  = vtdi ^. J.version
              J.List changes = params ^. J.contentChanges
          updateDocumentRequest uri (fromMaybe 0 ver) $ GReq tn "update-position" (Just uri) Nothing Nothing (const $ return ()) () $
            -- Important - Call this before requestDiagnostics
            updatePositionMap uri changes

          -- By default we don't run diagnostics on each change, unless configured
          -- by the client explicitly
          shouldRunDiag <- configVal diagnosticsOnChange
          when shouldRunDiag
               (queueDiagnosticsRequest diagIn DiagnosticOnChange tn uri ver)

        -- -------------------------------

        NotDidCloseTextDocument notification -> do
          liftIO $ U.logm "****** reactor: processing NotDidCloseTextDocument"
          let
              uri = notification ^. J.params . J.textDocument . J.uri
          -- unmapFileFromVfs versionTVar cin uri
          makeRequest $ GReq tn "delete-cache" (Just uri) Nothing Nothing (const $ return ()) () $ do
            forM_ (uriToFilePath uri)
              deleteCachedModule
            return $ IdeResultOk ()

        -- -------------------------------

        ReqRename req -> do
          liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
          -- TODO: re-enable HaRe
          -- let (params, doc, pos) = reqParams req
          --     newName  = params ^. J.newName
          --     callback = reactorSend . RspRename . Core.makeResponseMessage req
          -- let hreq = GReq tn "HaRe-rename" (Just doc) Nothing (Just $ req ^. J.id) callback mempty
          --              $ HaRe.renameCmd' doc pos newName
          -- makeRequest hreq
          reactorSend $ RspRename $ Core.makeResponseMessage req mempty

        -- -------------------------------

        ReqHover req -> do
          liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
          let params = req ^. J.params
              pos = params ^. J.position
              doc = params ^. J.textDocument . J.uri

          hps <- asks hoverProviders

          let callback :: [[J.Hover]] -> R ()
              callback hhs =
                -- TODO: We should support ServerCapabilities and declare that
                -- we don't support hover requests during initialization if we
                -- don't have any hover providers
                -- TODO: maybe only have provider give MarkedString and
                -- work out range here?
                let hs = concat hhs
                    h = case mconcat ((map (^. J.contents) hs) :: [J.HoverContents]) of
                      J.HoverContentsMS (List []) -> Nothing
                      hh                          -> Just $ J.Hover hh r
                    r = listToMaybe $ mapMaybe (^. J.range) hs
                in reactorSend $ RspHover $ Core.makeResponseMessage req h

              hreq :: PluginRequest R
              hreq = IReq tn "hover" (req ^. J.id) callback $
                sequence <$> mapM (\hp -> lift $ hp doc pos) hps
          makeRequest hreq
          liftIO $ U.logs "reactor:HoverRequest done"

        -- -------------------------------

        ReqCodeAction req -> do
          liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
          handleCodeActionReq tn req

        -- -------------------------------

        ReqExecuteCommand req -> do
          liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" ++ show req
          lf <- asks lspFuncs

          let params = req ^. J.params

              parseCmdId :: T.Text -> Maybe (PluginId, CommandId)
              parseCmdId x = case T.splitOn ":" x of
                [plugin, command] -> Just (PluginId plugin, CommandId command)
                [_, plugin, command] -> Just (PluginId plugin, CommandId command)
                _ -> Nothing

              callback obj = do
                liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show obj
                case fromDynJSON obj :: Maybe J.WorkspaceEdit of
                  Just v -> do
                    lid <- nextLspReqId
                    reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req (A.Object mempty)
                    let msg = fmServerApplyWorkspaceEditRequest lid $ J.ApplyWorkspaceEditParams v
                    liftIO $ U.logs $ "ExecuteCommand sending edit: " ++ show msg
                    reactorSend $ ReqApplyWorkspaceEdit msg
                  Nothing -> reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req $ dynToJSON obj

              execCmd cmdId args = do
                -- The parameters to the HIE command are always the first element
                let cmdParams = case args of
                     Just (J.List (x:_)) -> x
                     _ -> A.Null

                case parseCmdId cmdId of
                  -- Shortcut for immediately applying a applyWorkspaceEdit as a fallback for v3.8 code actions
                  Just ("hie", "fallbackCodeAction") -> do
                    case A.fromJSON cmdParams of
                      A.Success (FallbackCodeActionParams mEdit mCmd) -> do

                        -- Send off the workspace request if it has one
                        forM_ mEdit $ \edit -> do
                          lid <- nextLspReqId
                          let eParams = J.ApplyWorkspaceEditParams edit
                              eReq = fmServerApplyWorkspaceEditRequest lid eParams
                          reactorSend $ ReqApplyWorkspaceEdit eReq

                        case mCmd of
                          -- If we have a command, continue to execute it
                          Just (J.Command _ innerCmdId innerArgs) -> execCmd innerCmdId innerArgs

                          -- Otherwise we need to send back a response oureslves
                          Nothing -> reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req (A.Object mempty)

                      -- Couldn't parse the fallback command params
                      _ -> liftIO $
                        Core.sendErrorResponseS (Core.sendFunc lf)
                                                (J.responseId (req ^. J.id))
                                                J.InvalidParams
                                                "Invalid fallbackCodeAction params"
                  -- Just an ordinary HIE command
                  Just (plugin, cmd) ->
                    let preq = GReq tn "plugin" Nothing Nothing (Just $ req ^. J.id) callback (toDynJSON (Nothing :: Maybe J.WorkspaceEdit))
                               $ runPluginCommand plugin cmd cmdParams
                    in makeRequest preq

                  -- Couldn't parse the command identifier
                  _ -> liftIO $
                    Core.sendErrorResponseS (Core.sendFunc lf)
                                            (J.responseId (req ^. J.id))
                                            J.InvalidParams
                                            "Invalid command identifier"

          execCmd (params ^. J.command) (params ^. J.arguments)


        -- -------------------------------

        ReqCompletion req -> do
          liftIO $ U.logs $ "reactor:got CompletionRequest:" ++ show req
          let (_, doc, pos) = reqParams req

          mprefix <- getPrefixAtPos doc pos

          let callback compls = do
                let rspMsg = Core.makeResponseMessage req
                              $ J.Completions $ J.List compls
                reactorSend $ RspCompletion rspMsg
          case mprefix of
            Nothing -> callback []
            Just prefix -> do
              snippets <- Completions.WithSnippets <$> configVal completionSnippetsOn
              let hreq = IReq tn "completion" (req ^. J.id) callback
                           $ lift $ Completions.getCompletions doc prefix snippets
              makeRequest hreq

        ReqCompletionItemResolve req -> do
          liftIO $ U.logs $ "reactor:got CompletionItemResolveRequest:" ++ show req
          snippets <- Completions.WithSnippets <$> configVal completionSnippetsOn
          let origCompl = req ^. J.params
              callback res = do
                let rspMsg = Core.makeResponseMessage req $ res
                reactorSend $ RspCompletionItemResolve rspMsg
              hreq = IReq tn "completion" (req ^. J.id) callback $ runIdeResultT $ do
                lift $ lift $ Completions.resolveCompletion snippets origCompl
          makeRequest hreq

        -- -------------------------------

        ReqDocumentHighlights req -> do
          liftIO $ U.logs $ "reactor:got DocumentHighlightsRequest:" ++ show req
          let (_, doc, pos) = reqParams req
              callback = reactorSend . RspDocumentHighlights . Core.makeResponseMessage req . J.List
          let hreq = IReq tn "highlights" (req ^. J.id) callback
                   $ Hie.getReferencesInDoc doc pos
          makeRequest hreq

        -- -------------------------------

        ReqDefinition req -> do
          liftIO $ U.logs $ "reactor:got DefinitionRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
              pos = params ^. J.position
              callback = reactorSend . RspDefinition . Core.makeResponseMessage req
          let hreq = IReq tn "find-def" (req ^. J.id) callback
                       $ fmap J.MultiLoc <$> Hie.findDef doc pos
          makeRequest hreq

        ReqTypeDefinition req -> do
          liftIO $ U.logs $ "reactor:got DefinitionTypeRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
              pos = params ^. J.position
              callback = reactorSend . RspTypeDefinition . Core.makeResponseMessage req
          let hreq = IReq tn "type-def" (req ^. J.id) callback
                       $ fmap J.MultiLoc <$> Hie.findTypeDef doc pos
          makeRequest hreq

        ReqFindReferences req -> do
          liftIO $ U.logs $ "reactor:got FindReferences:" ++ show req
          -- TODO: implement project-wide references
          let (_, doc, pos) = reqParams req
              callback = reactorSend . RspFindReferences.  Core.makeResponseMessage req . J.List
          let hreq = IReq tn "references" (req ^. J.id) callback
                   $ fmap (map (J.Location doc . (^. J.range)))
                   <$> Hie.getReferencesInDoc doc pos
          makeRequest hreq

        -- -------------------------------

        ReqDocumentFormatting req -> do
          liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
          provider <- getFormattingProvider
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
          withDocumentContents (req ^. J.id) doc $ \text ->
            let callback = reactorSend . RspDocumentFormatting . Core.makeResponseMessage req . J.List
                hreq = IReq tn "format" (req ^. J.id) callback $ lift $ provider text doc FormatText (params ^. J.options)
              in makeRequest hreq

        -- -------------------------------

        ReqDocumentRangeFormatting req -> do
          liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
          provider <- getFormattingProvider
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
          withDocumentContents (req ^. J.id) doc $ \text ->
            let range = params ^. J.range
                callback = reactorSend . RspDocumentRangeFormatting . Core.makeResponseMessage req . J.List
                hreq = IReq tn "range-format" (req ^. J.id) callback $ lift $ provider text doc (FormatRange range) (params ^. J.options)
              in makeRequest hreq

        -- -------------------------------

        ReqDocumentSymbols req -> do
          liftIO $ U.logs $ "reactor:got Document symbol request:" ++ show req
          sps <- asks symbolProviders
          C.ClientCapabilities _ tdc _ _ <- asksLspFuncs Core.clientCapabilities
          let uri = req ^. J.params . J.textDocument . J.uri
              supportsHierarchy = fromMaybe False $ tdc >>= C._documentSymbol >>= C._hierarchicalDocumentSymbolSupport
              convertSymbols :: [J.DocumentSymbol] -> J.DSResult
              convertSymbols symbs
                | supportsHierarchy = J.DSDocumentSymbols $ J.List symbs
                | otherwise = J.DSSymbolInformation (J.List $ concatMap (go Nothing) symbs)
                where
                    go :: Maybe T.Text -> J.DocumentSymbol -> [J.SymbolInformation]
                    go parent ds =
                      let children = concatMap (go (Just name)) (fromMaybe mempty (ds ^. J.children))
                          loc = Location uri (ds ^. J.range)
                          name = ds ^. J.name
                          si = J.SymbolInformation name (ds ^. J.kind) (ds ^. J.deprecated) loc parent
                      in [si] <> children

              callback = reactorSend . RspDocumentSymbols . Core.makeResponseMessage req . convertSymbols . concat
          let hreq = IReq tn "symbols" (req ^. J.id) callback (sequence <$> mapM (\f -> f uri) sps)
          makeRequest hreq

        -- -------------------------------

        NotCancelRequestFromClient notif -> do
          liftIO $ U.logs $ "reactor:got CancelRequest:" ++ show notif
          let lid = notif ^. J.params . J.id
          cancelRequest lid

        -- -------------------------------

        NotDidChangeConfiguration notif -> do
          liftIO $ U.logs $ "reactor:didChangeConfiguration notification:" ++ show notif
          -- if hlint has been turned off, flush the diagnostics
          diagsOn              <- configVal hlintOn
          maxDiagnosticsToSend <- configVal maxNumberOfProblems
          liftIO $ U.logs $ "reactor:didChangeConfiguration diagsOn:" ++ show diagsOn
          -- If hlint is off, remove the diags. But make sure they get sent, in
          -- case maxDiagnosticsToSend has changed.
          if diagsOn
            then flushDiagnosticsBySource maxDiagnosticsToSend Nothing
            else flushDiagnosticsBySource maxDiagnosticsToSend (Just "hlint")

        -- -------------------------------
        om -> do
          liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om
      loop (tn + 1)

  -- Actually run the thing
  loop 0

-- ---------------------------------------------------------------------

-- | Execute a function in the current request with an Uri.
-- Reads the content of the file specified by the Uri and invokes
-- the function on it.
--
-- If the Uri can not be mapped to a real file, the function will
-- not be executed and an error message will be sent to the client.
-- Error message is associated with the request id and, thus, identifiable.
withDocumentContents :: J.LspId -> J.Uri -> (T.Text -> R ()) -> R ()
withDocumentContents reqId uri f = do
  vfsFunc <- asksLspFuncs Core.getVirtualFileFunc
  mvf <- liftIO $ vfsFunc (J.toNormalizedUri uri)
  lf <- asks lspFuncs
  case mvf of
    Nothing -> liftIO $
      Core.sendErrorResponseS (Core.sendFunc lf)
        (J.responseId reqId)
        J.InvalidRequest
        "Document was not open"
    Just vf -> f (VFS.virtualFileText vf)

-- | Get the currently configured formatter provider.
-- The currently configured formatter provider is defined in @Config@ by PluginId.
--
-- It is possible that formatter configured by the user is not present.
-- In this case, a nop (No-Operation) formatter is returned and a message will
-- be sent to the user.
getFormattingProvider :: R FormattingProvider
getFormattingProvider = do
  plugins <- asks idePlugins
  config <- getClientConfig
  -- LL: Is this overengineered? Do we need a pluginFormattingProvider
  -- or should we just call plugins straight from here based on the providerType?
  let providerName = formattingProvider config
      mprovider = Hie.getFormattingPlugin config plugins
  case mprovider of
    Nothing -> do
      unless (providerName == "none") $ do
        let msg = providerName <> " is not a recognised plugin for formatting. Check your config"
        reactorSend $ NotShowMessage $ fmServerShowMessageNotification J.MtWarning msg
        reactorSend $ NotLogMessage $ fmServerLogMessageNotification J.MtWarning msg
      return (\_ _ _ _ -> return (IdeResultOk [])) -- nop formatter
    Just (_, provider) -> return provider

-- ---------------------------------------------------------------------

-- | Queue a diagnostics request to be performed after a timeout. This prevents recompiling
-- too often when there is a quick stream of changes.
queueDiagnosticsRequest
  :: TChan DiagnosticsRequest -- ^ The channel to publish the diagnostics requests to
  -> DiagnosticTrigger
  -> TrackingNumber
  -> Uri
  -> J.TextDocumentVersion
  -> R ()
queueDiagnosticsRequest diagIn dt tn uri mVer =
  liftIO $ atomically $ writeTChan diagIn (DiagnosticsRequest dt tn uri mVer)


-- | Actually compile the file and perform diagnostics and then send the diagnostics
-- results back to the client
requestDiagnostics :: DiagnosticsRequest -> R ()
requestDiagnostics DiagnosticsRequest{trigger, file, trackingNumber, documentVersion} = do
  requestDiagnosticsNormal trackingNumber file documentVersion

  diagFuncs <- asks diagnosticSources
  lf <- asks lspFuncs
  clientConfig <- getClientConfig
  case Map.lookup trigger diagFuncs of
    Nothing -> do
      debugm $ "requestDiagnostics: no diagFunc for:" ++ show trigger
      return ()
    Just dss -> do
      dpsEnabled <- configVal getDiagnosticProvidersConfig
      debugm $ "requestDiagnostics: got diagFunc for:" ++ show trigger
      forM_ dss $ \(pid,ds) -> do
        debugm $ "requestDiagnostics: calling diagFunc for plugin:" ++ show pid
        let
          PluginId pid' = pid
          enabled = Map.findWithDefault True pid dpsEnabled
          publishDiagnosticsIO = Core.publishDiagnosticsFunc lf
          maxToSend = maxNumberOfProblems clientConfig
          sendOne (fileUri,ds') = do
            debugm $ "LspStdio.sendone:(fileUri,ds')=" ++ show(fileUri,ds')
            publishDiagnosticsIO maxToSend (J.toNormalizedUri fileUri) Nothing (Map.fromList [(Just pid',SL.toSortedList ds')])

          sendEmpty = do
            debugm "LspStdio.sendempty"
            publishDiagnosticsIO maxToSend (J.toNormalizedUri file) Nothing (Map.fromList [(Just pid',SL.toSortedList [])])

          -- fv = case documentVersion of
          --   Nothing -> Nothing
          --   Just v -> Just (file,v)
        -- let fakeId = J.IdString "fake,remove" -- TODO:AZ: IReq should take a Maybe LspId
        let fakeId = J.IdString ("fake,remove:pid=" <> pid') -- TODO:AZ: IReq should take a Maybe LspId
        let reql = case ds of
              DiagnosticProviderSync dps ->
                IReq trackingNumber "diagnostics" fakeId callbackl
                     $ dps trigger file
              DiagnosticProviderAsync dpa ->
                IReq trackingNumber "diagnostics-a" fakeId pure
                     $ dpa trigger file callbackl
            -- This callback is used in R for the dispatcher normally,
            -- but also in IO if the plugin chooses to spawn an
            -- external process that returns diagnostics when it
            -- completes.
            callbackl :: forall m. MonadIO m => Map.Map Uri (S.Set Diagnostic) -> m ()
            callbackl pd = do
              liftIO $ logm $ "LspStdio.callbackl called with pd=" ++ show pd
              let diags = Map.toList $ S.toList <$> pd
              case diags of
                [] -> liftIO sendEmpty
                _ -> mapM_ (liftIO . sendOne) diags
        when enabled $ makeRequest reql

-- | get hlint and GHC diagnostics and loads the typechecked module into the cache
requestDiagnosticsNormal :: TrackingNumber -> Uri -> J.TextDocumentVersion -> R ()
requestDiagnosticsNormal tn file mVer = do
  clientConfig <- getClientConfig
  let
    ver = fromMaybe 0 mVer

    -- | If there is a GHC error, flush the hlint diagnostics
    -- TODO: Just flush the parse error diagnostics
    sendOneGhc :: J.DiagnosticSource -> (J.NormalizedUri, [Diagnostic]) -> R ()
    sendOneGhc pid (fileUri,ds) = do
      if any (hasSeverity J.DsError) ds
        then publishDiagnostics maxToSend fileUri Nothing
               (Map.fromList [(Just "hlint",SL.toSortedList []),(Just pid,SL.toSortedList ds)])
        else sendOne pid (fileUri,ds)

    sendOne pid (fileUri,ds) = do
      publishDiagnostics maxToSend fileUri Nothing (Map.fromList [(Just pid,SL.toSortedList ds)])

    hasSeverity :: J.DiagnosticSeverity -> J.Diagnostic -> Bool
    hasSeverity sev (J.Diagnostic _ (Just s) _ _ _ _) = s == sev
    hasSeverity _ _ = False
    sendEmpty = publishDiagnostics maxToSend (J.toNormalizedUri file) Nothing (Map.fromList [(Just "bios",SL.toSortedList [])])
    maxToSend = maxNumberOfProblems clientConfig

  let sendHlint = hlintOn clientConfig
  when sendHlint $ do
    -- get hlint diagnostics
    let reql = GReq tn "apply-refact" (Just file) (Just (file,ver)) Nothing callbackl (PublishDiagnosticsParams file mempty)
                 $ ApplyRefact.lint file
        callbackl (PublishDiagnosticsParams fp (List ds))
             = sendOne "hlint" (J.toNormalizedUri fp, ds)
    makeRequest reql

  -- get GHC diagnostics and loads the typechecked module into the cache
  let reqg = GReq tn "typecheck" (Just file) (Just (file,ver)) Nothing callbackg mempty
               $ HIE.setTypecheckedModule file
      callbackg (HIE.Diagnostics pd, errs) = do
        forM_ errs $ \e -> do
          reactorSend $ NotShowMessage $
            fmServerShowMessageNotification J.MtError
              $ "Got error while processing diagnostics: " <> e
        let ds = Map.toList $ S.toList <$> pd
        case ds of
          [] -> sendEmpty
          _ -> do
            debugm ("Diags: " ++ show ds)
            mapM_ (sendOneGhc "bios") ds

  makeRequest reqg

-- ---------------------------------------------------------------------

reqParams ::
     (J.HasParams r p, J.HasTextDocument p i, J.HasUri i u, J.HasPosition p l)
  => r
  -> (p, u, l)
reqParams req = (params, doc, pos)
  where
    params = req ^. J.params
    doc = params ^. (J.textDocument . J.uri)
    pos = params ^. J.position

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

-- | Create 'Language.Haskell.LSP.Core.Options'.
-- There may need to be more options configured, depending on what handlers
-- are registered.
-- Consult the haskell-lsp haddocks to see all possible options.
hieOptions :: [T.Text] -> Core.Options
hieOptions commandIds =
  def { Core.textDocumentSync       = Just syncOptions
      -- The characters that trigger completion automatically.
      , Core.completionTriggerCharacters = Just ['.']
      -- As of 2018-05-24, vscode needs the commands to be registered
      -- otherwise they will not be available as codeActions (will be
      -- silently ignored, despite UI showing to the contrary).
      --
      -- Hopefully the end May 2018 vscode release will stabilise
      -- this, it is a major rework of the machinery anyway.
      , Core.executeCommandCommands = Just commandIds
      }


hieHandlers :: TChan ReactorInput -> Core.Handlers
hieHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
        -- , Core.renameHandler                            = Just $ passHandler rin ReqRename
        , Core.definitionHandler                        = Just $ passHandler rin ReqDefinition
        , Core.typeDefinitionHandler                    = Just $ passHandler rin ReqTypeDefinition
        , Core.referencesHandler                        = Just $ passHandler rin ReqFindReferences
        , Core.hoverHandler                             = Just $ passHandler rin ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
        , Core.willSaveTextDocumentNotificationHandler  = Just $ passHandler rin NotWillSaveTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
        , Core.didChangeWatchedFilesNotificationHandler = Just $ passHandler rin NotDidChangeWatchedFiles
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
        , Core.didChangeConfigurationParamsHandler      = Just $ passHandler rin NotDidChangeConfiguration
        , Core.responseHandler                          = Just $ passHandler rin RspFromClient
        , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
        , Core.completionHandler                        = Just $ passHandler rin ReqCompletion
        , Core.completionResolveHandler                 = Just $ passHandler rin ReqCompletionItemResolve
        , Core.documentHighlightHandler                 = Just $ passHandler rin ReqDocumentHighlights
        , Core.documentFormattingHandler                = Just $ passHandler rin ReqDocumentFormatting
        , Core.documentRangeFormattingHandler           = Just $ passHandler rin ReqDocumentRangeFormatting
        , Core.documentSymbolHandler                    = Just $ passHandler rin ReqDocumentSymbols
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (c notification)

-- ---------------------------------------------------------------------
