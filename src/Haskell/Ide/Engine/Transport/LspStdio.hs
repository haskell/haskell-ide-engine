{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Haskell.Ide.Engine.Transport.LspStdio
  (
    lspStdioTransport
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception as E
import           Control.Lens ( (^.), (.~) )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Reader
import qualified Data.Aeson as J
import           Data.Aeson ( (.=) )
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isUpper, isAlphaNum)
import           Data.Default
import           Data.Maybe
import           Data.Monoid ( (<>) )
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.SortedList as SL
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified GhcModCore               as GM
import qualified GhcMod.Monad.Types       as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.LSP.CodeActions
import           Haskell.Ide.Engine.LSP.Config
import           Haskell.Ide.Engine.LSP.Reactor
import qualified Haskell.Ide.Engine.Plugin.HaRe          as HaRe
import qualified Haskell.Ide.Engine.Plugin.GhcMod        as GhcMod
import qualified Haskell.Ide.Engine.Plugin.ApplyRefact   as ApplyRefact
import qualified Haskell.Ide.Engine.Plugin.Brittany      as Brittany
import qualified Haskell.Ide.Engine.Plugin.Hoogle        as Hoogle
import qualified Haskell.Ide.Engine.Plugin.Haddock       as Haddock
import qualified Haskell.Ide.Engine.Plugin.HieExtras     as Hie
import           Haskell.Ide.Engine.Plugin.Base
import qualified Language.Haskell.LSP.Control            as CTRL
import qualified Language.Haskell.LSP.Core               as Core
import qualified Language.Haskell.LSP.VFS                as VFS
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types              as J
import           Language.Haskell.LSP.Types.Capabilities as C
import qualified Language.Haskell.LSP.Utility            as U
import           System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi

import Name

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport
  :: (DispatcherEnv -> ErrorHandler -> CallbackHandler R -> ClientCapabilities -> IO ())
  -> TChan (PluginRequest R)
  -> FilePath
  -> IdePlugins
  -> Maybe FilePath
  -> IO ()
lspStdioTransport hieDispatcherProc cin origDir plugins captureFp = do
  run hieDispatcherProc cin origDir plugins captureFp >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

run
  :: (DispatcherEnv -> ErrorHandler -> CallbackHandler R -> ClientCapabilities -> IO ())
  -> TChan (PluginRequest R)
  -> FilePath
  -> IdePlugins
  -> Maybe FilePath
  -> IO Int
run dispatcherProc cin _origDir plugins captureFp = flip E.catches handlers $ do

  rin <- atomically newTChan :: IO (TChan ReactorInput)

  prefix <- cmdPrefixer

  let dp lf = do
        cancelTVar  <- atomically $ newTVar S.empty
        wipTVar     <- atomically $ newTVar S.empty
        versionTVar <- atomically $ newTVar Map.empty
        let dEnv = DispatcherEnv
              { cancelReqsTVar = cancelTVar
              , wipReqsTVar    = wipTVar
              , docVersionTVar = versionTVar
              }
        let reactorFunc = runReactor lf dEnv cin diagnosticProviders prefix $ reactor rin
            caps = Core.clientCapabilities lf

        let errorHandler :: ErrorHandler
            errorHandler lid code e =
              Core.sendErrorResponseS (Core.sendFunc lf) (J.responseId lid) code e
            callbackHandler :: CallbackHandler R
            callbackHandler f x = runReactor lf dEnv cin diagnosticProviders prefix $ f x


        -- haskell lsp sets the current directory to the project root in the InitializeRequest
        -- We launch the dispatcher after that so that the defualt cradle is
        -- recognized properly by ghc-mod
        _ <- forkIO $ race_ (dispatcherProc dEnv errorHandler callbackHandler caps) reactorFunc
        return Nothing

      commandIds = Map.foldlWithKey cmdFolder [] (pluginCommands <$> ipMap plugins)

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

      cmdFolder :: [T.Text] -> T.Text -> [PluginCommand] -> [T.Text]
      cmdFolder acc plugin cmds = acc ++ map prefix cmdIds
        where cmdIds = map (\cmd -> plugin <> ":" <> commandName cmd) cmds

  flip E.finally finalProc $ do
    CTRL.run (getConfigFromNotification, dp) (hieHandlers rin) (hieOptions commandIds) captureFp
 where
  handlers  = [E.Handler ioExcept, E.Handler someExcept]
  finalProc = L.removeAllHandlers
  ioExcept (e :: E.IOException) = print e >> return 1
  someExcept (e :: E.SomeException) = print e >> return 1
  -- TODO:AZ:This type should be something like :: IO (UnprefixedCommandId -> PrefixedCommandId)
  cmdPrefixer = do
    pid <- T.pack . show <$> getProcessID
    return ((pid <> ":") <>)

-- ---------------------------------------------------------------------

type ReactorInput
  = FromClientMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------

configVal :: c -> (Config -> c) -> R c
configVal defVal field = do
  gmc <- asksLspFuncs Core.config
  mc <- liftIO gmc
  return $ maybe defVal field mc

-- ---------------------------------------------------------------------

getPrefixAtPos :: (MonadIO m, MonadReader REnv m)
  => Uri -> Position -> m (Maybe (T.Text,T.Text))
getPrefixAtPos uri (Position l c) = do
  mvf <- liftIO =<< asksLspFuncs Core.getVirtualFileFunc <*> pure uri
  case mvf of
    Just (VFS.VirtualFile _ yitext) -> return $ do
      let headMaybe [] = Nothing
          headMaybe (x:_) = Just x
          lastMaybe [] = Nothing
          lastMaybe xs = Just $ last xs
      curLine <- headMaybe $ Yi.lines $ snd $ Yi.splitAtLine l yitext
      let beforePos = Yi.take c curLine
      curWord <- Yi.toText <$> lastMaybe (Yi.words beforePos)
      let parts = T.split (=='.')
                    $ T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("._'"::String)) curWord
      case reverse parts of
        [] -> Nothing
        (x:xs) -> do
          let modParts = dropWhile (not . isUpper . T.head)
                              $ reverse $ filter (not .T.null) xs
              modName = T.intercalate "." modParts
          return (modName,x)
    Nothing -> return Nothing

-- ---------------------------------------------------------------------


mapFileFromVfs :: (MonadIO m, MonadReader REnv m)
               => TrackingNumber
               -> J.VersionedTextDocumentIdentifier -> m ()
mapFileFromVfs tn vtdi = do
  verTVar <- asks (docVersionTVar . dispatcherEnv)
  cin <- asks reqChanIn
  let uri = vtdi ^. J.uri
      ver = fromMaybe 0 (vtdi ^. J.version)
  vfsFunc <- asksLspFuncs Core.getVirtualFileFunc
  mvf <- liftIO $ vfsFunc uri
  case (mvf, uriToFilePath uri) of
    (Just (VFS.VirtualFile _ yitext), Just fp) -> do
      let text' = Yi.toString yitext
          -- text = "{-# LINE 1 \"" ++ fp ++ "\"#-}\n" <> text'
      let req = GReq tn (Just uri) Nothing Nothing (const $ return ())
                  $ IdeResultOk <$> do
                      GM.loadMappedFileSource fp text'
                      fileMap <- GM.getMMappedFiles
                      debugm $ "file mapping state is: " ++ show fileMap
      liftIO $ atomically $ do
        modifyTVar' verTVar (Map.insert uri ver)
        writeTChan cin req
      return ()
    (_, _) -> return ()

_unmapFileFromVfs :: (MonadIO m, MonadReader REnv m) => TrackingNumber -> J.Uri -> m ()
_unmapFileFromVfs tn uri = do
  verTVar <- asks (docVersionTVar . dispatcherEnv)
  cin <- asks reqChanIn
  case J.uriToFilePath uri of
    Just fp -> do
      let req = GReq tn (Just uri) Nothing Nothing (const $ return ())
                $ IdeResultOk <$> GM.unloadMappedFile fp
      liftIO $ atomically $ do
        modifyTVar' verTVar (Map.delete uri)
        writeTChan cin req
      return ()
    _ -> return ()

-- TODO: generalise this and move it to GhcMod.ModuleLoader
updatePositionMap :: Uri -> [J.TextDocumentContentChangeEvent] -> IdeGhcM (IdeResult ())
updatePositionMap uri changes = pluginGetFile "updatePositionMap: " uri $ \file -> do
  mcm <- getCachedModule file
  case mcm of
    ModuleCached cm _ -> do
      let n2oOld = newPosToOld cm
          o2nOld = oldPosToNew cm
          (n2o,o2n) = foldl' go (n2oOld, o2nOld) changes
          go (n2o', o2n') (J.TextDocumentContentChangeEvent (Just r) _ txt) =
            (n2o' <=< newToOld r txt, oldToNew r txt <=< o2n')
          go _ _ = (const Nothing, const Nothing)
      let cm' = cm {newPosToOld = n2o, oldPosToNew = o2n}
      cacheModuleNoClear file cm'
      return $ IdeResultOk ()
    _ ->
      return $ IdeResultOk ()
  where
    f (+/-) (J.Range (Position sl _) (Position el _)) txt p@(Position l c)
      | l < sl = Just p
      | l > el = Just $ Position l' c
      | otherwise = Nothing
         where l' = l +/- dl
               dl = newL - oldL
               oldL = el-sl
               newL = T.count "\n" txt
    oldToNew = f (+)
    newToOld = f (-)

-- ---------------------------------------------------------------------

publishDiagnostics :: (MonadIO m, MonadReader REnv m)
  => Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> m ()
publishDiagnostics maxToSend uri' mv diags = do
  lf <- asks lspFuncs
  liftIO $ (Core.publishDiagnosticsFunc lf) maxToSend uri' mv diags

-- ---------------------------------------------------------------------

flushDiagnosticsBySource :: (MonadIO m, MonadReader REnv m)
  => Int -> Maybe J.DiagnosticSource -> m ()
flushDiagnosticsBySource maxToSend msource = do
  lf <- asks lspFuncs
  liftIO $ (Core.flushDiagnosticsBySourceFunc lf) maxToSend msource

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
reactor :: forall void. TChan ReactorInput -> R void
reactor inp = do
  -- forever $ do
  let
    loop :: TrackingNumber -> R void
    loop tn = do
      inval <- liftIO $ atomically $ readTChan inp
      liftIO $ U.logs $ "****** reactor: got message number:" ++ show tn

      case inval of
        RspFromClient resp@(J.ResponseMessage _ _ _ merr) -> do
          liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show resp
          case merr of
            Nothing -> return ()
            Just _ -> sendErrorLog $ "Got error response:" <> decodeUtf8 (BL.toStrict $ J.encode resp)

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

          -- TODO: Get all commands
          prefix <- asks commandPrefixer
          -- let pluginIds = map prefix (Map.keys (ipMap plugins))

          let
            options = J.object ["documentSelector" .= J.object [ "language" .= J.String "haskell"]]
            registrationsList =
              [ J.Registration (prefix "hare:demote") J.WorkspaceExecuteCommand (Just options)
              ]
          let registrations = J.RegistrationParams (J.List registrationsList)

          -- Do not actually register a command, but keep the code in
          -- place so we know how to do it when we actually need it.
          when False $ do
            rid <- nextLspReqId
            reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations

          reactorSend $ NotLogMessage $
                  fmServerLogMessageNotification J.MtLog $ "Using hie version: " <> T.pack version

          -- Check for mismatching GHC versions
          projGhcVersion <- liftIO getProjectGhcVersion
          when (projGhcVersion /= hieGhcVersion) $ do
            let msg = T.pack $ "Mismatching GHC versions: Project is " ++ projGhcVersion ++ ", HIE is " ++ hieGhcVersion
                      ++ "\nYou may want to use hie-wrapper. Check the README for more information"
            reactorSend $ NotShowMessage $ fmServerShowMessageNotification J.MtWarning msg
            reactorSend $ NotLogMessage $ fmServerLogMessageNotification J.MtWarning msg

          -- Check cabal is installed
          hasCabal <- liftIO checkCabalInstall
          unless hasCabal $ do
            let msg = T.pack "cabal-install is not installed. Check the README for more information"
            reactorSend $ NotShowMessage $ fmServerShowMessageNotification J.MtWarning msg
            reactorSend $ NotLogMessage $ fmServerLogMessageNotification J.MtWarning msg


          lf <- ask
          let hreq = GReq tn Nothing Nothing Nothing callback $ IdeResultOk <$> Hoogle.initializeHoogleDb
              callback Nothing = flip runReaderT lf $
                reactorSend $ NotShowMessage $
                  fmServerShowMessageNotification J.MtWarning "No hoogle db found. Check the README for instructions to generate one"
              callback (Just db) = flip runReaderT lf $ do
                reactorSend $ NotLogMessage $
                  fmServerLogMessageNotification J.MtLog $ "Using hoogle db at: " <> T.pack db
          makeRequest hreq

        -- -------------------------------

        NotDidOpenTextDocument notification -> do
          liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
          let
              td  = notification ^. J.params . J.textDocument
              uri = td ^. J.uri
              ver = Just $ td ^. J.version
          mapFileFromVfs tn $ J.VersionedTextDocumentIdentifier uri ver
          requestDiagnostics DiagnosticOnOpen tn uri ver

        -- -------------------------------

        NotDidChangeWatchedFiles _notification -> do
          liftIO $ U.logm "****** reactor: not processing NotDidChangeWatchedFiles"

        -- -------------------------------

        NotWillSaveTextDocument _notification -> do
          liftIO $ U.logm "****** reactor: not processing NotWillSaveTextDocument"

        NotDidSaveTextDocument notification -> do
          -- This notification is redundant, as we get the NotDidChangeTextDocument
          liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
          let
              td  = notification ^. J.params . J.textDocument
              uri = td ^. J.uri
              -- ver = Just $ td ^. J.version
              ver = Nothing
          mapFileFromVfs tn $ J.VersionedTextDocumentIdentifier uri ver
          requestDiagnostics DiagnosticOnSave tn uri ver

        NotDidChangeTextDocument notification -> do
          liftIO $ U.logm "****** reactor: processing NotDidChangeTextDocument"
          let
              params = notification ^. J.params
              vtdi = params ^. J.textDocument
              uri  = vtdi ^. J.uri
              ver  = vtdi ^. J.version
              J.List changes = params ^. J.contentChanges
          mapFileFromVfs tn vtdi
          makeRequest $ GReq tn (Just uri) Nothing Nothing (const $ return ()) $
            -- mark this module's cache as stale
            pluginGetFile "markCacheStale:" uri $ \fp -> do
              markCacheStale fp
              -- Important - Call this before requestDiagnostics
              updatePositionMap uri changes
          requestDiagnostics DiagnosticOnChange tn uri ver

        NotDidCloseTextDocument notification -> do
          liftIO $ U.logm "****** reactor: processing NotDidCloseTextDocument"
          let
              uri = notification ^. J.params . J.textDocument . J.uri
          -- unmapFileFromVfs versionTVar cin uri
          makeRequest $ GReq tn (Just uri) Nothing Nothing (const $ return ()) $ do
            forM_ (uriToFilePath uri)
              deleteCachedModule
            return $ IdeResultOk ()

        -- -------------------------------

        ReqRename req -> do
          liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
              pos = params ^. J.position
              newName  = params ^. J.newName
              callback = reactorSend . RspRename . Core.makeResponseMessage req
          let hreq = GReq tn (Just doc) Nothing (Just $ req ^. J.id) callback
                       $ HaRe.renameCmd' doc pos newName
          makeRequest hreq


        -- -------------------------------

        ReqHover req -> do
          liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
          let params = req ^. J.params
              pos = params ^. J.position
              doc = params ^. J.textDocument . J.uri
              callback (typ, docs, mrange) = do
                let
                  ht = case mrange of
                    Nothing    -> J.Hover (J.List []) Nothing
                    Just range -> J.Hover (J.List hovers)
                                          (Just range)
                      where
                        hovers = catMaybes [typ] ++ fmap J.PlainString docs
                  rspMsg = Core.makeResponseMessage req ht
                reactorSend $ RspHover rspMsg
          let
            getHoverInfo :: IdeM (IdeResponse (Maybe J.MarkedString, [T.Text], Maybe Range))
            getHoverInfo = runIdeResponseT $ do
                info' <- IdeResponseT $ IdeResponseResult <$> GhcMod.newTypeCmd pos doc
                names' <- IdeResponseT $ Hie.getSymbolsAtPoint doc pos
                let
                  f = (==) `on` (Hie.showName . snd)
                  f' = compare `on` (Hie.showName . snd)
                  names = mapMaybe pickName $ groupBy f $ sortBy f' names'
                  pickName [] = Nothing
                  pickName [x] = Just x
                  pickName xs@(x:_) = case find (isJust . nameModule_maybe . snd) xs of
                    Nothing -> Just x
                    Just a -> Just a
                  nnames = length names
                  (info,mrange) =
                    case map last $ groupBy ((==) `on` fst) info' of
                      ((r,typ):_) ->
                        case find ((r ==) . fst) names of
                          Nothing ->
                            (Just $ J.CodeString $ J.LanguageString "haskell" $ "_ :: " <> typ, Just r)
                          Just (_,name)
                            | nnames == 1 ->
                              (Just $ J.CodeString $ J.LanguageString "haskell" $ Hie.showName name <> " :: " <> typ, Just r)
                            | otherwise ->
                              (Just $ J.CodeString $ J.LanguageString "haskell" $ "_ :: " <> typ, Just r)
                      [] -> case names of
                        [] -> (Nothing, Nothing)
                        ((r,_):_) -> (Nothing, Just r)
                df <- IdeResponseT $ Hie.getDynFlags doc
                docs <- forM names $ \(_,name) -> do
                    let sname = Hie.showName name
                    case Hie.getModule df name of
                      Nothing -> return $ "`" <> sname <> "` *local*"
                      (Just (pkg,mdl)) -> do
                        let mname = "`"<> sname <> "`\n\n"
                        let minfo = maybe "" (<>" ") pkg <> mdl
                        mdocu' <- lift $ Haddock.getDocsWithType df name
                        mdocu <- case mdocu' of
                          Just _ -> return mdocu'
                          -- Hoogle as fallback
                          Nothing -> lift $ getDocsForName sname pkg mdl
                        case mdocu of
                          Nothing -> return $ mname <> minfo
                          Just docu -> return $ docu <> "\n\n" <> minfo
                return (info,docs,mrange)
          let hreq = IReq tn (req ^. J.id) callback $ do
                pluginGetFileResponse "ReqHover:" doc $ \fp -> do
                  cached <- isCached fp
                  -- Hover requests need to be instant so don't wait
                  -- for cached module to be loaded
                  if cached
                    then getHoverInfo
                    else return (IdeResponseOk (Nothing,[],Nothing))
          makeRequest hreq

          liftIO $ U.logs $ "reactor:HoverRequest done"

        -- -------------------------------

        ReqCodeAction req -> do
          liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
          handleCodeActionReq tn req
          -- TODO: make context specific commands for all sorts of things, such as refactorings          

        -- -------------------------------

        ReqExecuteCommand req -> do
          liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" ++ show req
          lf <- asks lspFuncs

          let params = req ^. J.params

              parseCmdId :: T.Text -> Maybe (T.Text, T.Text)
              parseCmdId x = case T.splitOn ":" x of
                [plugin, command] -> Just (plugin, command)
                [_, plugin, command] -> Just (plugin, command)
                _ -> Nothing

              callback obj = do
                liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show obj
                case fromDynJSON obj :: Maybe J.WorkspaceEdit of
                  Just v -> do
                    lid <- nextLspReqId
                    reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req (J.Object mempty)
                    let msg = fmServerApplyWorkspaceEditRequest lid $ J.ApplyWorkspaceEditParams v
                    liftIO $ U.logs $ "ExecuteCommand sending edit: " ++ show msg
                    reactorSend $ ReqApplyWorkspaceEdit msg
                  Nothing -> reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req $ dynToJSON obj

              execCmd cmdId args = do
                -- The parameters to the HIE command are always the first element
                let cmdParams = case args of
                     Just (J.List (x:_)) -> x
                     _ -> J.Null

                case parseCmdId cmdId of
                  -- Shortcut for immediately applying a applyWorkspaceEdit as a fallback for v3.8 code actions
                  Just ("hie", "fallbackCodeAction") -> do
                    case J.fromJSON cmdParams of
                      J.Success (FallbackCodeActionParams mEdit mCmd) -> do

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
                          Nothing -> reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req (J.Object mempty)

                      -- Couldn't parse the fallback command params
                      _ -> liftIO $
                        Core.sendErrorResponseS (Core.sendFunc lf)
                                                (J.responseId (req ^. J.id))
                                                J.InvalidParams
                                                "Invalid fallbackCodeAction params"
                  -- Just an ordinary HIE command
                  Just (plugin, cmd) ->
                    let preq = GReq tn Nothing Nothing (Just $ req ^. J.id) callback
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
          let params = req ^. J.params
              doc = params ^. (J.textDocument . J.uri)
              pos = params ^. J.position

          mprefix <- getPrefixAtPos doc pos

          let callback compls = do
                let rspMsg = Core.makeResponseMessage req
                              $ J.Completions $ J.List compls
                reactorSend $ RspCompletion rspMsg
          case mprefix of
            Nothing -> callback []
            Just prefix -> do
              let hreq = IReq tn (req ^. J.id) callback
                           $ Hie.getCompletions doc prefix
              makeRequest hreq

        ReqCompletionItemResolve req -> do
          liftIO $ U.logs $ "reactor:got CompletionItemResolveRequest:" ++ show req
          let origCompl = req ^. J.params
              mquery = case J.fromJSON <$> origCompl ^. J.xdata of
                         Just (J.Success q) -> Just q
                         _ -> Nothing
              callback docs = do
                let rspMsg = Core.makeResponseMessage req $
                              origCompl & J.documentation .~ docs
                reactorSend $ RspCompletionItemResolve rspMsg
              hreq = IReq tn (req ^. J.id) callback $ runIdeResponseT $ case mquery of
                        Nothing -> return Nothing
                        Just query -> do
                          result <- lift $ Hoogle.infoCmd' query
                          case result of
                            Right x -> return $ Just x
                            _ -> return Nothing
          makeRequest hreq

        -- -------------------------------

        ReqDocumentHighlights req -> do
          liftIO $ U.logs $ "reactor:got DocumentHighlightsRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. (J.textDocument . J.uri)
              pos = params ^. J.position
              callback = reactorSend . RspDocumentHighlights . Core.makeResponseMessage req . J.List
          let hreq = IReq tn (req ^. J.id) callback
                   $ Hie.getReferencesInDoc doc pos
          makeRequest hreq

        -- -------------------------------

        ReqDefinition req -> do
          liftIO $ U.logs $ "reactor:got DefinitionRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
              pos = params ^. J.position
              callback = reactorSend . RspDefinition . Core.makeResponseMessage req
          let hreq = IReq tn (req ^. J.id) callback
                       $ fmap J.MultiLoc <$> Hie.findDef doc pos
          makeRequest hreq

        ReqFindReferences req -> do
          liftIO $ U.logs $ "reactor:got FindReferences:" ++ show req
          -- TODO: implement project-wide references
          let params = req ^. J.params
              doc = params ^. (J.textDocument . J.uri)
              pos = params ^. J.position
              callback = reactorSend . RspFindReferences.  Core.makeResponseMessage req . J.List
          let hreq = IReq tn (req ^. J.id) callback
                   $ fmap (map (J.Location doc . (^. J.range)))
                   <$> Hie.getReferencesInDoc doc pos
          makeRequest hreq

        -- -------------------------------

        ReqDocumentFormatting req -> do
          liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
              tabSize = params ^. J.options . J.tabSize
              callback = reactorSend . RspDocumentFormatting . Core.makeResponseMessage req . J.List
          let hreq = GReq tn (Just doc) Nothing (Just $ req ^. J.id) callback
                       $ Brittany.brittanyCmd tabSize doc Nothing
          makeRequest hreq

        -- -------------------------------

        ReqDocumentRangeFormatting req -> do
          liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
          let params = req ^. J.params
              doc = params ^. J.textDocument . J.uri
              range = params ^. J.range
              tabSize = params ^. J.options . J.tabSize
              callback = reactorSend . RspDocumentRangeFormatting . Core.makeResponseMessage req . J.List
          let hreq = GReq tn (Just doc) Nothing (Just $ req ^. J.id) callback
                       $ Brittany.brittanyCmd tabSize doc (Just range)
          makeRequest hreq

        -- -------------------------------

        ReqDocumentSymbols req -> do
          liftIO $ U.logs $ "reactor:got Document symbol request:" ++ show req
          let uri = req ^. J.params . J.textDocument . J.uri
              callback = reactorSend . RspDocumentSymbols . Core.makeResponseMessage req . J.List
          let hreq = IReq tn (req ^. J.id) callback
                   $ Hie.getSymbols uri
          makeRequest hreq

        -- -------------------------------

        NotCancelRequestFromClient notif -> do
          liftIO $ U.logs $ "reactor:got CancelRequest:" ++ show notif
          let lid = notif ^. J.params . J.id
          DispatcherEnv cancelReqTVar wipTVar _ <- asks dispatcherEnv
          liftIO $ atomically $ do
            wip <- readTVar wipTVar
            when (S.member lid wip) $ do
              modifyTVar' cancelReqTVar (S.insert lid)

        -- -------------------------------

        NotDidChangeConfiguration notif -> do
          liftIO $ U.logs $ "reactor:didChangeConfiguration notification:" ++ show notif
          -- if hlint has been turned off, flush the disgnostics
          diagsOn              <- configVal True hlintOn
          maxDiagnosticsToSend <- configVal 50 maxNumberOfProblems
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

docRules :: Maybe T.Text -> T.Text -> T.Text
docRules (Just "base") "GHC.Base"    = "Prelude"
docRules (Just "base") "GHC.Enum"    = "Prelude"
docRules (Just "base") "GHC.Num"     = "Prelude"
docRules (Just "base") "GHC.Real"    = "Prelude"
docRules (Just "base") "GHC.Float"   = "Prelude"
docRules (Just "base") "GHC.Show"    = "Prelude"
docRules (Just "containers") modName =
  fromMaybe modName $ T.stripSuffix ".Base" modName
docRules _ modName = modName

getDocsForName :: T.Text -> Maybe T.Text -> T.Text -> IdeM (Maybe T.Text)
getDocsForName name pkg modName' = do
  let modName = docRules pkg modName'
      query = name
           <> maybe "" (T.append " package:") pkg
           <> " module:" <> modName
           <> " is:exact"
  debugm $ "hoogle query: " ++ T.unpack query
  res <- Hoogle.infoCmdFancyRender query
  case res of
    Right x -> return $ Just x
    Left _ -> return Nothing

-- ---------------------------------------------------------------------

requestDiagnostics :: DiagnosticTrigger -> TrackingNumber -> J.Uri -> J.TextDocumentVersion -> R ()
requestDiagnostics trigger tn file mVer = do
  when (S.member trigger (S.fromList [DiagnosticOnChange,DiagnosticOnOpen])) $
    requestDiagnosticsNormal tn file mVer

  diagFuncs <- asks diagnosticSources
  lf <- asks lspFuncs
  cin <- asks reqChanIn
  mc <- liftIO $ Core.config lf
  case Map.lookup trigger diagFuncs of
    Nothing -> return ()
    Just dss -> do
      forM_ dss $ \(pid,ds) -> do
        let
          maxToSend = maybe 50 maxNumberOfProblems mc
          sendOne (fileUri,ds') = do
            publishDiagnostics maxToSend fileUri Nothing (Map.fromList [(Just pid,SL.toSortedList ds')])

          sendEmpty = publishDiagnostics maxToSend file Nothing (Map.fromList [(Just pid,SL.toSortedList [])])
          fv = case mVer of
            Nothing -> Nothing
            Just v -> Just (file,v)
        let reql = GReq tn (Just file) fv Nothing callbackl
                     $ ds file
            callbackl pd = do
              let diags = Map.toList $ S.toList <$> pd
              case diags of
                [] -> sendEmpty
                _ -> mapM_ sendOne diags
        liftIO $ atomically $ writeTChan cin reql

-- | get hlint and GHC diagnostics and loads the typechecked module into the cache
requestDiagnosticsNormal :: TrackingNumber -> J.Uri -> J.TextDocumentVersion -> R ()
requestDiagnosticsNormal tn file mVer = do
  lf <- asks lspFuncs
  cin <- asks reqChanIn
  mc <- liftIO $ Core.config lf
  let
    ver = fromMaybe 0 mVer

    -- | If there is a GHC error, flush the hlint diagnostics
    -- TODO: Just flush the parse error diagnostics
    sendOneGhc :: J.DiagnosticSource -> (Uri, [Diagnostic]) -> R ()
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
    sendEmpty = publishDiagnostics maxToSend file Nothing (Map.fromList [(Just "ghcmod",SL.toSortedList [])])
    maxToSend = maybe 50 maxNumberOfProblems mc

  let sendHlint = maybe True hlintOn mc
  when sendHlint $ do
    -- get hlint diagnostics
    let reql = GReq tn (Just file) (Just (file,ver)) Nothing callbackl
                 $ ApplyRefact.lintCmd' file
        callbackl (PublishDiagnosticsParams fp (List ds))
             = sendOne "hlint" (fp, ds)
    liftIO $ atomically $ writeTChan cin reql

  -- get GHC diagnostics and loads the typechecked module into the cache
  let reqg = GReq tn (Just file) (Just (file,ver)) Nothing callbackg
               $ GhcMod.setTypecheckedModule file
      callbackg (pd, errs) = do
        forM_ errs $ \e -> do
          reactorSend $ NotShowMessage $
            fmServerShowMessageNotification J.MtError
              $ "Got error while processing diagnostics: " <> e
        let ds = Map.toList $ S.toList <$> pd
        case ds of
          [] -> sendEmpty
          _ -> mapM_ (sendOneGhc "ghcmod") ds

  liftIO $ atomically $ writeTChan cin reqg

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

hieOptions :: [T.Text] -> Core.Options
hieOptions commandIds =
  def { Core.textDocumentSync       = Just syncOptions
      , Core.completionProvider     = Just (J.CompletionOptions (Just True) (Just ["."]))
      -- As of 2018-05-24, vscode needs the commands to be registered
      -- otherwise they will not be available as codeActions (will be
      -- silently ignored, despite UI showing to the contrary).
      --
      -- Hopefully the end May 2018 vscode release will stabilise
      -- this, it is a major rework of the machinery anyway.
      , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List commandIds))
      }


hieHandlers :: TChan ReactorInput -> Core.Handlers
hieHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin ReqRename
        , Core.definitionHandler                        = Just $ passHandler rin ReqDefinition
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
