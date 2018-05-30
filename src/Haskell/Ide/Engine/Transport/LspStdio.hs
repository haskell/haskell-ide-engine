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
import           Data.Aeson ( (.=), (.:), (.:?), (.!=) )
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isUpper, isAlphaNum)
import           Data.Default
import           Data.Maybe
import           Data.Monoid ( (<>) )
import           Data.Function
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.SortedList as SL
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Vector as V
import qualified GhcModCore               as GM
import qualified GhcMod.Monad.Types       as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import qualified Haskell.Ide.Engine.Plugin.HaRe        as HaRe
import qualified Haskell.Ide.Engine.Plugin.GhcMod      as GhcMod
import qualified Haskell.Ide.Engine.Plugin.ApplyRefact as ApplyRefact
import qualified Haskell.Ide.Engine.Plugin.Brittany    as Brittany
import qualified Haskell.Ide.Engine.Plugin.Hoogle      as Hoogle
import qualified Haskell.Ide.Engine.Plugin.Haddock     as Haddock
import qualified Haskell.Ide.Engine.Plugin.HieExtras   as Hie
import           Haskell.Ide.Engine.Plugin.Base
import qualified Language.Haskell.LSP.Control          as CTRL
import qualified Language.Haskell.LSP.Core             as Core
import qualified Language.Haskell.LSP.VFS              as VFS
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Utility          as U
import           System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi

import Name

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport :: (DispatcherEnv -> ErrorHandler -> CallbackHandler R -> IO ()) -> TChan (PluginRequest R) -> FilePath -> IO ()
lspStdioTransport hieDispatcherProc cin origDir = do
  run hieDispatcherProc cin origDir >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

run :: (DispatcherEnv -> ErrorHandler -> CallbackHandler R -> IO ()) -> TChan (PluginRequest R) -> FilePath -> IO Int
run dispatcherProc cin _origDir = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  let
    dp lf = do
      cancelTVar      <- atomically $ newTVar S.empty
      wipTVar         <- atomically $ newTVar S.empty
      versionTVar     <- atomically $ newTVar Map.empty
      let dispatcherEnv = DispatcherEnv
            { cancelReqsTVar     = cancelTVar
            , wipReqsTVar        = wipTVar
            , docVersionTVar     = versionTVar
            }
      let reactorFunc =  flip runReaderT lf $ reactor dispatcherEnv cin rin

      let errorHandler :: ErrorHandler
          errorHandler lid code e =
            Core.sendErrorResponseS (Core.sendFunc lf) (J.responseId lid) code (T.pack e)
          callbackHandler :: CallbackHandler R
          callbackHandler f x = flip runReaderT lf $ f x

      -- haskell lsp sets the current directory to the project root in the InitializeRequest
      -- We launch the dispatcher after that so that the defualt cradle is
      -- recognized properly by ghc-mod
      _ <- forkIO $ race_ (dispatcherProc dispatcherEnv errorHandler callbackHandler) reactorFunc
      return Nothing

  flip E.finally finalProc $ do
    -- TODO: Merge this with branch for recording client output
    CTRL.run (getConfig,dp) (hieHandlers rin) hieOptions Nothing

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

type ReactorInput
  = FromClientMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------

-- | Callback from haskell-lsp core to convert the generic message to the
-- specific one for hie
getConfig :: J.DidChangeConfigurationNotification -> Either T.Text Config
getConfig (J.NotificationMessage _ _ (J.DidChangeConfigurationParams p)) =
  case J.fromJSON p of
    J.Success c -> Right c
    J.Error err -> Left $ T.pack err

data Config =
  Config
    { hlintOn             :: Bool
    , maxNumberOfProblems :: Int
    } deriving (Show)

instance J.FromJSON Config where
  parseJSON = J.withObject "Config" $ \v -> do
    s <- v .: "languageServerHaskell"
    flip (J.withObject "Config.settings") s $ \o -> Config
      <$> o .:? "hlintOn" .!= True
      <*> o .: "maxNumberOfProblems"

-- 2017-10-09 23:22:00.710515298 [ThreadId 11] - ---> {"jsonrpc":"2.0","method":"workspace/didChangeConfiguration","params":{"settings":{"languageServerHaskell":{"maxNumberOfProblems":100,"hlintOn":true}}}}
-- 2017-10-09 23:22:00.710667381 [ThreadId 15] - reactor:got didChangeConfiguration notification:
-- NotificationMessage
--   {_jsonrpc = "2.0"
--   , _method = WorkspaceDidChangeConfiguration
--   , _params = DidChangeConfigurationParams
--                 {_settings = Object (fromList [("languageServerHaskell",Object (fromList [("hlintOn",Bool True)
--                                                                                          ,("maxNumberOfProblems",Number 100.0)]))])}}

configVal :: c -> (Config -> c) -> R c
configVal defVal field = do
  gmc <- asks Core.config
  mc <- liftIO gmc
  return $ maybe defVal field mc

-- ---------------------------------------------------------------------

-- | The monad used in the reactor

type R = ReaderT (Core.LspFuncs Config) IO
-- type R a = ReaderT (Core.LspFuncs Config) IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

reactorSend :: (MonadIO m, MonadReader (Core.LspFuncs Config) m) => FromServerMessage -> m ()
reactorSend msg = do
  sf <- asks Core.sendFunc
  liftIO $ sf msg

-- ---------------------------------------------------------------------

reactorSend' :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => (Core.SendFunc -> IO ()) -> m ()
reactorSend' f = do
  lf <- ask
  liftIO $ f (Core.sendFunc lf)

-- ---------------------------------------------------------------------

getPrefixAtPos :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => Uri -> Position -> m (Maybe (T.Text,T.Text))
getPrefixAtPos uri (Position l c) = do
  mvf <- liftIO =<< asks Core.getVirtualFileFunc <*> pure uri
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

mapFileFromVfs :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => TVar (Map.Map Uri Int) -> TChan (PluginRequest R) -> J.VersionedTextDocumentIdentifier -> m ()
mapFileFromVfs verTVar cin vtdi = do
  let uri = vtdi ^. J.uri
      ver = vtdi ^. J.version
  vfsFunc <- asks Core.getVirtualFileFunc
  mvf <- liftIO $ vfsFunc uri
  case (mvf, uriToFilePath uri) of
    (Just (VFS.VirtualFile _ yitext), Just fp) -> do
      let text' = Yi.toString yitext
          -- text = "{-# LINE 1 \"" ++ fp ++ "\"#-}\n" <> text'
      let req = GReq (Just uri) Nothing Nothing (const $ return ())
                  $ IdeResultOk <$> do
                      GM.loadMappedFileSource fp text'
                      fileMap <- GM.getMMappedFiles
                      debugm $ "file mapping state is: " ++ show fileMap
      liftIO $ atomically $ do
        modifyTVar' verTVar (Map.insert uri ver)
        writeTChan cin req
      return ()
    (_, _) -> return ()

_unmapFileFromVfs :: (MonadIO m)
  => TVar (Map.Map Uri Int) -> TChan (PluginRequest R) -> Uri -> m ()
_unmapFileFromVfs verTVar cin uri = do
  case uriToFilePath uri of
    Just fp -> do
      let req = GReq (Just uri) Nothing Nothing (const $ return ())
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
          (n2o,o2n) = foldr go (n2oOld, o2nOld) changes
          go (J.TextDocumentContentChangeEvent (Just r) _ txt) (n2o', o2n') =
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

publishDiagnostics :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => Int -> J.Uri -> Maybe J.TextDocumentVersion -> DiagnosticsBySource -> m ()
publishDiagnostics maxToSend uri' mv diags = do
  lf <- ask
  liftIO $ (Core.publishDiagnosticsFunc lf) maxToSend uri' mv diags

-- ---------------------------------------------------------------------

flushDiagnosticsBySource :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => Int -> Maybe J.DiagnosticSource -> m ()
flushDiagnosticsBySource maxToSend msource = do
  lf <- ask
  liftIO $ (Core.flushDiagnosticsBySourceFunc lf) maxToSend msource

-- ---------------------------------------------------------------------

nextLspReqId :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => m J.LspId
nextLspReqId = do
  f <- asks Core.getNextReqId
  liftIO f

-- ---------------------------------------------------------------------

sendErrorLog :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
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
reactor :: forall void. DispatcherEnv -> TChan (PluginRequest R) -> TChan ReactorInput -> R void
reactor (DispatcherEnv cancelReqTVar wipTVar versionTVar) cin inp = do
  let
    makeRequest req@(GReq _ Nothing (Just lid) _ _) = liftIO $ atomically $ do
      modifyTVar wipTVar (S.insert lid)
      writeTChan cin req
    makeRequest req@(IReq lid _ _) = liftIO $ atomically $ do
      modifyTVar wipTVar (S.insert lid)
      writeTChan cin req
    makeRequest req =
      liftIO $ atomically $ writeTChan cin req

  forever $ do
    inval <- liftIO $ atomically $ readTChan inp
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
        let
          options = J.object ["documentSelector" .= J.object [ "language" .= J.String "haskell"]]
          registrationsList =
            [ J.Registration "hare:demote" J.WorkspaceExecuteCommand (Just options)
            ]
        let registrations = J.RegistrationParams (J.List registrationsList)

        -- Do not actually register a command, but keep the code in
        -- place so we know how to do it when we actually need it.
        when False $ do
          rid <- nextLspReqId
          reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations

        reactorSend $ NotLogMessage $
                fmServerLogMessageNotification J.MtLog $ "Using hie version: " <> T.pack version

        lf <- ask
        let hreq = GReq Nothing Nothing Nothing callback $ IdeResultOk <$> Hoogle.initializeHoogleDb
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
            ver = td ^. J.version
        mapFileFromVfs versionTVar cin $ J.VersionedTextDocumentIdentifier uri ver
        requestDiagnostics cin uri ver

      -- -------------------------------

      NotWillSaveTextDocument _notification -> do
        liftIO $ U.logm "****** reactor: not processing NotWillSaveTextDocument"

      NotDidChangeWatchedFiles _notification -> do
        liftIO $ U.logm "****** reactor: not processing NotDidChangeWatchedFiles"
      NotDidSaveTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            uri = notification ^. J.params . J.textDocument . J.uri
        mver <- liftIO $ atomically $ Map.lookup uri <$> readTVar versionTVar
        case mver of
          Just ver -> requestDiagnostics cin uri ver
          Nothing -> do
            let ver = -1
            liftIO $ atomically $ modifyTVar' versionTVar (Map.insert uri ver)
            requestDiagnostics cin uri ver

      NotDidChangeTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidChangeTextDocument"
        let
            params = notification ^. J.params
            vtdi = params ^. J.textDocument
            uri  = vtdi ^. J.uri
            ver  = vtdi ^. J.version
            J.List changes = params ^. J.contentChanges
        mapFileFromVfs versionTVar cin vtdi
        makeRequest $ GReq (Just uri) Nothing Nothing (const $ return ()) $
          -- mark this module's cache as stale
          pluginGetFile "markCacheStale:" uri $ \fp -> do
            markCacheStale fp
            -- Important - Call this before requestDiagnostics
            updatePositionMap uri changes
        requestDiagnostics cin uri ver

      NotDidCloseTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidCloseTextDocument"
        let
            uri = notification ^. J.params . J.textDocument . J.uri
        -- unmapFileFromVfs versionTVar cin uri
        makeRequest $ GReq (Just uri) Nothing Nothing (const $ return ()) $ do
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
        let hreq = GReq (Just doc) Nothing (Just $ req ^. J.id) callback
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
        let hreq = IReq (req ^. J.id) callback $ do
              pluginGetFileResponse "ReqHover:" doc $ \fp -> do
                cached <- isCached fp
                -- Hover requests need to be instant so don't wait
                -- for cached module to be loaded
                if cached
                  then getHoverInfo
                  else return (IdeResponseOk (Nothing,[],Nothing))
        makeRequest hreq

        liftIO $ U.logs "reactor:HoverRequest done"

      -- -------------------------------

      ReqCodeAction req -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req

        let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            (J.List diags) = params ^. J.context . J.diagnostics

        let
           -- |Some hints do not have an associated refactoring
           validCommand (J.Diagnostic _ _ (Just code) (Just "hlint") _ _) =
             case code of
               "Eta reduce" -> False
               _            -> True
           validCommand _ = False

           makeCommand (J.Diagnostic (J.Range start _) _s (Just code) (Just "hlint") m _) = [J.Command title cmd cmdparams]
             where
               title :: T.Text
               title = "Apply hint:" <> head (T.lines m)
               -- NOTE: the cmd needs to be registered via the InitializeResponse message. See hieOptions above
               cmd = "applyrefact:applyOne"
               -- need 'file', 'start_pos' and hint title (to distinguish between alternative suggestions at the same location)
               args = J.Array $ V.singleton $ J.toJSON $ ApplyRefact.AOP doc start code
               cmdparams = Just args
           makeCommand (J.Diagnostic _r _s _c _source _m _) = []
           -- TODO: make context specific commands for all sorts of things, such as refactorings
        let body = J.List $ concatMap makeCommand $ filter validCommand diags
        let rspMsg = Core.makeResponseMessage req body
        reactorSend $ RspCodeAction rspMsg


      -- -------------------------------

      ReqExecuteCommand req -> do
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" ++ show req
        let params = req ^. J.params
            command = params ^. J.command
            margs = params ^. J.arguments


        --liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
        let cmdparams = case margs of
              Just (J.List (x:_)) -> x
              _ -> J.Null
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
        let (plugin,cmd) = T.break (==':') command
        let preq = GReq Nothing Nothing (Just $ req ^. J.id) callback
                     $ runPluginCommand plugin (T.drop 1 cmd) cmdparams
        makeRequest preq

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
            let hreq = IReq (req ^. J.id) callback
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
            hreq = IReq (req ^. J.id) callback $ runIdeResponseT $ case mquery of
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
        let hreq = IReq (req ^. J.id) callback
                 $ Hie.getReferencesInDoc doc pos
        makeRequest hreq

      -- -------------------------------
      ReqDefinition req -> do
        liftIO $ U.logs $ "reactor:got DefinitionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            pos = params ^. J.position
            callback = reactorSend . RspDefinition . Core.makeResponseMessage req
        let hreq = IReq (req ^. J.id) callback
                     $ fmap J.MultiLoc <$> Hie.findDef doc pos
        makeRequest hreq

      ReqFindReferences req -> do
        liftIO $ U.logs $ "reactor:got FindReferences:" ++ show req
        -- TODO: implement project-wide references
        let params = req ^. J.params
            doc = params ^. (J.textDocument . J.uri)
            pos = params ^. J.position
            callback = reactorSend . RspFindReferences . Core.makeResponseMessage req . J.List
        let hreq = IReq (req ^. J.id) callback
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
        let hreq = GReq (Just doc) Nothing (Just $ req ^. J.id) callback
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
        let hreq = GReq (Just doc) Nothing (Just $ req ^. J.id) callback
                     $ Brittany.brittanyCmd tabSize doc (Just range)
        makeRequest hreq

      -- -------------------------------

      ReqDocumentSymbols req -> do
        liftIO $ U.logs $ "reactor:got Document symbol request:" ++ show req
        let uri = req ^. J.params . J.textDocument . J.uri
            callback = reactorSend . RspDocumentSymbols . Core.makeResponseMessage req . J.List
        let hreq = IReq (req ^. J.id) callback
                 $ Hie.getSymbols uri
        makeRequest hreq

      -- -------------------------------

      NotCancelRequestFromClient notif -> do
        liftIO $ U.logs $ "reactor:got CancelRequest:" ++ show notif
        let lid = notif ^. J.params . J.id
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

-- | get hlint and GHC diagnostics and loads the typechecked module into the cache
requestDiagnostics :: TChan (PluginRequest R) -> J.Uri -> Int -> R ()
requestDiagnostics cin file ver = do
  lf <- ask
  mc <- liftIO $ Core.config lf
  let
    -- | If there is a GHC error, flush the hlint diagnostics
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

  -- mc <- asks Core.config
  let sendHlint = maybe True hlintOn mc
  when sendHlint $ do
    -- get hlint diagnostics
    let reql = GReq (Just file) (Just (file,ver)) Nothing callbackl
                 $ ApplyRefact.lintCmd' file
        callbackl (PublishDiagnosticsParams fp (List ds))
             = sendOne "hlint" (fp, ds)
    liftIO $ atomically $ writeTChan cin reql

  -- get GHC diagnostics and loads the typechecked module into the cache
  let reqg = GReq (Just file) (Just (file,ver)) Nothing callbackg
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

hieOptions :: Core.Options
hieOptions =
  def { Core.textDocumentSync       = Just syncOptions
      , Core.completionProvider     = Just (J.CompletionOptions (Just True) (Just ["."]))
      -- As of 2018-05-24, vscode needs the commands to be registered
      -- otherwise they will not be available as codeActions (will be
      -- silently ignored, despite UI showing to the contrary).
      --
      -- Hopefully the end May 2018 vscode release will stabilise
      -- this, it is a major rework of the machinery anyway.
      , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["applyrefact:applyOne","hare:demote"]))
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
