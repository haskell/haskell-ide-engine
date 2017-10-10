{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TupleSections         #-}
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
import           Control.Monad.Trans.Either
import           Control.Monad.STM
import           Control.Monad.Reader
import qualified Data.Aeson as J
import           Data.Aeson ( (.=), (.:) )
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
import qualified GhcMod.ModuleLoader      as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import qualified Haskell.Ide.HaRePlugin as HaRe
#if __GLASGOW_HASKELL__ >= 802
#else
import qualified Haskell.Ide.HaddockPlugin as Haddock
#endif
import qualified Haskell.Ide.GhcModPlugin as GhcMod
import qualified Haskell.Ide.ApplyRefactPlugin as ApplyRefact
import qualified Haskell.Ide.BrittanyPlugin as Brittany
import qualified Haskell.Ide.HooglePlugin as Hoogle
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as Core
import qualified Language.Haskell.LSP.VFS     as VFS
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import           System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi

import Name

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport :: (DispatcherEnv -> IO ()) -> TChan PluginRequest -> FilePath -> IO ()
lspStdioTransport hieDispatcherProc cin origDir = do
  run hieDispatcherProc cin origDir >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

run :: (DispatcherEnv -> IO ()) -> TChan PluginRequest -> FilePath -> IO Int
run dispatcherProc cin _origDir = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  let
    dp lf = do
      cancelTVar  <- atomically $ newTVar S.empty
      wipTVar     <- atomically $ newTVar S.empty
      versionTVar <- atomically $ newTVar Map.empty
      let dispatcherEnv = DispatcherEnv
            { cancelReqsTVar = cancelTVar
            , wipReqsTVar    = wipTVar
            , docVersionTVar = versionTVar
            }
      let reactorFunc =  flip runReaderT lf $ reactor dispatcherEnv cin rin
      -- haskell lsp sets the current directory to the project root in the InitializeRequest
      -- We launch the dispatcher after that so that the defualt cradle is
      -- recognized properly by ghc-mod
      _ <- forkIO $ race_ (dispatcherProc dispatcherEnv) reactorFunc
      return Nothing

  flip E.finally finalProc $ do
    -- tmpDir <- getTemporaryDirectory
    -- let logDir = tmpDir </> "hie-logs"
    -- createDirectoryIfMissing True logDir
    -- let dirStr = map (\c -> if c == pathSeparator then '-' else c) origDir
    -- let logFileName = logDir </> (dirStr ++ "-hie.log")
    -- Core.setupLogger logFileName ["HaRe"] L.DEBUG
    CTRL.run (getConfig,dp) (hieHandlers rin) hieOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

type ReactorInput
  = Core.OutMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

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
      <$> (o .: "hlintOn")
      <*> (o .: "maxNumberOfProblems")

-- 2017-10-09 23:22:00.710515298 [ThreadId 11] - ---> {"jsonrpc":"2.0","method":"workspace/didChangeConfiguration","params":{"settings":{"languageServerHaskell":{"maxNumberOfProblems":100,"hlintOn":true}}}}
-- 2017-10-09 23:22:00.710667381 [ThreadId 15] - reactor:got didChangeConfiguration notification:
-- NotificationMessage
--   {_jsonrpc = "2.0"
--   , _method = WorkspaceDidChangeConfiguration
--   , _params = DidChangeConfigurationParams
--                 {_settings = Object (fromList [("languageServerHaskell",Object (fromList [("hlintOn",Bool True)
--                                                                                          ,("maxNumberOfProblems",Number 100.0)]))])}}

configVal :: c -> (Config -> c) -> R c
configVal def field = do
  gmc <- asks Core.config
  mc <- liftIO gmc
  return $ maybe def field mc

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R a = ReaderT (Core.LspFuncs Config) IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

reactorSend :: (J.ToJSON a, MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => a -> m ()
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
          let moduleParts = dropWhile (not . isUpper . T.head)
                              $ reverse $ filter (not .T.null) xs
              moduleName = T.intercalate "." moduleParts
          return (moduleName,x)
    Nothing -> return Nothing
-- ---------------------------------------------------------------------

mapFileFromVfs :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => TVar (Map.Map Uri Int) -> TChan PluginRequest -> J.VersionedTextDocumentIdentifier -> m ()
mapFileFromVfs verTVar cin vtdi = do
  let uri = vtdi ^. J.uri
      ver = vtdi ^. J.version
  vfsFunc <- asks Core.getVirtualFileFunc
  mvf <- liftIO $ vfsFunc uri
  case (mvf, uriToFilePath uri) of
    (Just (VFS.VirtualFile _ yitext), Just fp) -> do
      let text' = Yi.toString yitext
          -- text = "{-# LINE 1 \"" ++ fp ++ "\"#-}\n" <> text'
      let req = PReq (Just uri) Nothing Nothing (const $ return ())
                  $ IdeResponseOk <$> GM.loadMappedFileSource fp text'
      liftIO $ atomically $ do
        modifyTVar' verTVar (Map.insert uri ver)
        writeTChan cin req
      return ()
    (_, _) -> return ()

unmapFileFromVfs :: (MonadIO m)
  => TVar (Map.Map Uri Int) -> TChan PluginRequest -> Uri -> m ()
unmapFileFromVfs verTVar cin uri = do
  case uriToFilePath uri of
    Just fp -> do
      let req = PReq (Just uri) Nothing Nothing (const $ return ())
                 $ IdeResponseOk <$> GM.unloadMappedFile fp
      liftIO $ atomically $ do
        modifyTVar' verTVar (Map.delete uri)
        writeTChan cin req
      return ()
    _ -> return ()

-- TODO: generalise this and move it to GhcMod.ModuleLoader
updatePositionMap :: Uri -> [J.TextDocumentContentChangeEvent] -> IdeM (IdeResponse ())
updatePositionMap uri changes = pluginGetFile "updatePositionMap: " uri $ \file -> do
  mcm <- GM.getCachedModule (GM.filePathToUri file)
  case mcm of
    Just cm -> do
      let n2oOld = GM.newPosToOldPos cm
          o2nOld = GM.oldPosToNewPos cm
          (n2o,o2n) = foldr go (n2oOld, o2nOld) changes
          go (J.TextDocumentContentChangeEvent (Just r) _ txt) (n2o', o2n') =
            (n2o' <=< newToOld r txt, oldToNew r txt <=< o2n')
          go _ _ = (const Nothing, const Nothing)
      let cm' = cm {GM.newPosToOldPos = n2o, GM.oldPosToNewPos = o2n}
      GM.cacheModule (GM.filePathToUri file) cm'
      return $ IdeResponseOk ()
    Nothing ->
      return $ IdeResponseOk ()
  where
    oldToNew (J.Range (Position sl _) (Position el _)) txt p@(GM.Pos l c)
      | l < sl = Just p
      | l > el = Just $ GM.Pos l' c
      | otherwise = Nothing
         where l' = l + dl
               dl = newL - oldL
               oldL = el-sl
               newL = T.count "\n" txt
    newToOld (J.Range (Position sl _) (Position el _)) txt p@(GM.Pos l c)
      | l < sl = Just p
      | l > el = Just $ GM.Pos l' c
      | otherwise = Nothing
         where l' = l - dl
               dl = newL - oldL
               oldL = el-sl
               newL = T.count "\n" txt

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

sendErrorResponse
  :: (MonadIO m, MonadReader (Core.LspFuncs Config) m)
  => J.LspId
  -> J.ErrorCode
  -> T.Text
  -> m ()
sendErrorResponse origId err msg =
  reactorSend' (\sf -> Core.sendErrorResponseS sf (J.responseId origId) err msg)

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
reactor :: forall void. DispatcherEnv -> TChan PluginRequest -> TChan ReactorInput -> R void
reactor (DispatcherEnv cancelReqTVar wipTVar versionTVar) cin inp = do
  let makeRequest req@(PReq _ Nothing (Just lid) _ _) = liftIO $ atomically $ do
        modifyTVar wipTVar (S.insert lid)
        writeTChan cin req
      makeRequest req =
        liftIO $ atomically $ writeTChan cin req
  forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of
      Core.RspFromClient resp@(J.ResponseMessage _ _ _ merr) -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show resp
        case merr of
          Nothing -> return ()
          Just _ -> sendErrorLog $ "Got error response:" <> decodeUtf8 (BL.toStrict $ J.encode resp)

      -- -------------------------------

      Core.NotInitialized _notification -> do
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
          registrationsList =
            [ J.Registration "hare:demote" J.WorkspaceExecuteCommand (Just options)
            ]
        let registrations = J.RegistrationParams (J.List registrationsList)

        rid <- nextLspReqId
        reactorSend $ fmServerRegisterCapabilityRequest rid registrations

        lf <- ask
        let hreq = PReq Nothing Nothing Nothing callback
                     Hoogle.initializeHoogleDb
            callback Nothing = flip runReaderT lf $
              reactorSend $
                fmServerShowMessageNotification J.MtWarning "No hoogle db found. Check the README for instructions to generate one"
            callback (Just db) = flip runReaderT lf $ do
              reactorSend $
                fmServerLogMessageNotification J.MtLog $ "Using hoogle db at: " <> T.pack db
        makeRequest hreq

      -- -------------------------------

      Core.NotDidOpenTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
        let
            td  = notification ^. J.params . J.textDocument
            uri = td ^. J.uri
            ver = td ^. J.version
        mapFileFromVfs versionTVar cin $ J.VersionedTextDocumentIdentifier uri ver
        requestDiagnostics cin uri ver

      -- -------------------------------

      Core.NotDidSaveTextDocument notification -> do
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

      Core.NotDidChangeTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidChangeTextDocument"
        let
            params = notification ^. J.params
            vtdi = params ^. J.textDocument
            uri  = vtdi ^. J.uri
            ver  = vtdi ^. J.version
            J.List changes = params ^. J.contentChanges
        mapFileFromVfs versionTVar cin vtdi
        -- Important - Call this before requestDiagnostics
        makeRequest $ PReq (Just uri) Nothing Nothing (const $ return ())
                        $ updatePositionMap uri changes
        requestDiagnostics cin uri ver

      Core.NotDidCloseTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidCloseTextDocument"
        let
            uri = notification ^. J.params . J.textDocument . J.uri
        -- unmapFileFromVfs versionTVar cin uri
        makeRequest $ PReq (Just uri) Nothing Nothing (const $ return ()) $ do
          case uriToFilePath uri of
            Just fp -> GM.deleteCachedModule (GM.filePathToUri fp)
            Nothing -> return ()
          return $ IdeResponseOk ()

      -- -------------------------------

      Core.ReqRename req -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            pos = params ^. J.position
            newName  = params ^. J.newName
        callback <- hieResponseHelper (req ^. J.id) $ \we -> do
            let rspMsg = Core.makeResponseMessage req we
            reactorSend rspMsg
        let hreq = PReq (Just $ doc ^. J.uri) Nothing (Just $ req ^. J.id) callback
                     $ HaRe.renameCmd' (TextDocumentPositionParams doc pos) newName
        makeRequest hreq


      -- -------------------------------

      Core.ReqHover req -> do
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        let params = req ^. J.params
            pos = params ^. J.position
            doc = params ^. J.textDocument . J.uri
        callback <- hieResponseHelper (req ^. J.id) $ \(typ,docs,mrange) -> do
            let
              ht = case mrange of
                Nothing    -> J.Hover (J.List []) Nothing
                Just range -> J.Hover (J.List hovers)
                              (Just range)
                  where
                    hovers = catMaybes [typ] ++ fmap J.PlainString docs
              rspMsg = Core.makeResponseMessage req ht
            reactorSend rspMsg
        let hreq = PReq (Just doc) Nothing (Just $ req ^. J.id) callback $ runEitherT $ do
              info' <- EitherT $ GhcMod.newTypeCmd True doc pos
              names' <- EitherT $ HaRe.getSymbolsAtPoint doc pos
              let
                f = (==) `on` (HaRe.showName . snd)
                f' = compare `on` (HaRe.showName . snd)
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
                            (Just $ J.CodeString $ J.LanguageString "haskell" $ HaRe.showName name <> " :: " <> typ, Just r)
                          | otherwise ->
                            (Just $ J.CodeString $ J.LanguageString "haskell" $ "_ :: " <> typ, Just r)
                    [] -> case names of
                      [] -> (Nothing, Nothing)
                      ((r,_):_) -> (Nothing, Just r)
              docs <- forM names $ \(_,name) -> do
                  let sname = HaRe.showName name
                  df <- lift GhcMod.getDynFlags
                  case HaRe.getModule df name of
                    Nothing -> return $ "`" <> sname <> "` *local*"
                    (Just (pkg,mdl)) -> do
#if __GLASGOW_HASKELL__ >= 802
                      mdocu <- lift $ getDocsForName sname pkg mdl
#else
                      mdocu <- lift $ Haddock.getDocsWithType df name
                      let mname = "`"<> sname <> "`\n\n"
                      let minfo = maybe "" (<>" ") pkg <> mdl
#endif
                      let mname = "`"<> sname <> "`\n\n"
                      let minfo = maybe "" (<>" ") pkg <> mdl
                      case mdocu of
#if __GLASGOW_HASKELL__ >= 802
                        Nothing -> return $ mname <> minfo
                        Just docu -> return $ docu <> "\n\n" <> minfo
#else
                        Nothing -> return $ "`"<> sname <> "`\n" <> maybe "" (<>" ") pkg <> mdl
                        Just docu -> return docu
#endif
              return (info,docs,mrange)
        makeRequest hreq

      -- -------------------------------

      Core.ReqCodeAction req -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            (J.List diags) = params ^. J.context . J.diagnostics

        let
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "hlint") m  ) = [J.Command title cmd cmdparams]
            where
              title :: T.Text
              title = "Apply hint:" <> head (T.lines m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See hieOptions above
              cmd = "applyrefact:applyOne"
              -- need 'file' and 'start_pos'
              args = J.Array $ V.singleton $ J.toJSON $ ApplyRefact.AOP doc start
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m  ) = []
          -- TODO: make context specific commands for all sorts of things, such as refactorings
        let body = J.List $ concatMap makeCommand diags
        let rspMsg = Core.makeResponseMessage req body
        reactorSend rspMsg

      -- -------------------------------

      Core.ReqExecuteCommand req -> do
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" ++ show req
        -- cwd <- liftIO getCurrentDirectory
        -- liftIO $ U.logs $ "reactor:cwd:" ++ cwd
        let params = req ^. J.params
            command = params ^. J.command
            margs = params ^. J.arguments


        --liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
        let cmdparams = case margs of
              Just (J.List (x:_)) -> x
              _ -> J.Null
        callback <- hieResponseHelper (req ^. J.id) $ \obj -> do
          liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show obj
          case J.fromJSON obj of
            J.Success v -> do
              lid <- nextLspReqId
              reactorSend $ Core.makeResponseMessage req (J.Object mempty)
              let msg = fmServerApplyWorkspaceEditRequest lid $ J.ApplyWorkspaceEditParams v
              liftIO $ U.logs $ "ExecuteCommand sending edit: " ++ show msg
              reactorSend msg
            _ -> reactorSend $ Core.makeResponseMessage req obj
        let (plugin,cmd) = T.break (==':') command
        let preq = PReq Nothing Nothing (Just $ req ^. J.id) (const $ return ())
                     $ runPluginCommand plugin (T.drop 1 cmd) cmdparams callback
        makeRequest preq

      -- -------------------------------

      Core.ReqCompletion req -> do
        liftIO $ U.logs $ "reactor:got CompletionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument ^. J.uri
            pos = params ^. J.position

        mprefix <- getPrefixAtPos doc pos

        callback <- hieResponseHelper (req ^. J.id) $ \compls -> do
          let rspMsg = Core.makeResponseMessage req
                         $ J.Completions $ J.List compls
          reactorSend rspMsg
        case mprefix of
          Nothing -> liftIO $ callback $ IdeResponseOk []
          Just prefix -> do
            let hreq = PReq (Just doc) Nothing (Just $ req ^. J.id) callback
                         $ HaRe.getCompletions doc prefix
            makeRequest hreq

      Core.ReqCompletionItemResolve req -> do
        liftIO $ U.logs $ "reactor:got CompletionItemResolveRequest:" ++ show req
        let origCompl = req ^. J.params
            mquery = case J.fromJSON <$> origCompl ^. J.xdata of
                       Just (J.Success q) -> Just q
                       _ -> Nothing
        callback <- hieResponseHelper (req ^. J.id) $ \docs -> do
          let rspMsg = Core.makeResponseMessage req $
                         origCompl & J.documentation .~ docs
          reactorSend rspMsg
        let hreq = PReq Nothing Nothing (Just $ req ^. J.id) callback $ runEitherT $ do
              case mquery of
                Nothing -> return Nothing
                Just query -> do
                  res <- lift $ Hoogle.infoCmd' query
                  case res of
                    Right x -> return $ Just x
                    _ -> return Nothing
        makeRequest hreq

      -- -------------------------------

      Core.ReqDocumentHighlights req -> do
        liftIO $ U.logs $ "reactor:got DocumentHighlightsRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument ^. J.uri
            pos = params ^. J.position
        callback <- hieResponseHelper (req ^. J.id) $ \highlights -> do
          let rspMsg = Core.makeResponseMessage req $ J.List highlights
          reactorSend rspMsg
        let hreq = PReq (Just doc) Nothing (Just $ req ^. J.id) callback
                 $ HaRe.getReferencesInDoc doc pos
        makeRequest hreq

      -- -------------------------------
      Core.ReqDefinition req -> do
        liftIO $ U.logs $ "reactor:got DefinitionRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            pos = params ^. J.position
        callback <- hieResponseHelper (req ^. J.id) $ \loc -> do
            let rspMsg = Core.makeResponseMessage req loc
            reactorSend rspMsg
        let hreq = PReq (Just doc) Nothing (Just $ req ^. J.id) callback
                     $ fmap J.MultiLoc <$> HaRe.findDef doc pos
        makeRequest hreq
      -- -------------------------------
      Core.ReqDocumentFormatting req -> do
        liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            tabSize = params ^. J.options . J.tabSize
        callback <- hieResponseHelper (req ^. J.id) $ \textEdit -> do
            let rspMsg = Core.makeResponseMessage req $ J.List textEdit
            reactorSend rspMsg
        let hreq = PReq (Just $ doc ^. J.uri) Nothing (Just $ req ^. J.id) callback
                     $ Brittany.brittanyCmd tabSize doc Nothing
        makeRequest hreq
      -- -------------------------------
      Core.ReqDocumentRangeFormatting req -> do
        liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
        let params = req ^. J.params
            doc = params ^. J.textDocument
            range = params ^. J.range
            tabSize = params ^. J.options . J.tabSize
        callback <- hieResponseHelper (req ^. J.id) $ \textEdit -> do
            let rspMsg = Core.makeResponseMessage req $ J.List textEdit
            reactorSend rspMsg
        let hreq = PReq (Just $ doc ^. J.uri) Nothing (Just $ req ^. J.id) callback
                     $ Brittany.brittanyCmd tabSize doc (Just range)
        makeRequest hreq
      -- -------------------------------
      Core.ReqDocumentSymbols req -> do
        liftIO $ U.logs $ "reactor:got Document symbol request:" ++ show req
        let uri = req ^. J.params . J.textDocument . J.uri
        callback <- hieResponseHelper (req ^. J.id) $ \docSymbols -> do
            let rspMsg = Core.makeResponseMessage req $ J.List docSymbols
            reactorSend rspMsg
        let hreq = PReq (Just uri) Nothing (Just $ req ^. J.id) callback
                     $ HaRe.getSymbols uri
        makeRequest hreq
      -- -------------------------------
      Core.NotCancelRequest notif -> do
        liftIO $ U.logs $ "reactor:got CancelRequest:" ++ show notif
        let lid = notif ^. J.params . J.id
        liftIO $ atomically $ do
          wip <- readTVar wipTVar
          when (S.member lid wip) $ do
            modifyTVar' cancelReqTVar (S.insert lid)

      -- -------------------------------

      Core.NotDidChangeConfigurationParams notif -> do
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
           <> fromMaybe "" (T.append " package:" <$> pkg)
           <> " module:" <> modName
           <> " is:exact"
  debugm $ "hoogle query: " ++ T.unpack query
  res <- Hoogle.infoCmdFancyRender query
  case res of
    Right x -> return $ Just x
    Left _ -> return Nothing
-- ---------------------------------------------------------------------

  -- get hlint+GHC diagnostics and loads the typechecked module into the cache
requestDiagnostics :: TChan PluginRequest -> J.Uri -> Int -> R ()
requestDiagnostics cin file ver = do
  lf <- ask
  mc <- liftIO $ Core.config lf
  let sendOne pid (uri',ds) =
        publishDiagnostics maxToSend uri' Nothing (Map.fromList [(Just pid,SL.toSortedList ds)])
      sendEmpty = publishDiagnostics maxToSend file Nothing (Map.fromList [(Just "ghcmod",SL.toSortedList [])])
      maxToSend = maybe 50 maxNumberOfProblems mc

  -- mc <- asks Core.config
  let sendHlint = maybe True hlintOn mc
  when sendHlint $ do
    -- get hlint diagnostics
    let reql = PReq (Just file) (Just (file,ver)) Nothing (flip runReaderT lf . callbackl)
                 $ ApplyRefact.lintCmd' file
        callbackl (IdeResponseFail  err) = liftIO $ U.logs $ "got err" ++ show err
        callbackl (IdeResponseError err) = liftIO $ U.logs $ "got err" ++ show err
        callbackl (IdeResponseOk  diags) =
          case diags of
            (PublishDiagnosticsParams fp (List ds)) -> sendOne "hlint" (fp, ds)
        callbackl _ = error "impossible"
    liftIO $ atomically $ writeTChan cin reql

  -- get GHC diagnostics and loads the typechecked module into the cache
  let reqg = PReq (Just file) (Just (file,ver)) Nothing (flip runReaderT lf . callbackg)
               $ GhcMod.setTypecheckedModule file
      callbackg (IdeResponseFail  err) = liftIO $ U.logs $ "got err" ++ show err
      callbackg (IdeResponseError err) = liftIO $ U.logs $ "got err" ++ show err
      callbackg (IdeResponseOk    (pd, errs)) = do
        forM_ errs $ \e -> do
          reactorSend $
            fmServerShowMessageNotification J.MtError
              $ "Got error while processing diagnostics: " <> e
        let ds = Map.toList $ S.toList <$> pd
        case ds of
          [] -> sendEmpty
          _ -> mapM_ (sendOne "ghcmod") ds
      callbackg _ = error "impossible"

  liftIO $ atomically $ writeTChan cin reqg

-- ---------------------------------------------------------------------

-- | Manage the boilerplate for passing on any errors found in the IdeResponse
hieResponseHelper :: (MonadReader (Core.LspFuncs Config) m, MonadIO m)
  => J.LspId -> (t -> ReaderT (Core.LspFuncs Config) IO ()) -> m (IdeResponse t -> IO ())
hieResponseHelper lid action = do
  lf <- ask
  return $ \res -> flip runReaderT lf $
    case res of
      IdeResponseFail  err -> sendErrorResponse lid J.InternalError (T.pack $ show err)
      IdeResponseError err -> sendErrorResponse lid J.InternalError (T.pack $ show err)
      IdeResponseOk r -> action r
      _ -> error "impossible"

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
      , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["applyrefact:applyOne","hare:demote"]))
      }


hieHandlers :: TChan ReactorInput -> Core.Handlers
hieHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin Core.NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin Core.ReqRename
        , Core.definitionHandler                        = Just $ passHandler rin Core.ReqDefinition
        , Core.hoverHandler                             = Just $ passHandler rin Core.ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin Core.NotDidOpenTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin Core.NotDidSaveTextDocument
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin Core.NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin Core.NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin Core.NotCancelRequest
        , Core.didChangeConfigurationParamsHandler      = Just $ passHandler rin Core.NotDidChangeConfigurationParams
        , Core.responseHandler                          = Just $ passHandler rin Core.RspFromClient
        , Core.codeActionHandler                        = Just $ passHandler rin Core.ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin Core.ReqExecuteCommand
        , Core.completionHandler                        = Just $ passHandler rin Core.ReqCompletion
        , Core.completionResolveHandler                 = Just $ passHandler rin Core.ReqCompletionItemResolve
        , Core.documentHighlightHandler                 = Just $ passHandler rin Core.ReqDocumentHighlights
        , Core.documentFormattingHandler                = Just $ passHandler rin Core.ReqDocumentFormatting
        , Core.documentRangeFormattingHandler           = Just $ passHandler rin Core.ReqDocumentRangeFormatting
        , Core.documentSymbolHandler                    = Just $ passHandler rin Core.ReqDocumentSymbols
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> Core.OutMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (c notification)

-- ---------------------------------------------------------------------
