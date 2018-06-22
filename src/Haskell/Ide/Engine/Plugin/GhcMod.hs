{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.Engine.Plugin.GhcMod where

import           Bag
import           Control.Monad.IO.Class
import           Data.Aeson
#if __GLASGOW_HASKELL__ < 802
import           Data.Aeson.Types
#endif
import           Data.Function
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict                   as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           DynFlags
import           ErrUtils
import qualified Exception                         as G
import           GHC
import           IOEnv                             as G
import           GHC.Generics
import qualified GhcMod                            as GM
import qualified GhcMod.DynFlags                   as GM
import qualified GhcMod.Error                      as GM
import qualified GhcMod.Gap                        as GM
import qualified GhcMod.ModuleLoader               as GM
import qualified GhcMod.Monad                      as GM
import qualified GhcMod.SrcUtils                   as GM
import qualified GhcMod.Types                      as GM
import qualified GhcMod.Utils                      as GM
import qualified GhcMod.Exe.CaseSplit              as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.ArtifactMap
import           HscTypes
import           TcRnTypes
import           Outputable                        (renderWithStyle, mkUserStyle, Depth(..))

-- ---------------------------------------------------------------------

ghcmodDescriptor :: PluginDescriptor
ghcmodDescriptor = PluginDescriptor
  {
    pluginName = "ghc-mod"
  , pluginDesc = "ghc-mod is a backend program to enrich Haskell programming "
              <> "in editors. It strives to offer most of the features one has come to expect "
              <> "from modern IDEs in any editor."
  , pluginCommands =
      [ PluginCommand "check" "check a file for GHC warnings and errors" checkCmd
      , PluginCommand "lint" "Check files using `hlint'" lintCmd
      , PluginCommand "info" "Look up an identifier in the context of FILE (like ghci's `:info')" infoCmd
      , PluginCommand "type" "Get the type of the expression under (LINE,COL)" typeCmd
      , PluginCommand "casesplit" "Generate a pattern match for a binding under (LINE,COL)" splitCaseCmd
      ]
  }

-- ---------------------------------------------------------------------

type Diagnostics = Map.Map Uri (Set.Set Diagnostic)
type AdditionalErrs = [T.Text]

checkCmd :: CommandFunc Uri (Diagnostics, AdditionalErrs)
checkCmd = CmdSync $ \ uri ->
  setTypecheckedModule uri

-- ---------------------------------------------------------------------

lspSev :: Severity -> DiagnosticSeverity
lspSev SevWarning = DsWarning
lspSev SevError   = DsError
lspSev SevFatal   = DsError
lspSev SevInfo    = DsInfo
lspSev _          = DsInfo

-- type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logDiag :: (FilePath -> FilePath) -> IORef AdditionalErrs -> IORef Diagnostics -> LogAction
logDiag rfm eref dref df _reason sev spn style msg = do
  eloc <- srcSpan2Loc rfm spn
  let msgTxt = T.pack $ renderWithStyle df msg style
  case eloc of
    Right (Location uri range) -> do
      let update = Map.insertWith Set.union uri l
            where l = Set.singleton diag
          diag = Diagnostic range (Just $ lspSev sev) Nothing (Just "ghcmod") msgTxt Nothing
      modifyIORef' dref update
    Left _ -> do
      modifyIORef' eref (msgTxt:)
      return ()

unhelpfulSrcSpanErr :: T.Text -> IdeError
unhelpfulSrcSpanErr err =
  IdeError PluginError
            ("Unhelpful SrcSpan" <> ": \"" <> err <> "\"")
            Null

srcErrToDiag :: MonadIO m
  => DynFlags
  -> (FilePath -> FilePath)
  -> SourceError -> m (Diagnostics, AdditionalErrs)
srcErrToDiag df rfm se = do
  debugm "in srcErrToDiag"
  let errMsgs = bagToList $ srcErrorMessages se
      processMsg err = do
        let sev = Just DsError
            unqual = errMsgContext err
            st = GM.mkErrStyle' df unqual
            msgTxt = T.pack $ renderWithStyle df (pprLocErrMsg err) st
        eloc <- srcSpan2Loc rfm $ errMsgSpan err
        case eloc of
          Right (Location uri range) ->
            return $ Right (uri, Diagnostic range sev Nothing (Just "ghcmod") msgTxt Nothing)
          Left _ -> return $ Left msgTxt
      processMsgs [] = return (Map.empty,[])
      processMsgs (x:xs) = do
        res <- processMsg x
        (m,es) <- processMsgs xs
        case res of
          Right (uri, diag) ->
            return (Map.insertWith Set.union uri (Set.singleton diag) m, es)
          Left e -> return (m, e:es)
  processMsgs errMsgs

myLogger :: GM.IOish m
  => (FilePath -> FilePath)
  -> GM.GmlT m ()
  -> GM.GmlT m (Diagnostics, AdditionalErrs)
myLogger rfm action = do
  env <- getSession
  diagRef <- liftIO $ newIORef Map.empty
  errRef <- liftIO $ newIORef []
  let setLogger df = df { log_action = logDiag rfm errRef diagRef }
      ghcErrRes msg = (Map.empty, [T.pack msg])
      handlers = errorHandlers ghcErrRes (srcErrToDiag (hsc_dflags env) rfm )
      action' = do
        GM.withDynFlags setLogger action
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags,errs)
  GM.gcatches action' handlers

errorHandlers :: (Monad m) => (String -> a) -> (SourceError -> m a) -> [GM.GHandler m a]
errorHandlers ghcErrRes renderSourceError = handlers
  where
      -- ghc throws GhcException, SourceError, GhcApiError and
      -- IOEnvFailure. ghc-mod-core throws GhcModError.
      handlers =
        [ GM.GHandler $ \(ex :: GM.GhcModError) ->
            return $ ghcErrRes (show ex)
        , GM.GHandler $ \(ex :: IOEnvFailure) ->
            return $ ghcErrRes (show ex)
        , GM.GHandler $ \(ex :: GhcApiError) ->
            return $ ghcErrRes (show ex)
        , GM.GHandler $ \(ex :: SourceError) ->
            renderSourceError ex
        , GM.GHandler $ \(ex :: GhcException) ->
            return $ ghcErrRes $ GM.renderGm $ GM.ghcExceptionDoc ex
        , GM.GHandler $ \(ex :: IOError) ->
            return $ ghcErrRes (show ex)
        -- , GM.GHandler $ \(ex :: GM.SomeException) ->
        --     return $ ghcErrRes (show ex)
        ]

setTypecheckedModule :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
setTypecheckedModule uri =
  pluginGetFile "setTypecheckedModule: " uri $ \fp -> do
    fileMap <- GM.getMMappedFiles
    debugm $ "setTypecheckedModule: file mapping state is: " ++ show fileMap
    rfm <- GM.mkRevRedirMapFunc
    let
      ghcErrRes msg = ((Map.empty, [T.pack msg]),Nothing)
    debugm "setTypecheckedModule: before ghc-mod"
    ((diags', errs), mtm) <- GM.gcatches
                              (GM.getTypecheckedModuleGhc' (myLogger rfm) fp)
                              (errorHandlers ghcErrRes (return . ghcErrRes . show))
    debugm "setTypecheckedModule: after ghc-mod"
    canonUri <- canonicalizeUri uri
    let diags = Map.insertWith Set.union canonUri Set.empty diags'
    case mtm of
      Nothing -> do
        debugm $ "setTypecheckedModule: Didn't get typechecked module for: " ++ show fp

        failModule fp (T.unlines errs)

        return $ IdeResultOk (diags,errs)
      Just tm -> do
        debugm $ "setTypecheckedModule: Did get typechecked module for: " ++ show fp
        typm <- GM.unGmlT $ genTypeMap tm
        sess <- fmap GM.gmgsSession . GM.gmGhcSession <$> GM.gmsGet
        let cm = CachedModule tm (genLocMap tm) typm rfm return return
        cacheModule fp cm
        modifyMTS (\s -> s {ghcSession = sess})
        debugm "setTypecheckedModule: done"
        return $ IdeResultOk (diags,errs)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc Uri T.Text
lintCmd = CmdSync $ \ uri ->
  lintCmd' uri

lintCmd' :: Uri -> IdeGhcM (IdeResult T.Text)
lintCmd' uri =
  pluginGetFile "lint: " uri $ \file ->
    fmap T.pack <$> runGhcModCommand (GM.lint GM.defaultLintOpts file)

-- ---------------------------------------------------------------------

customOptions :: Options
customOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}

data InfoParams =
  IP { ipFile :: Uri
     , ipExpr :: T.Text
     } deriving (Eq,Show,Generic)

instance FromJSON InfoParams where
  parseJSON = genericParseJSON customOptions
instance ToJSON InfoParams where
  toJSON = genericToJSON customOptions

infoCmd :: CommandFunc InfoParams T.Text
infoCmd = CmdSync $ \(IP uri expr) ->
  infoCmd' uri expr

infoCmd' :: Uri -> T.Text -> IdeGhcM (IdeResult T.Text)
infoCmd' uri expr =
  pluginGetFile "info: " uri $ \file ->
    fmap T.pack <$> runGhcModCommand (GM.info file (GM.Expression (T.unpack expr)))

-- ---------------------------------------------------------------------
data TypeParams =
  TP { tpIncludeConstraints :: Bool
     , tpFile               :: Uri
     , tpPos                :: Position
     } deriving (Eq,Show,Generic)

instance FromJSON TypeParams where
  parseJSON = genericParseJSON customOptions
instance ToJSON TypeParams where
  toJSON = genericToJSON customOptions

typeCmd :: CommandFunc TypeParams [(Range,T.Text)]
typeCmd = CmdSync $ \(TP _bool uri pos) ->
  liftToGhc $ newTypeCmd pos uri

newTypeCmd :: Position -> Uri -> IdeM (IdeResult [(Range, T.Text)])
newTypeCmd newPos uri =
  pluginGetFile "newTypeCmd: " uri $ \fp -> do
      mcm <- getCachedModule fp
      case mcm of
        ModuleCached cm _ -> return $ IdeResultOk $ pureTypeCmd newPos cm
        _ -> return $ IdeResultOk []

pureTypeCmd :: Position -> CachedModule -> [(Range,T.Text)]
pureTypeCmd newPos cm  =
    case mOldPos of
      Nothing -> []
      Just pos -> concatMap f (spanTypes pos)
  where
    mOldPos = newPosToOld cm newPos
    tm = tcMod cm
    typm = typeMap cm
    spanTypes' pos = getArtifactsAtPos pos typm
    spanTypes pos = sortBy (cmp `on` fst) (spanTypes' pos)
    dflag = ms_hspp_opts $ pm_mod_summary $ tm_parsed_module tm
    unqual = mkPrintUnqualified dflag $ tcg_rdr_env $ fst $ tm_internals_ tm
#if __GLASGOW_HASKELL__ >= 802
    st = mkUserStyle dflag unqual AllTheWay
#else
    st = mkUserStyle unqual AllTheWay
#endif

    f (range', t) =
      case oldRangeToNew cm range' of
        (Just range) -> [(range , T.pack $ GM.pretty dflag st t)]
        _ -> []

cmp :: Range -> Range -> Ordering
cmp a b
  | a `isSubRangeOf` b = LT
  | b `isSubRangeOf` a = GT
  | otherwise = EQ

isSubRangeOf :: Range -> Range -> Bool
isSubRangeOf (Range sa ea) (Range sb eb) = sb <= sa && eb >= ea



data DocumentPosition =
  DocPos {
        docFile :: Uri
      , docPos  :: Position
      } deriving (Eq,Generic,Show)

instance FromJSON DocumentPosition where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3}
instance ToJSON DocumentPosition where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3}

splitCaseCmd ::CommandFunc DocumentPosition WorkspaceEdit
splitCaseCmd = CmdSync $ \(DocPos uri pos) -> do
  splitCaseCmd' uri pos

splitCaseCmd' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
splitCaseCmd' uri position =
  pluginGetFile "splitCaseCmd: " uri $ \path -> do
    origText <- GM.withMappedFile path $ liftIO . T.readFile
    cachedMod <- getCachedModule path
    case cachedMod of
      ModuleCached checkedModule _ -> runGhcModCommand $ do
        Just splitResult <- GM.splits' path (tcMod checkedModule) line column
        return $ splitResultToWorkspaceEdit origText splitResult
      ModuleFailed errText -> return $ IdeResultFail $ IdeError PluginError (T.append "hie-ghc-mod: " errText) Null
      ModuleLoading -> return $ IdeResultOk mempty
  where
    (line, column) = unPos position

    -- | Given the range and text to replace, construct a 'WorkspaceEdit'
    -- by diffing the change against the current text.
    splitResultToWorkspaceEdit :: T.Text -> GM.SplitResult -> WorkspaceEdit
    splitResultToWorkspaceEdit originalText (GM.SplitResult replaceFromLine replaceFromCol replaceToLine replaceToCol replaceWith) =
      diffText (uri, originalText) newText
      where
        before = takeUntil (toPos (replaceFromLine, replaceFromCol)) originalText
        after = dropUntil (toPos (replaceToLine, replaceToCol)) originalText
        newText = before <> replaceWith <> after

    -- | Take the first part of text until the given position.
    -- Returns all characters before the position.
    takeUntil :: Position -> T.Text -> T.Text
    takeUntil (Position l c) txt =
      T.unlines takeLines <> takeCharacters
      where
        textLines = T.lines txt
        takeLines = take l textLines
        takeCharacters = T.take c (textLines !! c)

    -- | Drop the first part of text until the given position.
    -- Returns all characters after and including the position.
    dropUntil :: Position -> T.Text -> T.Text
    dropUntil (Position l c) txt = dropCharacters
      where
        textLines = T.lines txt
        dropLines = drop l textLines
        dropCharacters = T.drop c (T.unlines dropLines)

-- ---------------------------------------------------------------------

runGhcModCommand :: IdeGhcM a
                 -> IdeGhcM (IdeResult a)
runGhcModCommand cmd =
  (IdeResultOk <$> cmd) `G.gcatch`
    \(e :: GM.GhcModError) ->
      return $
      IdeResultFail $
      IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Null
