{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
module Haskell.Ide.Engine.Plugin.Bios(setTypecheckedModule, biosDescriptor) where

import           Bag
import           Control.Monad.IO.Class
import           Control.Lens hiding (cons, children)
import           Data.Aeson
import           Data.Function
import qualified Data.HashMap.Strict               as HM
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import           ErrUtils
import           Name
import           GHC.Generics

import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Plugin.HieExtras as Hie
import           Haskell.Ide.Engine.ArtifactMap
import qualified Language.Haskell.LSP.Types        as LSP
import qualified Language.Haskell.LSP.Types.Lens   as LSP
import           Language.Haskell.Refact.API       (hsNamessRdr)

import qualified GhcMod                            as GM
import qualified GhcMod.DynFlags                   as GM
import qualified GhcMod.Error                      as GM
import qualified GhcMod.Gap                        as GM
import qualified GhcMod.ModuleLoader               as GM
import qualified GhcMod.Monad                      as GM
import qualified GhcMod.SrcUtils                   as GM
import qualified GhcMod.Types                      as GM
import qualified GhcMod.Utils                      as GM
import qualified GhcMod.Target                     as GM

import           DynFlags
import           GHC
import           IOEnv                             as G
import           HscTypes
import           DataCon
import           TcRnTypes
import           Outputable hiding ((<>))
import qualified HIE.Bios as BIOS
import qualified HIE.Bios as BIOS
-- This function should be defined in HIE probably, nothing in particular
-- to do with BIOS
import qualified HIE.Bios.GHCApi as BIOS (withDynFlags)
import System.Directory


-- ---------------------------------------------------------------------

biosDescriptor :: PluginId -> PluginDescriptor
biosDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "bios"
  , pluginDesc = "bios"
  , pluginCommands =
      [ PluginCommand "check" "check a file for GHC warnings and errors" checkCmd ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Just hoverProvider
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

type Diagnostics = Map.Map Uri (Set.Set Diagnostic)
type AdditionalErrs = [T.Text]

checkCmd :: CommandFunc Uri (Diagnostics, AdditionalErrs)
checkCmd = CmdSync setTypecheckedModule

-- ---------------------------------------------------------------------

lspSev :: Severity -> DiagnosticSeverity
lspSev SevWarning = DsWarning
lspSev SevError   = DsError
lspSev SevFatal   = DsError
lspSev SevInfo    = DsInfo
lspSev _          = DsInfo


-- | Make an error which doesn't have its own location
unhelpfulSrcSpanErr :: T.Text -> IdeError
unhelpfulSrcSpanErr err =
  IdeError PluginError
            ("Unhelpful SrcSpan" <> ": \"" <> err <> "\"")
            Null

-- | Turn a 'SourceError' into the HIE 'Diagnostics' format.
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


-- | Run a Ghc action and capture any diagnostics and errors produced.
captureDiagnostics :: (GM.MonadIO m, GhcMonad m)
  => (FilePath -> FilePath)
  -> m r
  -> m (Diagnostics, AdditionalErrs, Maybe r)
captureDiagnostics rfm action = do
  env <- getSession
  diagRef <- liftIO $ newIORef Map.empty
  errRef <- liftIO $ newIORef []
  let setLogger df = df { log_action = logDiag rfm errRef diagRef }
      setDeferTypedHoles = setGeneralFlag' Opt_DeferTypedHoles
      ghcErrRes msg = do
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags, (T.pack msg) : errs, Nothing)
      to_diag x = do
        (d1, e1) <- srcErrToDiag (hsc_dflags env) rfm x
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (Map.unionWith Set.union d1 diags, e1 ++ errs, Nothing)


      handlers = errorHandlers ghcErrRes to_diag
      action' = do
        r <- BIOS.withDynFlags (setLogger . setDeferTypedHoles) action
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags,errs, Just r)
  GM.gcatches action' handlers

-- | Create a 'LogAction' which will be invoked by GHC when it tries to
-- write anything to `stdout`.
logDiag :: (FilePath -> FilePath) -> IORef AdditionalErrs -> IORef Diagnostics -> LogAction
-- type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logDiag rfm eref dref df _reason sev spn style msg = do
  eloc <- srcSpan2Loc rfm spn
  let msgTxt = T.pack $ renderWithStyle df msg style
  case eloc of
    Right (Location uri range) -> do
      let update = Map.insertWith Set.union uri l
            where l = Set.singleton diag
          diag = Diagnostic range (Just $ lspSev sev) Nothing (Just "ghcmod") msgTxt Nothing
      debugm $ "Writing diag" <> (show diag)
      modifyIORef' dref update
    Left _ -> do
      debugm $ "Writing err" <> (show msgTxt)
      modifyIORef' eref (msgTxt:)
      return ()


errorHandlers :: (Monad m) => (String -> m a) -> (SourceError -> m a) -> [GM.GHandler m a]
errorHandlers ghcErrRes renderSourceError = handlers
  where
      -- ghc throws GhcException, SourceError, GhcApiError and
      -- IOEnvFailure. ghc-mod-core throws GhcModError.
      handlers =
        [ GM.GHandler $ \(ex :: GM.GhcModError) ->
            ghcErrRes (show ex)
        , GM.GHandler $ \(ex :: IOEnvFailure) ->
            ghcErrRes (show ex)
        , GM.GHandler $ \(ex :: GhcApiError) ->
            ghcErrRes (show ex)
        , GM.GHandler $ \(ex :: SourceError) ->
            renderSourceError ex
        , GM.GHandler $ \(ex :: IOError) ->
            ghcErrRes (show ex)
        -- , GM.GHandler $ \(ex :: GM.SomeException) ->
        --     return $ ghcErrRes (show ex)
        ]



setTypecheckedModule :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
setTypecheckedModule uri =
  pluginGetFile "setTypecheckedModule: " uri $ \fp -> do
    debugm "setTypecheckedModule: before ghc-mod"
    let ghcErrRes msg = (Map.empty, [T.pack msg],Nothing)
    debugm "Loading file"
    (diags', errs, mmods) <- (captureDiagnostics id $ BIOS.loadFile fp)
    debugm "File, loaded"
    canonUri <- canonicalizeUri uri
    let diags = Map.insertWith Set.union canonUri Set.empty diags'
    debugm "setTypecheckedModule: after ghc-mod"
    debugm ("Diags: " <> show diags')

    let diagonal Nothing = (Nothing, Nothing)
        diagonal (Just (x, y)) = (Just x, Just y)
    diags2 <- case diagonal mmods of
      (Just pm, Nothing) -> do
        debugm $ "setTypecheckedModule: Did get parsed module for: " ++ show fp
        cacheModule fp (Left pm)
        debugm "setTypecheckedModule: done"
        return diags

      (_, Just tm) -> do
        debugm $ "setTypecheckedModule: Did get typechecked module for: " ++ show fp
        --sess <- fmap GM.gmgsSession . GM.gmGhcSession <$> GM.gmsGet

        -- set the session before we cache the module, so that deferred
        -- responses triggered by cacheModule can access it
        --modifyMTS (\s -> s {ghcSession = sess})
        cacheModule fp (Right tm)
        debugm "setTypecheckedModule: done"
        return diags

      _ -> do
        debugm $ "setTypecheckedModule: Didn't get typechecked or parsed module for: " ++ show fp
        --debugm $ "setTypecheckedModule: errs: " ++ show errs

        failModule fp

        let sev = Just DsError
            range = Range (Position 0 0) (Position 1 0)
            msgTxt = T.unlines errs
        let d = Diagnostic range sev Nothing (Just "ghcmod") msgTxt Nothing
        return $ Map.insertWith Set.union canonUri (Set.singleton d) diags

    return $ IdeResultOk (diags2,errs)


-- ---------------------------------------------------------------------
data TypeParams =
  TP { tpIncludeConstraints :: Bool
     , tpFile               :: Uri
     , tpPos                :: Position
     } deriving (Eq,Show,Generic)

customOptions :: Options
customOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}

instance FromJSON TypeParams where
  parseJSON = genericParseJSON customOptions
instance ToJSON TypeParams where
  toJSON = genericToJSON customOptions

typeCmd :: CommandFunc TypeParams [(Range,T.Text)]
typeCmd = CmdSync $ \(TP _bool uri pos) ->
  liftToGhc $ newTypeCmd pos uri

newTypeCmd :: Position -> Uri -> IdeM (IdeResult [(Range, T.Text)])
newTypeCmd newPos uri =
  pluginGetFile "newTypeCmd: " uri $ \fp ->
    ifCachedModule fp (IdeResultOk []) $ \tm info -> do
      debugm $ "newTypeCmd: " <> (show (newPos, uri))
      return $ IdeResultOk $ pureTypeCmd newPos tm info

pureTypeCmd :: Position -> GHC.TypecheckedModule -> CachedInfo -> [(Range,T.Text)]
pureTypeCmd newPos tm info =
    case mOldPos of
      Nothing -> []
      Just pos -> concatMap f (spanTypes pos)
  where
    mOldPos = newPosToOld info newPos
    typm = typeMap info
    spanTypes' pos = getArtifactsAtPos pos typm
    spanTypes pos = sortBy (cmp `on` fst) (spanTypes' pos)
    dflag = ms_hspp_opts $ pm_mod_summary $ tm_parsed_module tm
    unqual = mkPrintUnqualified dflag $ tcg_rdr_env $ fst $ tm_internals_ tm
    st = mkUserStyle dflag unqual AllTheWay

    f (range', t) =
      case oldRangeToNew info range' of
        (Just range) -> [(range , T.pack $ GM.pretty dflag st t)]
        _ -> []

cmp :: Range -> Range -> Ordering
cmp a b
  | a `isSubRangeOf` b = LT
  | b `isSubRangeOf` a = GT
  | otherwise = EQ

isSubRangeOf :: Range -> Range -> Bool
isSubRangeOf (Range sa ea) (Range sb eb) = sb <= sa && eb >= ea

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

hoverProvider :: HoverProvider
hoverProvider doc pos = runIdeResultT $ do
  info' <- IdeResultT $ newTypeCmd pos doc
  names' <- IdeResultT $ pluginGetFile "ghc-mod:hoverProvider" doc $ \fp ->
    ifCachedModule fp (IdeResultOk []) $ \(_ :: GHC.ParsedModule) info ->
      return $ IdeResultOk $ Hie.getSymbolsAtPoint pos info
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
              (Just $ LSP.CodeString $ LSP.LanguageString "haskell" $ "_ :: " <> typ, Just r)
            Just (_,name)
              | nnames == 1 ->
                (Just $ LSP.CodeString $ LSP.LanguageString "haskell" $ Hie.showName name <> " :: " <> typ, Just r)
              | otherwise ->
                (Just $ LSP.CodeString $ LSP.LanguageString "haskell" $ "_ :: " <> typ, Just r)
        [] -> case names of
          [] -> (Nothing, Nothing)
          ((r,_):_) -> (Nothing, Just r)
  return $ case mrange of
    Just r -> [LSP.Hover (LSP.List $ catMaybes [info]) (Just r)]
    Nothing -> []

-- ---------------------------------------------------------------------
