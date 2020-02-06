{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
-- | This module provides the interface to GHC, mainly for loading
-- modules while updating the module cache.

module Haskell.Ide.Engine.Ghc
  (
    setTypecheckedModule
  , Diagnostics(..)
  , AdditionalErrs
  -- , cabalModuleGraphs
  , makeRevRedirMapFunc
  ) where

import Debug.Trace

import           Bag
import           Control.Monad.IO.Class
import           Control.Monad                  ( when )
import           Data.IORef
import qualified Data.Map.Strict                   as Map
-- import qualified Data.IntMap.Strict                   as IM
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Aeson
import           ErrUtils

import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils

import           DynFlags
import           GHC
import qualified HscTypes

#if __GLASGOW_HASKELL__ < 808
import           Data.Semigroup ((<>), Semigroup)
import           Outputable                        (renderWithStyle)
#endif

import           Language.Haskell.LSP.Types        ( NormalizedUri(..), toNormalizedUri )

import           Haskell.Ide.Engine.GhcUtils
import           Haskell.Ide.Engine.GhcCompat as Compat
--import qualified Haskell.Ide.Engine.Plugin.HieExtras as Hie

import           Outputable hiding ((<>))
-- This function should be defined in HIE probably, nothing in particular
-- to do with BIOS
import qualified HIE.Bios.Ghc.Api as BIOS (withDynFlags)
import qualified HIE.Bios.Ghc.Load as BIOS

import System.Directory

-- import GhcMake ( moduleGraphNodes )
import GhcMonad

-- ---------------------------------------------------------------------

newtype Diagnostics = Diagnostics (Map.Map NormalizedUri (Set.Set Diagnostic))
  deriving (Show, Eq)

instance Semigroup Diagnostics where
  Diagnostics d1 <> Diagnostics d2 = Diagnostics (Map.unionWith Set.union d1 d2)

instance Monoid Diagnostics where
  mappend = (<>)
  mempty = Diagnostics mempty

instance Data.Aeson.ToJSON Diagnostics where
  toJSON (Diagnostics d) = Data.Aeson.toJSON
    (Map.mapKeys extractUri d :: Map.Map T.Text (Set.Set Diagnostic))
    where extractUri (NormalizedUri _ t) =  t

type AdditionalErrs = [T.Text]


-- ---------------------------------------------------------------------

lspSev :: WarnReason -> Severity -> DiagnosticSeverity
lspSev (Reason r) _
  | r `elem` [ Opt_WarnDeferredTypeErrors
             , Opt_WarnDeferredOutOfScopeVariables
             ]
  = DsError
lspSev _ SevWarning  = DsWarning
lspSev _ SevError    = DsError
lspSev _ SevFatal    = DsError
lspSev _ SevInfo     = DsInfo
lspSev _ _           = DsInfo

-- ---------------------------------------------------------------------

-- unhelpfulSrcSpanErr :: T.Text -> IdeError
-- unhelpfulSrcSpanErr err =
--   IdeError PluginError
--             ("Unhelpful SrcSpan" <> ": \"" <> err <> "\"")
--             Null

-- ---------------------------------------------------------------------

srcErrToDiag :: MonadIO m
  => DynFlags
  -> (FilePath -> FilePath)
  -> HscTypes.SourceError -> m (Diagnostics, AdditionalErrs)
srcErrToDiag df rfm se = do
  debugm "in srcErrToDiag"
  let errMsgs = bagToList $ HscTypes.srcErrorMessages se
      processMsg err = do
        let sev = Just DsError
            unqual = errMsgContext err
            st = mkErrStyle df unqual
            msgTxt = T.pack $ renderWithStyle df (pprLocErrMsg err) st
        eloc <- srcSpan2Loc rfm $ errMsgSpan err
        case eloc of
          Right (Location uri range) ->
            return $ Right (uri, Diagnostic range sev Nothing (Just "bios") msgTxt Nothing)
          Left _ -> return $ Left msgTxt
      processMsgs [] = return (Map.empty,[])
      processMsgs (x:xs) = do
        res <- processMsg x
        (m,es) <- processMsgs xs
        case res of
          Right (uri, diag) ->
            return (Map.insertWith Set.union (toNormalizedUri uri) (Set.singleton diag) m, es)
          Left e -> return (m, e:es)

  (diags, errs) <- processMsgs errMsgs
  return (Diagnostics diags, errs)


-- | Run a Ghc action and capture any diagnostics and errors produced.
captureDiagnostics :: (MonadIO m, GhcMonad m)
  => (FilePath -> FilePath)
  -> m r
  -> m (Diagnostics, AdditionalErrs, Maybe r)
captureDiagnostics rfm action = do
  env <- getSession
  diagRef <- liftIO $ newIORef $ Diagnostics mempty
  errRef <- liftIO $ newIORef []
  let setLogger df = df { log_action = logDiag rfm errRef diagRef }
      -- Running HIE on projects with -Werror breaks most of the features since all warnings
      -- will be treated with the same severity of type errors. In order to offer a more useful
      -- experience, we make sure warnings are always reported as warnings by setting -Wwarn
      unsetWErr df = unSetGeneralFlag' Opt_WarnIsError (emptyFatalWarningFlags df)
      -- Dont report the missing module warnings. Before disabling this warning, it was
      -- repeatedly shown to the user.
      unsetMissingHomeModules = flip wopt_unset Opt_WarnMissingHomeModules
      -- Dont get rid of comments while typechecking.
      -- Important for various operations that work on a typechecked module.
      setRawTokenStream = setGeneralFlag' Opt_KeepRawTokenStream

      ghcErrRes msg = pure (mempty, [T.pack msg], Nothing)
      to_diag x = do
        (d1, e1) <- srcErrToDiag (HscTypes.hsc_dflags env) rfm x
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (d1 <> diags, e1 ++ errs, Nothing)

      handlers = errorHandlers ghcErrRes to_diag

      foldDFlags :: (a -> DynFlags -> DynFlags) -> [a] -> DynFlags -> DynFlags
      foldDFlags f xs x = foldr f x xs

      setDeferTypeErrors =
          foldDFlags (flip wopt_set) [Opt_WarnTypedHoles, Opt_WarnDeferredTypeErrors, Opt_WarnDeferredOutOfScopeVariables]
          . foldDFlags setGeneralFlag' [Opt_DeferTypedHoles, Opt_DeferTypeErrors, Opt_DeferOutOfScopeVariables]

      action' = do
        r <- BIOS.withDynFlags (setRawTokenStream . unsetMissingHomeModules . setLogger . setDeferTypeErrors . unsetWErr) $
                action
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags,errs, Just r)
  gcatches action' handlers

-- | Create a 'LogAction' which will be invoked by GHC when it tries to
-- write anything to `stdout`.
logDiag :: (FilePath -> FilePath) -> IORef AdditionalErrs -> IORef Diagnostics -> LogAction
-- type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logDiag rfm eref dref df reason sev spn style msg = do
  eloc <- srcSpan2Loc rfm spn
  debugm $ "Diagnostics at Location: " <> show (spn, eloc)
  let msgString = renderWithStyle df msg style
      msgTxt = T.pack msgString
  case sev of
    -- These three verbosity levels are triggered by increasing verbosity.
    -- Normally the verbosity is set to 0 when the session is initialised but
    -- sometimes for debugging it is useful to override this and piping the messages
    -- to the normal debugging framework means they just show up in the normal log.
    SevOutput -> debugm msgString
    SevDump -> debugm msgString
    SevInfo -> debugm msgString
    _ ->  do
      case eloc of
        Right (Location uri range) -> do
          let update = Map.insertWith Set.union (toNormalizedUri uri) l
                where l = Set.singleton diag
              diag = Diagnostic range (Just $ lspSev reason sev) Nothing (Just "bios") msgTxt Nothing
          debugm $ "Writing diag " <> (show diag)
          modifyIORef' dref (\(Diagnostics u) -> Diagnostics (update u))
        Left _ -> do
          debugm $ "Writing err " <> (show msgTxt)
          modifyIORef' eref (msgTxt:)
          return ()

-- | Load a module from a filepath into the cache, first check the cache
-- to see if it's already there.
setTypecheckedModule :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
setTypecheckedModule uri = do
  liftIO $ traceEventIO ("START typecheck" ++ show uri)
  pluginGetFile "setTypecheckedModule: " uri $ \_fp -> do
    debugm "setTypecheckedModule: before ghc-mod"
    debugm "Loading file"
    res <- setTypecheckedModule_load uri
    liftIO $ traceEventIO ("STOP typecheck" ++ show uri)
    return res

-- Hacky, need to copy hs-boot file if one exists for a module
-- This is because the virtual file gets created at VFS-1234.hs and
-- then GHC looks for the boot file at VFS-1234.hs-boot
--
-- This strategy doesn't work if the user wants to edit the boot file but
-- not save it and expect the VFS to save them. However, I expect that HIE
-- already didn't deal with boot files correctly.
copyHsBoot :: FilePath -> FilePath -> IO ()
copyHsBoot fp mapped_fp = do
  ex <- doesFileExist (fp <> "-boot")
  when ex $ copyFile (fp <> "-boot") (mapped_fp <> "-boot")


loadFile :: (FilePath -> FilePath) -> (FilePath, FilePath)
         -> IdeGhcM (Diagnostics, AdditionalErrs,
                     Maybe (Maybe TypecheckedModule, [TypecheckedModule]))
loadFile rfm t =
    captureDiagnostics rfm (withProgress "loading" NotCancellable $ \f -> BIOS.loadFileWithMessage (Just $ toMessager f) t)

-- | Actually load the module if it's not in the cache
setTypecheckedModule_load :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
setTypecheckedModule_load uri =
  pluginGetFile "setTypecheckedModule: " uri $ \fp -> do
    debugm "setTypecheckedModule: before ghc-mod"
    debugm "Loading file"
    getPersistedFile uri >>= \case
      Nothing -> return $ IdeResultOk (Diagnostics mempty, [])
      Just mapped_fp -> do
        liftIO $ copyHsBoot fp mapped_fp
        rfm <- reverseFileMap
        -- TODO:AZ: loading this one module may/should trigger loads of any
        -- other modules which currently have a VFS entry.  Need to make
        -- sure that their diagnostics are reported, and their module
        -- cache entries are updated.
        -- TODO: Are there any hooks we can use to report back on the progress?
        (Diagnostics diags', errs, mmods) <- loadFile rfm (fp, mapped_fp)
        debugm "File, loaded"
        canonUri <- toNormalizedUri <$> canonicalizeUri uri
        let diags = Map.insertWith Set.union canonUri Set.empty diags'
        debugm "setTypecheckedModule: after ghc-mod"
        debugm ("Diags: " <> show diags')
        let collapse Nothing = Nothing
            collapse (Just (n, _xs)) = n

            mtypechecked_module = collapse mmods
        case mtypechecked_module of
          Just _tm -> do
            debugm $ "setTypecheckedModule: Did get typechecked module for: " ++ show fp

            -- set the session before we cache the module, so that deferred
            -- responses triggered by cacheModule can access it

            Session sess <- GhcT pure
            modifyMTS (\s -> s {ghcSession = Just sess})
            cacheModules rfm [_tm]
            debugm "setTypecheckedModule: done"

          Nothing -> do
            debugm $ "setTypecheckedModule: Didn't get typechecked or parsed module for: " ++ show fp
            failModule fp

        -- Turn any fatal exceptions thrown by GHC into a diagnostic for
        -- this module so it appears somewhere permanent in the UI.
        let diags2 =
              case mtypechecked_module of
                Nothing ->
                  let sev = Just DsError
                      range = Range (Position 0 0) (Position 1 0)
                      msgTxt = T.unlines errs
                      d = Diagnostic range sev Nothing (Just "bios") msgTxt Nothing
                  in Map.insertWith Set.union canonUri (Set.singleton d) diags
                Just {} -> diags

        return $ IdeResultOk (Diagnostics diags2,errs)

{-

-- TODO: Come up with a reasonable approach to generate a module graph
-- for a given component or package.

-- TODO: make this work for all components
cabalModuleGraphs :: IdeGhcM [GM.GmModuleGraph]
cabalModuleGraphs = do
  mg <- getModuleGraph
  let (graph, _) = moduleGraphNodes False (Compat.mgModSummaries mg)
      msToModulePath ms =
        case ml_hs_file (ms_location ms) of
          Nothing -> []
          Just fp -> [ModulePath mn fp]
        where mn = moduleName (ms_mod ms)
      nodeMap = IM.fromList [(node_key n,n) | n <- nodes]
      nodes = verticesG graph
      gmg = Map.fromList
              [(mp,Set.fromList deps)
                  | node <- nodes
                  , mp <- msToModulePath (node_payload node)
                  , let int_deps = node_dependencies node
                        deps = [ d | i <- int_deps
                                   , Just dep_node <- pure $ IM.lookup i nodeMap
                                   , d <- msToModulePath (node_payload dep_node)
                               ]
                  ]
  pure [GmModuleGraph gmg]
-}

-- ---------------------------------------------------------------------

makeRevRedirMapFunc :: IdeGhcM (FilePath -> FilePath)
makeRevRedirMapFunc = reverseFileMap

-- ---------------------------------------------------------------------
