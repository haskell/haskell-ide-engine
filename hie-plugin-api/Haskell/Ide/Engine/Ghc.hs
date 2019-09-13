{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
-- | This module provides the interface to GHC, mainly for loading
-- modules while updating the module cache.

module Haskell.Ide.Engine.Ghc
  (
    setTypecheckedModule
  , Diagnostics(..)
  , AdditionalErrs
  , cabalModuleGraphs
  , makeRevRedirMapFunc
  ) where

import           Bag
import           Control.Monad.IO.Class
import           Control.Monad                  ( when )
import           Data.IORef
import qualified Data.Map.Strict                   as Map
import qualified Data.IntMap.Strict                   as IM
import           Data.Semigroup ((<>), Semigroup)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Aeson
import           Data.Coerce
import           ErrUtils

import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils

import           DynFlags
import qualified EnumSet as ES
import           GHC
import           IOEnv                             as G
import           HscTypes
import           Outputable                        (renderWithStyle)
import           Language.Haskell.LSP.Types        ( NormalizedUri(..), toNormalizedUri )

import           Haskell.Ide.Engine.GhcUtils
--import qualified Haskell.Ide.Engine.Plugin.HieExtras as Hie

import           Outputable hiding ((<>))
-- This function should be defined in HIE probably, nothing in particular
-- to do with BIOS
import qualified HIE.Bios.Ghc.Api as BIOS (withDynFlags, setDeferTypeErrors)
import qualified HIE.Bios.Ghc.Load as BIOS
import qualified HIE.Bios.Flags as BIOS (CradleError)
import Debug.Trace

import System.Directory

import GhcProject.Types as GM
import Digraph (Node(..), verticesG)
import GhcMake ( moduleGraphNodes )
import GhcMonad


newtype Diagnostics = Diagnostics (Map.Map NormalizedUri (Set.Set Diagnostic))
  deriving (Show, Eq)

instance Semigroup Diagnostics where
  Diagnostics d1 <> Diagnostics d2 = Diagnostics (Map.unionWith Set.union d1 d2)

instance Monoid Diagnostics where
  mappend = (<>)
  mempty = Diagnostics mempty

instance Data.Aeson.ToJSON Diagnostics where
  toJSON (Diagnostics d) = Data.Aeson.toJSON
    (Map.mapKeys coerce d :: Map.Map T.Text (Set.Set Diagnostic))

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
  -> SourceError -> m (Diagnostics, AdditionalErrs)
srcErrToDiag df rfm se = do
  debugm "in srcErrToDiag"
  let errMsgs = bagToList $ srcErrorMessages se
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
      unsetWErr df = unSetGeneralFlag' Opt_WarnIsError (df {fatalWarningFlags = ES.empty})

      ghcErrRes msg = pure (mempty, [T.pack msg], Nothing)
      to_diag x = do
        (d1, e1) <- srcErrToDiag (hsc_dflags env) rfm x
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (d1 <> diags, e1 ++ errs, Nothing)

      handlers = errorHandlers ghcErrRes to_diag

      action' = do
        r <- BIOS.withDynFlags (setLogger . BIOS.setDeferTypeErrors . unsetWErr) $
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
  traceShowM (spn, eloc)
  let msgTxt = T.pack $ renderWithStyle df msg style
  case eloc of
    Right (Location uri range) -> do
      let update = Map.insertWith Set.union (toNormalizedUri uri) l
            where l = Set.singleton diag
          diag = Diagnostic range (Just $ lspSev reason sev) Nothing (Just "bios") msgTxt Nothing
      debugm $ "Writing diag" <> (show diag)
      modifyIORef' dref (\(Diagnostics u) -> Diagnostics (update u))
    Left _ -> do
      debugm $ "Writing err" <> (show msgTxt)
      modifyIORef' eref (msgTxt:)
      return ()


errorHandlers :: (String -> m a) -> (SourceError -> m a) -> [ErrorHandler m a]
errorHandlers ghcErrRes renderSourceError = handlers
  where
      -- ghc throws GhcException, SourceError, GhcApiError and
      -- IOEnvFailure. ghc-mod-core throws GhcModError.
      handlers =
        [ ErrorHandler $ \(ex :: IOEnvFailure) ->
            ghcErrRes (show ex)
        , ErrorHandler $ \(ex :: GhcApiError) ->
            ghcErrRes (show ex)
        , ErrorHandler $ \(ex :: SourceError) ->
            renderSourceError ex
        , ErrorHandler $ \(ex :: IOError) ->
            ghcErrRes (show ex)
        , ErrorHandler $ \(ex :: BIOS.CradleError) ->
            ghcErrRes (show ex)
        ]


-- | Load a module from a filepath into the cache, first check the cache
-- to see if it's already there.
setTypecheckedModule :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
setTypecheckedModule uri =
  pluginGetFile "setTypecheckedModule: " uri $ \_fp -> do
    debugm "setTypecheckedModule: before ghc-mod"
    debugm "Loading file"
    -- mapped_fp <- persistVirtualFile uri
    -- ifCachedModuleM mapped_fp (setTypecheckedModule_load uri) cont
    setTypecheckedModule_load uri

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
    withProgress "loading" NotCancellable $ \f -> (captureDiagnostics rfm $ BIOS.loadFileWithMessage (Just $ toMessager f) t)

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
        let collapse Nothing = (Nothing, [])
            collapse (Just (n, xs)) = (n, xs)

        case collapse mmods of
          --Just (Just pm, Nothing) -> do
          --  debugm $ "setTypecheckedModule: Did get parsed module for: " ++ show fp
          -- cacheModule fp (Left pm)
          -- debugm "setTypecheckedModule: done"
          --  return diags

          (Just _tm, ts) -> do
            debugm $ "setTypecheckedModule: Did get typechecked module for: " ++ show fp
            --sess <- fmap GM.gmgsSession . GM.gmGhcSession <$> GM.gmsGet

            -- set the session before we cache the module, so that deferred
            -- responses triggered by cacheModule can access it

            Session sess <- GhcT pure
            modifyMTS (\s -> s {ghcSession = Just sess})
            cacheModules rfm ts
            --cacheModules rfm [tm]
            debugm "setTypecheckedModule: done"

          (Nothing, ts) -> do
            debugm $ "setTypecheckedModule: Didn't get typechecked or parsed module for: " ++ show fp
            --debugm $ "setTypecheckedModule: errs: " ++ show errs
            cacheModules rfm ts
            failModule fp

        return $ IdeResultOk (Diagnostics diags,errs)

-- TODO: make this work for all components
cabalModuleGraphs :: IdeGhcM [GM.GmModuleGraph]
cabalModuleGraphs = do
  mg <- getModuleGraph
  let (graph, _) = moduleGraphNodes False (mgModSummaries mg)
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

-- ---------------------------------------------------------------------

makeRevRedirMapFunc :: IdeGhcM (FilePath -> FilePath)
makeRevRedirMapFunc = reverseFileMap

-- ---------------------------------------------------------------------
