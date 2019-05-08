{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
-- | This module provides the interface to GHC, mainly for loading
-- modules while updating the module cache.

module Haskell.Ide.Engine.Ghc
  (
    setTypecheckedModule
  , Diagnostics
  , AdditionalErrs
  , cabalModuleGraphs
  , makeRevRedirMapFunc
  ) where

import           Bag
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Map.Strict                   as Map
import           Data.Monoid ((<>))
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import           ErrUtils

-- import qualified GhcMod.DynFlags                   as GM ( withDynFlags )
-- import qualified GhcMod.Error                      as GM ( gcatches, GHandler(..), ghcExceptionDoc )
-- import qualified GhcMod.Gap                        as GM ( mkErrStyle', renderGm )
-- import qualified GhcMod.ModuleLoader               as GM ( getModulesGhc' )
-- import qualified GhcMod.Monad                      as GM ( GmlT(..), getMMappedFiles, GmState(..), GhcModT, cradle )
-- import qualified GhcMod.Target                     as GM ( cabalResolvedComponents )
-- import qualified GhcMod.Types                      as GM ( IOish, GhcModError(..), GmGhcSession(..), GhcModState(..), GmModuleGraph(..), Cradle(..), gmcHomeModuleGraph )
-- import qualified GhcMod.Utils                      as GM ( mkRevRedirMapFunc )

import qualified GhcModCore                   as GM ( withDynFlags
                                                    , gcatches, GHandler(..), ghcExceptionDoc
                                                    , mkErrStyle', renderGm
                                                    , getModulesGhc'
                                                    , GmlT(..), getMMappedFiles, GmState(..), GhcModT, cradle
                                                    , cabalResolvedComponents
                                                    , IOish, GhcModError(..), GmGhcSession(..), GhcModState(..), GmModuleGraph(..), Cradle(..), gmcHomeModuleGraph
                                                    , mkRevRedirMapFunc )

import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           System.FilePath

import           DynFlags
import           GHC
import           IOEnv                             as G
import           HscTypes
import           Outputable                        (renderWithStyle)

-- ---------------------------------------------------------------------

type Diagnostics = Map.Map Uri (Set.Set Diagnostic)
type AdditionalErrs = [T.Text]

-- ---------------------------------------------------------------------

lspSev :: Severity -> DiagnosticSeverity
lspSev SevWarning = DsWarning
lspSev SevError   = DsError
lspSev SevFatal   = DsError
lspSev SevInfo    = DsInfo
lspSev _          = DsInfo

-- ---------------------------------------------------------------------
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

-- ---------------------------------------------------------------------

myWrapper :: GM.IOish m
  => (FilePath -> FilePath)
  -> GM.GmlT m ()
  -> GM.GmlT m (Diagnostics, AdditionalErrs)
myWrapper rfm action = do
  env <- getSession
  diagRef <- liftIO $ newIORef Map.empty
  errRef <- liftIO $ newIORef []
  let setLogger df = df { log_action = logDiag rfm errRef diagRef }
      setDeferTypedHoles = setGeneralFlag' Opt_DeferTypedHoles
      ghcErrRes msg = (Map.empty, [T.pack msg])
      handlers = errorHandlers ghcErrRes (srcErrToDiag (hsc_dflags env) rfm )
      action' = do
        GM.withDynFlags (setLogger . setDeferTypedHoles) action
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags,errs)
  GM.gcatches action' handlers

-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

setTypecheckedModule :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
setTypecheckedModule uri =
  pluginGetFile "setTypecheckedModule: " uri $ \fp -> do
    fileMap <- GM.getMMappedFiles
    debugm $ "setTypecheckedModule: file mapping state is: " ++ show fileMap
    rfm <- GM.mkRevRedirMapFunc
    let
      ghcErrRes msg = ((Map.empty, [T.pack msg]),Nothing,Nothing)
      progTitle = "Typechecking " <> T.pack (takeFileName fp)
    debugm "setTypecheckedModule: before ghc-mod"
    -- TODO:AZ: loading this one module may/should trigger loads of any
    -- other modules which currently have a VFS entry.  Need to make
    -- sure that their diagnostics are reported, and their module
    -- cache entries are updated.
    -- TODO: Are there any hooks we can use to report back on the progress?
    ((diags', errs), mtm, mpm) <- withIndefiniteProgress progTitle NotCancellable $ GM.gcatches
      (GM.getModulesGhc' (myWrapper rfm) fp)
      (errorHandlers ghcErrRes (return . ghcErrRes . show))
    debugm "setTypecheckedModule: after ghc-mod"

    canonUri <- canonicalizeUri uri
    let diags = Map.insertWith Set.union canonUri Set.empty diags'
    diags2 <- case (mpm,mtm) of
      (Just pm, Nothing) -> do
        debugm $ "setTypecheckedModule: Did get parsed module for: " ++ show fp
        cacheModule fp (Left pm)
        debugm "setTypecheckedModule: done"
        return diags

      (_, Just tm) -> do
        debugm $ "setTypecheckedModule: Did get typechecked module for: " ++ show fp
        sess <- fmap GM.gmgsSession . GM.gmGhcSession <$> GM.gmsGet

        -- set the session before we cache the module, so that deferred
        -- responses triggered by cacheModule can access it
        modifyMTS (\s -> s {ghcSession = sess})
        cacheModule fp (Right tm)
        debugm "setTypecheckedModule: done"
        return diags

      _ -> do
        debugm $ "setTypecheckedModule: Didn't get typechecked or parsed module for: " ++ show fp
        debugm $ "setTypecheckedModule: errs: " ++ show errs

        failModule fp

        let sev = Just DsError
            range = Range (Position 0 0) (Position 1 0)
            msgTxt = T.unlines errs
        let d = Diagnostic range sev Nothing (Just "ghcmod") msgTxt Nothing
        return $ Map.insertWith Set.union canonUri (Set.singleton d) diags

    return $ IdeResultOk (diags2,errs)

-- ---------------------------------------------------------------------

cabalModuleGraphs :: IdeGhcM [GM.GmModuleGraph]
cabalModuleGraphs = doCabalModuleGraphs
  where
    doCabalModuleGraphs :: (GM.IOish m) => GM.GhcModT m [GM.GmModuleGraph]
    doCabalModuleGraphs = do
      crdl <- GM.cradle
      case GM.cradleCabalFile crdl of
        Just _ -> do
          mcs <- GM.cabalResolvedComponents
          let graph = map GM.gmcHomeModuleGraph $ Map.elems mcs
          return graph
        Nothing -> return []

-- ---------------------------------------------------------------------

makeRevRedirMapFunc :: IdeGhcM (FilePath -> FilePath)
makeRevRedirMapFunc = make
  where
    make :: (GM.IOish m) => GM.GhcModT m (FilePath -> FilePath)
    make = GM.mkRevRedirMapFunc

-- ---------------------------------------------------------------------
