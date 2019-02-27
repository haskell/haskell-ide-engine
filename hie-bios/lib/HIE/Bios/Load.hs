{-# LANGUAGE ScopedTypeVariables #-}
module HIE.Bios.Load ( loadFile ) where

import CoreMonad (liftIO)
import DynFlags (gopt_set, wopt_set, WarningFlag(Opt_WarnTypedHoles))
import GHC
import qualified GHC as G
import qualified Exception as GE
import HscTypes (ModSummary)
import Outputable

import HIE.Bios.Doc (getStyle)
import HIE.Bios.GHCApi
import HIE.Bios.Gap
import System.Directory
import EnumSet
import Control.Monad (filterM)

-- | Obtaining type of a target expression. (GHCi's type:)
loadFile :: GhcMonad m
         => FilePath     -- ^ A target file.
         -> m (G.ParsedModule, TypecheckedModule)
loadFile file = do
  pprTraceM "loadFile:1" (ppr (file))
  dir <- liftIO $ getCurrentDirectory
  pprTraceM "loadFile:2" (ppr dir)
  body
  where
    body = inModuleContext file $ \dflag _style -> do
        modSum <- fileModSummary file
        pprTraceM "loadFile:3" (ppr $ optLevel dflag)
        pprTraceM "loadFile:4" (ppr $ show (EnumSet.toList (generalFlags dflag)))
        p <- G.parseModule modSum
        tcm <- G.typecheckModule p
        return $ (p, tcm)

fileModSummary :: GhcMonad m => FilePath -> m ModSummary
fileModSummary file = do
    mss <- getModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms

withContext :: (GhcMonad m) => m a -> m a
withContext action = G.gbracket setup teardown body
  where
    setup = G.getContext
    teardown = setCtx
    body _ = do
        topImports >>= setCtx
        action
    topImports = do
        mss <- getModSummaries <$> G.getModuleGraph
        map modName <$> filterM isTop mss
    isTop mos = lookupMod mos `GE.gcatch` (\(_ :: GE.IOException) -> returnFalse)
    lookupMod mos = G.lookupModule (G.ms_mod_name mos) Nothing >> return True
    returnFalse = return False
    modName = G.IIModule . G.moduleName . G.ms_mod
    setCtx = G.setContext


inModuleContext :: GhcMonad m => FilePath -> (DynFlags -> PprStyle -> m a) -> m a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do

    df <- getSessionDynFlags
    pprTraceM "loadFile:3" (ppr $ optLevel df)
    pprTraceM "loadFile:4" (text $ show (EnumSet.toList (generalFlags df)))
    setTargetFiles [file]
    withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle dflag
        action dflag style

setDeferTypeErrors :: DynFlags -> DynFlags
setDeferTypeErrors dflag = gopt_set dflag G.Opt_DeferTypeErrors

setWarnTypedHoles :: DynFlags -> DynFlags
setWarnTypedHoles dflag = wopt_set dflag Opt_WarnTypedHoles
