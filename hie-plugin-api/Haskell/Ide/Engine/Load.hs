{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Load ( loadFile ) where

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
import Control.Monad (filterM, forM, void)

#if __GLASGOW_HASKELL__ < 806
pprTraceM x s = pprTrace x s (return ())
#endif

-- | Obtaining type of a target expression. (GHCi's type:)
loadFile :: GhcMonad m
         => (FilePath, FilePath)     -- ^ A target file.
         -> m (G.ParsedModule, TypecheckedModule)
loadFile file = do
  dir <- liftIO $ getCurrentDirectory
  pprTraceM "loadFile:2" (text dir)
  body
  where
    body = inModuleContext file $ \dflag _style -> do
        modSum <- fileModSummary (snd file)
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


inModuleContext :: GhcMonad m => (FilePath, FilePath) -> (DynFlags -> PprStyle -> m a) -> m a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do

    df <- getSessionDynFlags
    pprTraceM "loadFile:3" (ppr $ optLevel df)
    setTargetFiles [file]
    pprTraceM "loaded" (text (fst file) $$ text (snd file))
    withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle dflag
        action dflag style

setDeferTypeErrors :: DynFlags -> DynFlags
setDeferTypeErrors dflag = gopt_set dflag G.Opt_DeferTypeErrors

setWarnTypedHoles :: DynFlags -> DynFlags
setWarnTypedHoles dflag = wopt_set dflag Opt_WarnTypedHoles

-- | Set the files as targets and load them.
setTargetFiles :: (GhcMonad m)  => [(FilePath, FilePath)] -> m ()
setTargetFiles files = do
    targets <- forM files guessTargetMapped
    pprTrace "setTargets" (vcat (map ppr files) $$ ppr targets) (return ())
    G.setTargets (map (\t -> t { G.targetAllowObjCode = False }) targets)
    void $ G.load LoadAllTargets

guessTargetMapped :: (GhcMonad m) => (FilePath, FilePath) -> m Target
guessTargetMapped (orig_file_name, mapped_file_name) = do
  t <- G.guessTarget orig_file_name Nothing
  return (setTargetFilename mapped_file_name t)

setTargetFilename :: FilePath -> Target -> Target
setTargetFilename fn t =
  t { targetId = case targetId t of
                  TargetFile _ p -> TargetFile fn p
                  tid -> tid }
