{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module HIE.Bios.Load ( loadFile, setTargetFiles ) where

import CoreMonad (liftIO)
import DynFlags (gopt_set, wopt_set, WarningFlag(Opt_WarnTypedHoles))
import GHC
import qualified GHC as G
import qualified Exception as GE
import HscTypes
import Outputable

import Data.IORef

import HIE.Bios.Doc (getStyle)
import HIE.Bios.GHCApi
import HIE.Bios.Gap
import System.Directory
import EnumSet
import Hooks
import TcRnTypes (FrontendResult(..))
import Control.Monad (filterM, forM, void)
import GhcMonad
import HscMain

#if __GLASGOW_HASKELL__ < 806
pprTraceM x s = pprTrace x s (return ())
#endif

-- | Obtaining type of a target expression. (GHCi's type:)
loadFile :: GhcMonad m
         => (FilePath, FilePath)     -- ^ A target file.
         -> m (Maybe G.ParsedModule, Maybe TypecheckedModule)
loadFile file = do
  dir <- liftIO $ getCurrentDirectory
  pprTraceM "loadFile:2" (text dir)
  withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do

    df <- getSessionDynFlags
    pprTraceM "loadFile:3" (ppr $ optLevel df)
    (_, tcs) <- collectASTs (setTargetFiles [file])
    pprTraceM "loaded" (text (fst file) $$ text (snd file))
    case tcs of
      [] -> return (Nothing, Nothing)
      (tc:_) -> return (Nothing, Just tc)

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

collectASTs :: (GhcMonad m) => m a -> m (a, [TypecheckedModule])
collectASTs action = do
  dflags0 <- getSessionDynFlags
  ref1 <- liftIO $ newIORef []
  let dflags1 = dflags0 { hooks = (hooks dflags0)
                          { hscFrontendHook = Just (astHook ref1) } }
  setSessionDynFlags dflags1
  res <- action
  tcs <- liftIO $ readIORef ref1
  return (res, tcs)

astHook :: IORef [TypecheckedModule] -> ModSummary -> Hsc FrontendResult
astHook tc_ref ms = ghcInHsc $ do
  p <- G.parseModule ms
  tcm <- G.typecheckModule p
  let tcg_env = fst (tm_internals_ tcm)
  liftIO $ modifyIORef tc_ref (tcm :)
  return $ FrontendTypecheck tcg_env

ghcInHsc :: Ghc a -> Hsc a
ghcInHsc gm = do
  hsc_session <- getHscEnv
  session <- liftIO $ newIORef hsc_session
  liftIO $ reflectGhc gm (Session session)




guessTargetMapped :: (GhcMonad m) => (FilePath, FilePath) -> m Target
guessTargetMapped (orig_file_name, mapped_file_name) = do
  t <- G.guessTarget orig_file_name Nothing
  return (setTargetFilename mapped_file_name t)

setTargetFilename :: FilePath -> Target -> Target
setTargetFilename fn t =
  t { targetId = case targetId t of
                  TargetFile _ p -> TargetFile fn p
                  tid -> tid }
