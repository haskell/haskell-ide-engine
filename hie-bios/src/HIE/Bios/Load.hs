{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module HIE.Bios.Load ( loadFile, setTargetFiles ) where

import CoreMonad (liftIO)
import DynFlags (gopt_set, wopt_set, WarningFlag(Opt_WarnTypedHoles))
import GHC
import qualified GHC as G
import HscTypes
import Outputable

import Data.IORef

import HIE.Bios.GHCApi
import System.Directory
import Hooks
import TcRnTypes (FrontendResult(..))
import Control.Monad (forM, void)
import GhcMonad
import HscMain
import Debug.Trace
import Data.List

#if __GLASGOW_HASKELL__ < 806
pprTraceM x s = pprTrace x s (return ())
#endif

-- | Obtaining type of a target expression. (GHCi's type:)
loadFile :: GhcMonad m
         => (FilePath, FilePath)     -- ^ A target file.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
loadFile file = do
  dir <- liftIO $ getCurrentDirectory
  pprTraceM "loadFile:2" (text dir)
  withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do

    df <- getSessionDynFlags
    pprTraceM "loadFile:3" (ppr $ optLevel df)
    (_, tcs) <- collectASTs (setTargetFiles [file])
    pprTraceM "loaded" (text (fst file) $$ text (snd file))
    let get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module
    traceShowM ("tms", (map get_fp tcs))
    let findMod [] = Nothing
        findMod (x:xs) = case get_fp x of
                           Just fp -> if fp `isSuffixOf` (snd file) then Just x else findMod xs
                           Nothing -> findMod xs
    return (findMod tcs, tcs)

{-
fileModSummary :: GhcMonad m => FilePath -> m ModSummary
fileModSummary file = do
    mss <- getModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms
    -}


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
  void $ setSessionDynFlags dflags1
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
