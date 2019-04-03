{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module HIE.Bios.GHCApi (
    withGHC
  , withGHC'
  , withGhcT
  , initializeFlagsWithCradle
  , getDynamicFlags
  , getSystemLibDir
  , withDynFlags
  , withCmdFlags
  , setNoWaringFlags
  , setAllWaringFlags
  , CradleError(..)
  ) where

import CoreMonad (liftIO)
import Exception (ghandle, SomeException(..), ExceptionMonad(..), throwIO, Exception(..))
import GHC (Ghc, DynFlags(..), GhcLink(..), HscTarget(..), LoadHowMuch(..), GhcMonad, GhcT)
import qualified GHC as G
import qualified Outputable as G
import qualified MonadUtils as G
import DynFlags
import DriverPhases

import Control.Monad (forM, void, when)
import System.Exit (exitSuccess, ExitCode(..))
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

import System.Directory

import qualified HIE.Bios.Gap as Gap
import HIE.Bios.Types
import Debug.Trace
import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
getSystemLibDir :: IO (Maybe FilePath)
getSystemLibDir = do
    res <- readProcess "ghc" ["--print-libdir"] []
    return $ case res of
        ""   -> Nothing
        dirn -> Just (init dirn)

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC file body = ghandle ignore $ withGHC' body
  where
    ignore :: SomeException -> IO a
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

withGHC' :: Ghc a -> IO a
withGHC' body = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir body

withGhcT :: (Exception.ExceptionMonad m, G.MonadIO m, Monad m) => GhcT m a -> m a
withGhcT body = do
  mlibdir <- G.liftIO $ getSystemLibDir
  G.runGhcT mlibdir body

----------------------------------------------------------------

data Build = CabalPkg | SingleFile deriving Eq

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle ::
        (GhcMonad m)
        => FilePath -- The file we are loading it because of
        -> Cradle
        -> m ()
initializeFlagsWithCradle fp cradle = do
      (ex, err, ghcOpts) <- liftIO $ getOptions (cradleOptsProg cradle) fp
      G.pprTrace "res" (G.text (show (ex, err, ghcOpts, fp))) (return ())
      case ex of
        ExitFailure _ -> throwCradleError err
        _ -> return ()
      let compOpts = CompilerOptions ghcOpts
      liftIO $ hPrint stderr ghcOpts
      initSession SingleFile compOpts

data CradleError = CradleError String deriving (Show)

instance Exception CradleError where

throwCradleError :: GhcMonad m => String -> m ()
throwCradleError = liftIO . throwIO . CradleError

----------------------------------------------------------------
cacheDir = "haskell-ide-engine"

clearInterfaceCache :: FilePath -> IO ()
clearInterfaceCache fp = do
  cd <- getCacheDir fp
  res <- doesPathExist cd
  when res (removeDirectoryRecursive cd)

getCacheDir :: FilePath -> IO FilePath
getCacheDir fp = getXdgDirectory XdgCache (cacheDir ++ "/" ++ fp)

initSession :: (GhcMonad m)
            => Build
            -> CompilerOptions
            -> m ()
initSession _build CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    traceShowM (length ghcOptions)

    let opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack ghcOptions)
    fp <- liftIO $ getCacheDir opts_hash
    -- For now, clear the cache initially rather than persist it across
    -- sessions
    liftIO $ clearInterfaceCache opts_hash
    df' <- addCmdOpts ghcOptions df
    void $ G.setSessionDynFlags
      (disableOptimisation
      $ setIgnoreInterfacePragmas
      $ resetPackageDb
      $ ignorePackageEnv
      $ writeInterfaceFiles (Just fp)
      $ setLinkerOptions df'
      )

----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

resetPackageDb :: DynFlags -> DynFlags
resetPackageDb df = df { pkgDatabase = Nothing }

ignorePackageEnv :: DynFlags -> DynFlags
ignorePackageEnv df = df { packageEnv = Just "-" }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
 gopt_set df Opt_IgnoreInterfacePragmas

writeInterfaceFiles :: Maybe FilePath -> DynFlags -> DynFlags
writeInterfaceFiles Nothing df = df
writeInterfaceFiles (Just hi_dir) df = setHiDir hi_dir (gopt_set df Opt_WriteInterface)

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d = d { hiDir      = Just f}


addCmdOpts :: (GhcMonad m)
           => [String] -> DynFlags -> m DynFlags
addCmdOpts cmdOpts df1 = do
    (df2, leftovers, warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
    traceShowM (map G.unLoc leftovers, length warns)
    -- TODO: Need to handle these as well
    -- Ideally it requires refactoring to work in GHCi monad rather than
    -- Ghc monad and then can just use newDynFlags.
    {-
    liftIO $ G.handleFlagWarnings idflags1 warns
    when (not $ null leftovers)
        (throwGhcException . CmdLineError
         $ "Some flags have not been recognized: "
         ++ (concat . intersperse ", " $ map unLoc leftovers))
    when (interactive_only && packageFlagsChanged idflags1 idflags0) $ do
       liftIO $ hPutStrLn stderr "cannot set package flags with :seti; use :set"
    -}
    return df2

----------------------------------------------------------------


----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir G.getSessionDynFlags

withDynFlags ::
  (GhcMonad m)
  => (DynFlags -> DynFlags) -> m a -> m a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

withCmdFlags ::
  (GhcMonad m)
  => [String] -> m a ->  m a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflag
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-w:".
setNoWaringFlags :: DynFlags -> DynFlags
setNoWaringFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWaringFlags :: DynFlags -> DynFlags
setAllWaringFlags df = df { warningFlags = allWarningFlags }

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

{-# NOINLINE allWarningFlags #-}
allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    mlibdir <- getSystemLibDir
    G.runGhcT mlibdir $ do
        df <- G.getSessionDynFlags
        df' <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'
