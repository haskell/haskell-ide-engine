module HIE.Bios.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  ) where

import DynFlags (dopt_set, DumpFlag(Opt_D_dump_splices))
import GHC (Ghc, DynFlags(..), GhcMonad)

import HIE.Bios.GHCApi
import HIE.Bios.Logger
import HIE.Bios.Types
import Outputable

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: Options
            -> Cradle
            -> [FilePath]  -- ^ The target files.
            -> IO String
checkSyntax _   _      []    = return ""
checkSyntax opt cradle files = withGhcT $ do
    pprTrace "cradble" (text $ show cradle) (return ())
    initializeFlagsWithCradle (head files) cradle
    either id id <$> check opt files
  where
    {-
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"
      -}

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: (GhcMonad m)
      => Options
      -> [FilePath]  -- ^ The target files.
      -> m (Either String String)
check opt fileNames = withLogger opt setAllWaringFlags $
    setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: Options
               -> Cradle
               -> [FilePath]  -- ^ The target files.
               -> IO String
expandTemplate _   _      []    = return ""
expandTemplate opt cradle files = withGHC sessionName $ do
    initializeFlagsWithCradle (head files) cradle
    either id id <$> expand opt files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc (Either String String)
expand opt fileNames = withLogger opt (setDumpSplices . setNoWaringFlags) $
    setTargetFiles fileNames

setDumpSplices :: DynFlags -> DynFlags
setDumpSplices dflag = dopt_set dflag Opt_D_dump_splices
