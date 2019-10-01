-- | This module provides an API that software intented to be
-- integrated into HIE can use, so that they can make use of the
-- shared BIOS features.

{-
-- Stuff used in HaRe currently
Options(..)
defaultOptions
GmModuleGraph(..)
ModulePath(..)
GmComponent(..)
GmComponentType(..)

CachedInfo(..)
HasGhcModuleCache(..)
IdeGhcM

cabalModuleGraphs
filePathToUri
makeRevRedirMapFunc

MonadIO(..)
ifCachedModule
runIdeGhcMBare
setTypecheckedModule
-}


module Haskell.Ide.Engine.PluginApi
  (
  -- ** Re-exported from ghc-mod via ghc-project-types
    GP.GmModuleGraph(..)
  , GP.ModulePath(..)
  , GP.GmComponent(..)
  , GP.GmComponentType(..)

  -- * IDE monads
  , HIE.IdeState(..)
  , HIE.IdeGhcM
  , HIE.runIdeGhcM
  , runIdeGhcMBare
  , HIE.IdeM
  , HIE.runIdeM
  , HIE.IdeDeferM
  , HIE.MonadIde
  , HIE.iterT
  , HIE.LiftsToGhc(..)
  , HIE.HasGhcModuleCache(..)
  , HIE.cabalModuleGraphs
  , HIE.makeRevRedirMapFunc

  -- * Using the HIE module cache etc
  , HIE.setTypecheckedModule
  , HIE.Diagnostics
  , HIE.AdditionalErrs
  , LSP.filePathToUri
  , HIE.ifCachedModule
  , HIE.CachedInfo(..)

  -- * used for tests in HaRe
  , BiosLogLevel
  , BiosOptions
  , defaultOptions
  ) where



import qualified GhcProject.Types                    as GP
import qualified Haskell.Ide.Engine.Ghc              as HIE
import qualified Haskell.Ide.Engine.GhcModuleCache   as HIE (CachedInfo(..),HasGhcModuleCache(..))
import qualified Haskell.Ide.Engine.ModuleCache      as HIE (ifCachedModule)
import qualified Haskell.Ide.Engine.PluginsIdeMonads as HIE
import qualified Language.Haskell.LSP.Types          as LSP ( filePathToUri )
import qualified HIE.Bios.Types as HIE

defaultOptions :: HIE.CradleOpts
defaultOptions = HIE.defaultCradleOpts
type BiosLogLevel = HIE.BIOSVerbosity

type BiosOptions = HIE.CradleOpts
runIdeGhcMBare :: a
runIdeGhcMBare = error "Not implemented"
