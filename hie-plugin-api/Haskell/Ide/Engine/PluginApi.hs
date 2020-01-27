-- | This module provides an API that software intended to be
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
  -- * IDE monads
    HIE.IdeState(..)
  , HIE.IdeGhcM
  , HIE.runIdeGhcM
  , HIE.runActionWithContext
  , HIE.IdeM
  , HIE.runIdeM
  , HIE.IdeDeferM
  , HIE.MonadIde
  , HIE.iterT
  , HIE.LiftsToGhc(..)
  , HIE.HasGhcModuleCache(..)
  -- , HIE.cabalModuleGraphs
  , HIE.makeRevRedirMapFunc

  -- * Using the HIE module cache etc
  , HIE.setTypecheckedModule
  , HIE.Diagnostics
  , HIE.AdditionalErrs
  , LSP.filePathToUri
  , LSP.uriToFilePath
  , LSP.Uri
  , HIE.ifCachedModule
  , HIE.CachedInfo(..)
  , HIE.IdeResult

  -- * used for tests in HaRe
  , BiosLogLevel
  , BiosOptions
  , defaultOptions
  , HIE.BIOSVerbosity(..)
  , HIE.CradleOpts(..)
  , emptyIdePlugins
  , emptyIdeState
  ) where



import qualified Haskell.Ide.Engine.Ghc              as HIE
import qualified Haskell.Ide.Engine.GhcModuleCache   as HIE (CachedInfo(..),HasGhcModuleCache(..),emptyModuleCache)
import qualified Haskell.Ide.Engine.ModuleCache      as HIE (ifCachedModule,runActionWithContext )
import qualified Haskell.Ide.Engine.PluginsIdeMonads as HIE
import qualified Language.Haskell.LSP.Types          as LSP ( filePathToUri, uriToFilePath, Uri )
import qualified HIE.Bios.Types as HIE

defaultOptions :: HIE.CradleOpts
defaultOptions = HIE.defaultCradleOpts
type BiosLogLevel = HIE.BIOSVerbosity

type BiosOptions = HIE.CradleOpts

emptyIdePlugins :: HIE.IdePlugins
emptyIdePlugins = HIE.IdePlugins mempty

emptyIdeState :: HIE.IdeState
emptyIdeState = HIE.IdeState HIE.emptyModuleCache mempty mempty Nothing
