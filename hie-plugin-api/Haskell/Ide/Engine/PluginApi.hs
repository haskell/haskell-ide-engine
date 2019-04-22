-- | This module provides an API that software intented to be
-- integrated into HIE can use, so that they can make use of the
-- shared BIOS features.

{-
-- Stuff used in HaRe currently
Options(..)
defaultOptions
mkRevRedirMapFunc
GmModuleGraph(..)
ModulePath(..)
GmComponent(..)
GmComponentType(..)

CachedInfo(..)
HasGhcModuleCache(..)
IdeGhcM

cabalModuleGraphs
filePathToUri

MonadIO(..)
ifCachedModule
runIdeGhcMBare
setTypecheckedModule
-}


module Haskell.Ide.Engine.PluginApi
  (
  -- ** Re-exported from ghc-mod
    GM.Options(..)
  , GM.defaultOptions
  , GM.mkRevRedirMapFunc
  , GM.GmModuleGraph(..)
  , GM.ModulePath(..)
  , GM.GmComponent(..)
  , GM.GmComponentType(..)

  -- * IDE monads
  , HIE.IdeState(..)
  , HIE.IdeGhcM
  , HIE.runIdeGhcM
  , HIE.runIdeGhcMBare
  , HIE.IdeM
  , HIE.runIdeM
  , HIE.IdeDeferM
  , HIE.MonadIde(..)
  , HIE.iterT
  , HIE.LiftsToGhc(..)
  , HIE.HasGhcModuleCache(..)
  , HIE.cabalModuleGraphs

  -- * Using the HIE module cache etc
  , HIE.setTypecheckedModule
  , HIE.Diagnostics
  , HIE.AdditionalErrs
  , LSP.filePathToUri
  , HIE.ifCachedModule
  , HIE.CachedInfo(..)

  -- * used for tests in HaRe
  , GM.globalArgSpec
  , GM.OutputOpts(..)
  , GM.GmLogLevel(..)
  , GM.OutputStyle(..)
  , GM.LineSeparator(..)
  ) where

import qualified GhcMod.Options.Options as GM (globalArgSpec)
import qualified GhcMod.Types          as GM (ModulePath(..),GmModuleGraph(..),GmComponent(..),GmComponentType(..),OutputOpts(..),GmLogLevel(..),OutputStyle(..),LineSeparator(..))
import qualified GhcMod.Utils          as GM (mkRevRedirMapFunc)
import qualified GhcModCore            as GM (Options(..),defaultOptions)
import qualified Haskell.Ide.Engine.Ghc              as HIE
import qualified Haskell.Ide.Engine.GhcModuleCache  as HIE (CachedInfo(..),HasGhcModuleCache(..))
import qualified Haskell.Ide.Engine.ModuleCache      as HIE (ifCachedModule)
import qualified Haskell.Ide.Engine.PluginsIdeMonads as HIE
import qualified Language.Haskell.LSP.Types          as LSP ( filePathToUri )
