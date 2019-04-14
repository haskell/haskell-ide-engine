-- | This module provides an API that software intented to be
-- integrated into HIE can use, so that they can make use of the
-- shared BIOS features.

module Haskell.Ide.Engine.PluginApi
  (
  -- ** Re-exported from ghc-mod
    GM.Options(..)
  , GM.defaultOptions
  , GM.getModulesGhc'
  , GM.mkRevRedirMapFunc
  , GM.cradle
  , GM.Cradle(..)

  , GM.GmModuleGraph(..)
  , GM.ModulePath(..)
  , GM.GmComponent(..)
  , GM.GmComponentType(..)
  , GM.cabalResolvedComponents

  -- * IDE monads
  , HIE.IdeState(..)
  , HIE.IdeGhcM
  , HIE.runIdeGhcM
  , HIE.IdeM
  , HIE.runIdeM
  , HIE.IdeDeferM
  , HIE.MonadIde(..)
  , HIE.iterT
  , HIE.LiftsToGhc(..)


  -- probably remove the next ones

  , GM.IOish
  , GM.MonadIO(..)
  , GM.GmOut(..)
  , GM.GhcModT
  , GM.runGhcModT
  , GM.GmlT(..)
  , GM.GmEnv(..)
  , GM.gmlGetSession
  , GM.gmlSetSession
  , GM.globalArgSpec
  , GM.OutputOpts(..)
  , GM.GmLogLevel(..)
  , GM.OutputStyle(..)
  , GM.LineSeparator(..)
  ) where

import qualified GhcMod.Monad.Newtypes as GM (GmlT(..))
import qualified GhcMod.Monad.Out      as GM (GmOut(..))
import qualified GhcMod.Monad.Types    as GM (GmEnv(..),IOish,gmlGetSession,gmlSetSession,cradle)
import qualified GhcMod.Target         as GM (cabalResolvedComponents)
import qualified GhcMod.Types          as GM (ModulePath(..),GmModuleGraph(..),GmComponent(..),GmComponentType(..),Cradle(..),MonadIO(..),OutputOpts(..),GmLogLevel(..),OutputStyle(..),LineSeparator(..))
import qualified GhcMod.Utils          as GM (mkRevRedirMapFunc)
import qualified GhcModCore            as GM (Options(..),defaultOptions,getModulesGhc',GhcModT,runGhcModT)
import qualified GhcMod.Options.Options as GM (globalArgSpec)

import Haskell.Ide.Engine.PluginsIdeMonads as HIE
