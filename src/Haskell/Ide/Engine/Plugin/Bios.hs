{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
module Haskell.Ide.Engine.Plugin.Bios(setTypecheckedModule, biosDescriptor) where

import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.GhcUtils
--import qualified Haskell.Ide.Engine.Plugin.HieExtras as Hie

import           DynFlags
import           GHC
import           IOEnv                             as G
import           HscTypes
import           Outputable hiding ((<>))
import qualified HscMain as G
import Haskell.Ide.Engine.Ghc

import System.Directory


-- ---------------------------------------------------------------------

biosDescriptor :: PluginId -> PluginDescriptor
biosDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "bios"
  , pluginDesc = "bios"
  , pluginCommands =
      [ PluginCommand "check" "check a file for GHC warnings and errors" checkCmd ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

checkCmd :: CommandFunc Uri (Diagnostics, AdditionalErrs)
checkCmd = CmdSync setTypecheckedModule

-- ---------------------------------------------------------------------



