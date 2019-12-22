{-# LANGUAGE OverloadedStrings   #-}
module Haskell.Ide.Engine.Plugin.Bios
  ( setTypecheckedModule
  , biosDescriptor
  )
where

import           Haskell.Ide.Engine.MonadTypes

import           Haskell.Ide.Engine.Ghc


-- ---------------------------------------------------------------------

biosDescriptor :: PluginId -> PluginDescriptor
biosDescriptor plId = PluginDescriptor
  { pluginId                 = plId
  , pluginName               = "bios"
  , pluginDesc               = "bios"
  , pluginCommands           =
    [PluginCommand "check" "check a file for GHC warnings and errors" checkCmd]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Nothing
  }

checkCmd :: Uri -> IdeGhcM (IdeResult (Diagnostics, AdditionalErrs))
checkCmd = setTypecheckedModule

-- ---------------------------------------------------------------------
