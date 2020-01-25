{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Stylish where

import           Data.Aeson                    (Value (Null))
import qualified Data.Text                     as T
import           Haskell.Ide.Engine.MonadTypes

stylishDescriptor :: PluginId -> PluginDescriptor
stylishDescriptor plId = PluginDescriptor
  { pluginId                 = plId
  , pluginName               = "Stylish"
  , pluginDesc               = "Stylish is a tool to format source code."
  , pluginCommands           = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Just provider
  }

provider :: FormattingProvider
provider _contents _uri _typ _opts =
  return $ IdeResultFail (IdeError PluginError (T.pack "Formatting with Stylish is not currently supported.") Null)
