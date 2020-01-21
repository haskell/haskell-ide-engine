{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Haskell.Ide.Engine.Plugin.Ormolu ( ormoluDescriptor ) where

import Haskell.Ide.Engine.MonadTypes

#if __GLASGOW_HASKELL__ >= 806
import Control.Exception
import Control.Monad.IO.Class ( liftIO , MonadIO(..) )
import Data.Aeson ( Value ( Null ) )
import Data.Text
import Ormolu
import Ormolu.Config (defaultConfig)
import Ormolu.Exception (OrmoluException)
import Haskell.Ide.Engine.PluginUtils
#endif

ormoluDescriptor :: PluginId -> PluginDescriptor
ormoluDescriptor plId = PluginDescriptor
  { pluginId                 = plId
  , pluginName               = "Ormolu"
  , pluginDesc               = "A formatter for Haskell source code."
  , pluginCommands           = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Just provider
  }


provider :: FormattingProvider
provider _contents _uri _ _typ _opts =
#if __GLASGOW_HASKELL__ >= 806
  case _typ of 
    FormatRange _ -> return $ IdeResultFail (IdeError PluginError (pack "Selection formatting for Ormolu is not currently supported.") Null)
    FormatText -> pluginGetFile _contents _uri $ \file -> do
        result <- liftIO $ try @OrmoluException (ormolu defaultConfig file (unpack _contents))
        case result of
          Left  err -> return $ IdeResultFail (IdeError PluginError (pack $  "ormoluCmd: " ++ show err) Null)
          Right new -> return $ IdeResultOk [TextEdit (fullRange _contents) new]
#else
  return $ IdeResultOk [] -- NOP formatter
#endif