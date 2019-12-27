{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Haskell.Ide.Engine.Plugin.Ormolu ( ormoluDescriptor ) where

import Control.Monad.IO.Class ( liftIO, MonadIO(..) )
import Control.Exception
import Data.Aeson ( Value ( Null ) )
import Data.Text
import Haskell.Ide.Engine.MonadTypes
import Haskell.Ide.Engine.PluginUtils

#if __GLASGOW_HASKELL__ >= 806
import Ormolu
import Ormolu.Config (defaultConfig)
import Ormolu.Exception (OrmoluException)
#else
import System.Log.Logger
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

#if __GLASGOW_HASKELL__ >= 806
provider :: FormattingProvider
provider contents uri typ _opts =
  case typ of 
    FormatRange _ -> return $ IdeResultFail (IdeError PluginError (pack "Selection formatting for Ormolu is not currently supported.") Null)
    FormatText -> pluginGetFile contents uri $ \file -> do
        result <- liftIO $ try @OrmoluException (ormolu defaultConfig file (unpack contents))
        case result of
          Left  err -> return $ IdeResultFail (IdeError PluginError (pack $  "ormoluCmd: " ++ show err) Null)
          Right new -> return $ IdeResultOk [TextEdit (fullRange contents) new]
#else
-- Work in progress
provider :: FormattingProvider
provider contents _uri typ _opts = do
  errorM "hie" "This version of HIE does not support Ormolu as a formatter"
  case typ of 
    FormatRange _ -> return $ IdeResultFail (IdeError PluginError (pack "Selection formatting for Ormolu is not currently supported.") Null)
    FormatText -> return $ IdeResultOk []
#endif