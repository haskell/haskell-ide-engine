{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Ide.Engine.Plugin.Ormolu ( ormoluDescriptor ) where

import Control.Monad.IO.Class ( liftIO, MonadIO(..) )
import Control.Exception
import Data.Aeson ( Value ( Null ) )
import qualified Data.Text as T
import Ormolu
import Ormolu.Config (defaultConfig)
import Ormolu.Exception (OrmoluException)
import Haskell.Ide.Engine.MonadTypes
import Haskell.Ide.Engine.PluginUtils

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
provider contents uri typ _opts =
  case typ of 
    -- // TODO Adequately throw an error on the following line
    FormatRange _ -> return $ IdeResultFail (IdeError PluginError (T.pack $  "Selection formatting for Ormolu is not currently supported.") Null)
    FormatText -> pluginGetFile contents uri $ \file -> do
        result <- liftIO $ try @OrmoluException (ormolu defaultConfig file (T.unpack contents))
        case result of
          Left  err -> return $ IdeResultFail (IdeError PluginError (T.pack $  "ormoluCmd: " ++ show err) Null)
          Right new -> return $ IdeResultOk [TextEdit (fullRange contents) new]
