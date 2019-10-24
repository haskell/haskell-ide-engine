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
    FormatRange r -> return $ IdeResultFail (IdeError PluginError (T.pack $  "ormoluCmd: " ++ show r) Null)
    FormatText -> pluginGetFile contents uri $ \file -> do
        -- INFO: Selection Formatting currently not supported, the below comment is just for temporal information
        -- let (range, selectedContents) = case typ of
        --       FormatText    -> (fullRange contents, contents)
        --       FormatRange r -> (r, extractRange r contents)
        result <- liftIO $ try @OrmoluException (ormolu defaultConfig file (T.unpack contents))
        case result of
          Left  err -> return $ IdeResultFail (IdeError PluginError (T.pack $  "ormoluCmd: " ++ show err) Null)
          Right new -> return $ IdeResultOk [TextEdit (fullRange contents) new]