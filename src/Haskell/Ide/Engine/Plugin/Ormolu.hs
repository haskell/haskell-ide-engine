{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Haskell.Ide.Engine.Plugin.Ormolu ( ormoluDescriptor ) where

import Haskell.Ide.Engine.MonadTypes

#if __GLASGOW_HASKELL__ >= 806
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class ( liftIO , MonadIO(..) )
import Data.Aeson ( Value ( Null ) )
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Ormolu
import Haskell.Ide.Engine.PluginUtils
import HIE.Bios.Types
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
provider _contents _uri _typ _opts =
#if __GLASGOW_HASKELL__ >= 806
  case _typ of
    FormatRange _ -> return $ IdeResultFail (IdeError PluginError (T.pack "Selection formatting for Ormolu is not currently supported.") Null)
    FormatText -> pluginGetFile _contents _uri $ \file -> do
        opts <- lookupComponentOptions file
        let opts' = map DynOption $ filter exop $ join $ maybeToList $ componentOptions <$> opts
            conf  = Config opts' False False True False
        result <- liftIO $ try @OrmoluException (ormolu conf file (T.unpack _contents))

        case result of
          Left  err -> return $ IdeResultFail (IdeError PluginError (T.pack $ "ormoluCmd: " ++ show err) Null)
          Right new -> return $ IdeResultOk [TextEdit (fullRange _contents) new]
  where
    exop s =
      "-X" `isPrefixOf` s
      || "-fplugin=" `isPrefixOf` s
      || "-pgmF=" `isPrefixOf` s
#else
  return $ IdeResultOk [] -- NOP formatter
#endif
