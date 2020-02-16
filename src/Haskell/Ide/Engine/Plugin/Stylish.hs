{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Stylish where

import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (Value (Null))
import           Data.List                      (intercalate)
import qualified Data.Text                      as T
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils (fullRange, pluginGetFile)
import           Language.Haskell.Stylish       (ConfigPath (..), format)


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
provider contents uri typ _ =
  case typ of
    FormatRange _ ->
      return $ IdeResultFail (IdeError PluginError (T.pack "Selection formatting for Stylish is not currently supported.") Null)
    FormatText -> pluginGetFile "stylish:" uri $ \file -> do
      res <- liftIO $ runStylish Nothing file contents
      case res of
        Left err  -> return $ IdeResultFail
          (IdeError PluginError
                  (T.pack $ "stylish: " ++ err)
                  Null
          )
        Right new -> return $ IdeResultOk [TextEdit (fullRange contents) (T.pack $ ((intercalate "\n" new) <> "\n"))]


runStylish :: Maybe ConfigPath -> FilePath -> T.Text -> IO (Either String [String])
runStylish config file contents = format config (Just file) (T.unpack contents)
