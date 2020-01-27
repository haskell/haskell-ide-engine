{-# LANGUAGE OverloadedStrings #-}

-- | A flexible Haskell source code pretty printer.
module Haskell.Ide.Engine.Plugin.Floskell
  ( floskellDescriptor
  )
where

import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString.Lazy           as BS
import qualified Data.Text.Encoding             as T
import           Floskell
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils

floskellDescriptor :: PluginId -> PluginDescriptor
floskellDescriptor plId = PluginDescriptor
  { pluginId                 = plId
  , pluginName               = "Floskell"
  , pluginDesc               = "A flexible Haskell source code pretty printer."
  , pluginCommands           = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Just provider
  }

-- | Format provider of Floskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingProvider
provider contents uri typ _opts =
  pluginGetFile "Floskell: " uri $ \file -> do
    config <- liftIO $ findConfigOrDefault file
    let (range, selectedContents) = case typ of
          FormatText    -> (fullRange contents, contents)
          FormatRange r -> (r, extractRange r contents)
    case reformat config (Just file) $ BS.fromStrict $ T.encodeUtf8 selectedContents of
      Left  err -> ideErrorFrom PluginError "floskellCmd" err
      Right new -> return $ Right [TextEdit range $ T.decodeUtf8 $ BS.toStrict new]

-- | Find Floskell Config, user and system wide or provides a default style.
-- Every directory of the filepath will be searched to find a user configuration.
-- Also looks into places such as XDG_CONFIG_DIRECTORY<https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>.
-- This function may not throw an exception and returns a default config.
findConfigOrDefault :: FilePath -> IO AppConfig
findConfigOrDefault file = do
  mbConf <- findAppConfigIn file
  case mbConf of
    Just confFile -> readAppConfig confFile
    Nothing ->
      let gibiansky = head (filter (\s -> styleName s == "gibiansky") styles)
      in return $ defaultAppConfig { appStyle = gibiansky }
