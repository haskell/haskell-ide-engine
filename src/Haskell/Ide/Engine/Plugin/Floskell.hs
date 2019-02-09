{-# LANGUAGE OverloadedStrings #-}

module Haskell.Ide.Engine.Plugin.Floskell
  ( floskellDescriptor
  )
where

import           Data.Aeson                     ( Value(Null) )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import qualified GhcMod.Utils                  as GM
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           FloskellFloskell
import           Control.Monad.IO.Class

floskellDescriptor :: PluginId -> PluginDescriptor
floskellDescriptor plId = PluginDescriptor
  { pluginId                 = plId
  , pluginName               = "Floskell"
  , pluginDesc               = "Floskell is a flexible Haskell source code pretty printer."
  , pluginCommands           = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Just provider
  }

provider :: FormattingProvider
provider uri typ _opts = pluginGetFile "floskell: " uri $ \file -> do
  contents <- GM.withMappedFile file (liftIO . T.readFile)
  let (range, selectedContents) = case typ of
        FormatDocument -> (fullRange contents, contents)
        FormatRange r  -> (r, extractRange r contents)
      result = reformat defaultAppConfig (uriToFilePath uri) (T.encodeUtf8 selectedContents)
  case result of
    Left  err -> return $ IdeResultFail (IdeError PluginError (T.pack err) Null)
    Right new -> return $ IdeResultOk [TextEdit range (T.decodeUtf8 (BS.toStrict new))]
