{-# LANGUAGE OverloadedStrings #-}

module Haskell.Ide.Engine.Plugin.Floskell
  ( floskellDescriptor
  )
where

import           Data.Aeson                     ( Value(Null) )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Floskell

floskellDescriptor :: PluginDescriptor
floskellDescriptor = PluginDescriptor
  { pluginId                 = "floskell"
  , pluginCommands           = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Just provider
  }

provider :: FormattingProvider
provider uri typ _opts = do
  mContents <- readVFS uri
  case mContents of
    Nothing -> return $ IdeResultFail (IdeError InternalError "File was not open" Null)
    Just contents ->
      let (range, selectedContents) = case typ of
            FormatDocument -> (fullRange contents, contents)
            FormatRange r  -> (r, extractRange r contents)
          result = reformat config (uriToFilePath uri) (T.encodeUtf8 selectedContents)
      in  case result of
            Left  err -> return $ IdeResultFail (IdeError PluginError (T.pack err) Null)
            Right new -> return $ IdeResultOk [TextEdit range (T.decodeUtf8 (BS.toStrict new))]
 where
  config    = defaultAppConfig { appStyle = gibiansky }
  -- seems to be the cmd line default
  gibiansky = head (filter (\s -> styleName s == "gibiansky") styles)
