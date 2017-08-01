{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.BrittanyPlugin where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Coerce
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import qualified GhcMod.Utils                          as GM
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.Brittany
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J


brittanyCmd :: Int -> TextDocumentIdentifier -> Maybe Range -> IdeM (IdeResponse [J.TextEdit])
brittanyCmd tabSize tdi range =
  pluginGetFile "brittanyCmd: " (tdi ^. J.uri) $ \file ->
    case range of
      Just r -> do
        text <- GM.withMappedFile file $ liftIO . T.readFile
        res <- liftIO $ runBrittany tabSize $ extractRange r text
        case res of
          Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ unlines (map showErr err)) Null)
          Right newText -> do
            let textEdit = J.TextEdit (normalize r) newText
            return $ IdeResponseOk [textEdit]
      Nothing -> do
        text <- GM.withMappedFile file $ liftIO . T.readFile
        res <- liftIO $ runBrittany tabSize text
        case res of
          Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ unlines (map showErr err)) Null)
          Right newText -> do
            let startPos = Position 0 0
                endPos = toPos (l,c+1)
                l = length textLines
                c = T.length $ last textLines
                textLines = T.lines text
                textEdit = J.TextEdit (Range startPos endPos) newText
            return $ IdeResponseOk [textEdit]

extractRange :: Range -> Text -> Text
extractRange (Range (Position sl _) (Position el _)) s = newS
  where focusLines = take (el-sl+1) $ drop sl $ T.lines s
        newS = T.unlines focusLines

normalize :: Range -> Range
normalize (Range (Position sl _) (Position el _)) =
  Range (Position sl 0) (Position el 10000)

runBrittany :: Int -> Text -> IO (Either [BrittanyError] Text)
runBrittany tabSize text = do
  let
    config' = staticDefaultConfig
    config  = config'
      { _conf_layout = (_conf_layout config') { _lconfig_indentAmount = coerce
                                                tabSize
                                              }
      , _conf_forward = _conf_forward config' <> forwardOptionsSyntaxExtsEnabled
      }
  parsePrintModule config text

showErr :: BrittanyError -> String
showErr (ErrorInput s)         = s
showErr (ErrorUnusedComment s) = s
showErr (LayoutWarning s)      = s
showErr (ErrorUnknownNode s _) = s
showErr ErrorOutputCheck       = "Brittany error - invalid output"

