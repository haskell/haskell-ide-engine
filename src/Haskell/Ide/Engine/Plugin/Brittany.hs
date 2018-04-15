{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Brittany where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
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
import           System.FilePath (FilePath, takeDirectory)
import           Data.Maybe (maybeToList)


brittanyCmd :: Int -> Uri -> Maybe Range -> IdeGhcM (IdeResponse [J.TextEdit])
brittanyCmd tabSize uri range =
  pluginGetFile "brittanyCmd: " uri $ \file -> do
    confFile <- liftIO $ findLocalConfigPath (takeDirectory file)
    text <- GM.withMappedFile file $ liftIO . T.readFile
    case range of
      Just r -> do
        -- format selection
        res <- liftIO $ runBrittany tabSize confFile $ extractRange r text
        case res of
          Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ unlines (map showErr err)) Null)
          Right newText -> do
            let textEdit = J.TextEdit (normalize r) newText
            return $ IdeResponseOk [textEdit]
      Nothing -> do
        -- format document
        res <- liftIO $ runBrittany tabSize confFile text
        case res of
          Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ unlines (map showErr err)) Null)
          Right newText -> do
            let startPos = Position 0 0
                endPos = toPos (lastLine,1)
                {-
                In order to replace everything including newline characters,
                the end range should extend below the last line. From the specification:
                "If you want to specify a range that contains a line including
                the line ending character(s) then use an end position denoting
                the start of the next line"
                -}
                lastLine = (length $ T.lines text) + 1
                textEdit = J.TextEdit (Range startPos endPos) newText
            return $ IdeResponseOk [textEdit]

extractRange :: Range -> Text -> Text
extractRange (Range (Position sl _) (Position el _)) s = newS
  where focusLines = take (el-sl+1) $ drop sl $ T.lines s
        newS = T.unlines focusLines

normalize :: Range -> Range
normalize (Range (Position sl _) (Position el _)) =
  Range (Position sl 0) (Position el 10000)

runBrittany :: Int              -- ^ tab  size
            -> Maybe FilePath   -- ^ local config file
            -> Text             -- ^ text to format
            -> IO (Either [BrittanyError] Text)
runBrittany tabSize confPath text = do
  let cfg = mempty
              { _conf_layout =
                  mempty { _lconfig_indentAmount = opt (coerce tabSize)
                         }
              , _conf_forward =
                  (mempty :: CForwardOptions Option)
                    { _options_ghc = opt (runIdentity ( _options_ghc forwardOptionsSyntaxExtsEnabled))
                    }
              }

  config <- fromMaybeT (pure staticDefaultConfig) (readConfigsWithUserConfig cfg (maybeToList confPath))
  parsePrintModule config text

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT def act = runMaybeT act >>= maybe def return

opt :: a -> Option a
opt = Option . Just

showErr :: BrittanyError -> String
showErr (ErrorInput s)         = s
showErr (ErrorUnusedComment s) = s
showErr (LayoutWarning s)      = s
showErr (ErrorUnknownNode s _) = s
showErr ErrorOutputCheck       = "Brittany error - invalid output"

