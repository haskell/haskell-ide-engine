{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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
import           GHC.Generics
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.Brittany
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import           System.FilePath (FilePath, takeDirectory)
import           Data.Maybe (maybeToList)

data FormatParams = FormatParams Int Uri (Maybe Range)
     deriving (Eq, Show, Generic, FromJSON, ToJSON)

brittanyDescriptor :: PluginDescriptor
brittanyDescriptor = PluginDescriptor
  { pluginId       = "brittany"
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Just provider
  }

provider :: FormattingProvider
provider uri formatType opts = pluginGetFile "brittanyCmd: " uri $ \file -> do
  confFile <- liftIO $ getConfFile file
  mtext <- readVFS uri
  case mtext of
    Nothing -> return $ IdeResultFail (IdeError InternalError "File was not open" Null)
    Just text -> case formatType of
      FormatRange r -> do
        res <- liftIO $ runBrittany tabSize confFile $ extractRange r text
        case res of
          Left err -> return $ IdeResultFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ unlines (map showErr err)) Null)
          Right newText -> do
            let textEdit = J.TextEdit (normalize r) newText
            return $ IdeResultOk [textEdit]
      FormatDocument -> do
        res <- liftIO $ runBrittany tabSize confFile text
        case res of
          Left err -> return $ IdeResultFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ unlines (map showErr err)) Null)
          Right newText ->
            return $ IdeResultOk [J.TextEdit (fullRange text) newText]
  where tabSize = opts ^. J.tabSize

normalize :: Range -> Range
normalize (Range (Position sl _) (Position el _)) =
  -- Extend to the line below to replace newline character, as above
  Range (Position sl 0) (Position (el + 1) 0)

getConfFile :: FilePath -> IO (Maybe FilePath)
getConfFile = findLocalConfigPath . takeDirectory

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
showErr (ErrorInput s)          = s
showErr (ErrorMacroConfig  err input)
  = "Error: parse error in inline configuration: " ++ err ++ " in the string \"" ++ input ++ "\"."
showErr (ErrorUnusedComment s)  = s
showErr (LayoutWarning s)       = s
showErr (ErrorUnknownNode s _)  = s
showErr ErrorOutputCheck        = "Brittany error - invalid output"

