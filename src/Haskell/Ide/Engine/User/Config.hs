{-# LANGUAGE LambdaCase #-}
module Haskell.Ide.Engine.User.Config
  ( HieConfigFile
  , Override(..)
  , hasUserOverrideRequest
  , getUserConfigFile
  )
where

import Control.Monad (join)
import Data.Functor ((<&>))
import qualified Data.Text as T (pack, unpack, toLower)
import qualified Data.Yaml as Yaml (decodeFileEither)
import qualified Data.Aeson as A (FromJSON(..), withObject)
import qualified Data.Aeson.Types as A (parseFieldMaybe)
import qualified System.Directory as SD (getCurrentDirectory, getHomeDirectory, doesFileExist)
import qualified Data.Maybe as Maybe (fromMaybe, mapMaybe)
import qualified Control.Exception as E (handle, IOException)



newtype HieConfigFile = HieConfigFile
  { requestOverrides :: [Override]
  } deriving (Show)

data Override
  = OnSaveOnly
  | NoAutoCompleteArguments
  deriving (Show, Eq)

emptyHieConfigFile :: HieConfigFile
emptyHieConfigFile = HieConfigFile
  { requestOverrides = []
  }

hasUserOverrideRequest :: Override -> HieConfigFile -> Bool
hasUserOverrideRequest x = elem x . requestOverrides

getUserConfigFile :: Maybe FilePath -> IO HieConfigFile
getUserConfigFile root = E.handle onIOException go
  where
    onIOException :: E.IOException -> IO HieConfigFile
    onIOException _ = return emptyHieConfigFile
    
    parse :: FilePath -> IO HieConfigFile
    parse filePath = Yaml.decodeFileEither filePath <&> \case
      Left _ -> emptyHieConfigFile
      Right x -> x
    
    go :: IO HieConfigFile
    go = do
      suggested <- join <$> mapM checkForConfigFile root
      local <- checkForConfigFile =<< SD.getCurrentDirectory
      home <- checkForConfigFile =<< SD.getHomeDirectory
      case (suggested, local, home) of
        (Just filePath, _, _) -> parse filePath
        (_, Just filePath, _) -> parse filePath
        (_, _, Just filePath) -> parse filePath
        _ -> return emptyHieConfigFile

checkForConfigFile :: FilePath -> IO (Maybe FilePath)
checkForConfigFile root = SD.doesFileExist hieFilePath <&> \case
  True -> Just hieFilePath
  _ -> Nothing
  where
    hieFilePath = appendHieFileName root

appendHieFileName :: FilePath -> FilePath
appendHieFileName root = root <> "/hie.yaml"

instance A.FromJSON HieConfigFile where
  parseJSON = A.withObject "config file" $ \o -> do
    overrides <- A.parseFieldMaybe o (T.pack "overrides")
      <&> Maybe.fromMaybe []
      <&> Maybe.mapMaybe (f . T.unpack . T.toLower)
    return $ HieConfigFile {requestOverrides = overrides}
    where
      f :: String -> Maybe Override
      f = \case
        "on_save_only" -> Just OnSaveOnly
        "no_autocomplete_arguments" -> Just NoAutoCompleteArguments
        _ -> Nothing

