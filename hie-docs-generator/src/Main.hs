{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Control.Monad.Reader
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import           Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Examples
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Options as O
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           Text.Regex.Applicative.Text

main :: IO ()
main =
  do config <- execParser opts
     flip runReaderT config . runDocGenM $ generateDocs
  where opts =
          info (helper <*> O.config)
               (fullDesc <> progDesc "Generate documentation for hie" <>
                header "hie-docs-generator")

newtype DocGenM a =
  DocGenM {runDocGenM :: ReaderT O.Config IO a}
  deriving (Monad,MonadIO,MonadReader O.Config,Applicative,Functor)

generateDocs :: DocGenM ()
generateDocs =
  do indexPath <- pluginIndexPath
     indexPath' <- liftIO $ handlePrefix indexPath
     index <- pluginIndex
     prefix <- asks O.prefix
     prefix' <- liftIO $ handlePrefix prefix
     liftIO . createDirectoryIfMissing True $ prefix' </> "plugins"
     liftIO $ T.writeFile indexPath' index
     plugins <- asks O.plugins
     let pluginDocs = map pluginDoc $ M.toList plugins
     pluginDocPaths <- mapM (pluginDocPath) $ M.keys plugins
     pluginDocPaths' <- mapM (liftIO . handlePrefix) pluginDocPaths
     mapM_ (\(path,doc) -> liftIO $ T.writeFile path doc) $
       zip pluginDocPaths' pluginDocs
    where
      handlePrefix :: FilePath -> IO FilePath
      handlePrefix [] = return []
      handlePrefix ('~':xs) = (<> xs) <$> getHomeDirectory
      handlePrefix xs = return xs

pluginIndexPath :: MonadReader O.Config m => m FilePath
pluginIndexPath = reader $ \(O.Config _ prefix) -> prefix </> "plugins" </> "index.rst"

pluginIndex :: MonadReader O.Config m => m T.Text
pluginIndex =
  reader $
  \(O.Config plugins _) ->
    T.unlines (["Plugins","=======","", ".. toctree::","   :maxdepth: 2",""] ++
               map ("   " <>) (M.keys plugins))

pluginDocPath :: MonadReader O.Config m => PluginId -> m FilePath
pluginDocPath pluginId =
  reader $
  \(O.Config _ prefix) -> prefix </> "plugins" </> T.unpack pluginId <.> "rst"

pluginDoc :: (PluginId,UntaggedPluginDescriptor) -> T.Text
pluginDoc (pluginId,PluginDescriptor{pdCommands = commands}) =
  T.unlines ([pluginId
             ,T.replicate (T.length pluginId)
                          "="
             ,".. hie:plugin:: " <> pluginId
             ,""] ++
             commandDocs)
  where commandDocs = map (commandDoc pluginId) commands

commandDoc :: PluginId -> UntaggedCommand -> T.Text
commandDoc pluginId (cmdDesc -> cmddesc@(CommandDesc{cmdName = name,cmdUiDescription = desc})) =
  T.unlines $
  ["   .. hie:command:: " <> name
  ,""
  ,"      " <> cleanupDescription desc
  ,""
  ,"      Example::"
  ,""] ++
  map ("        " <>)
      (T.lines $
       T.decodeUtf8 $
       BS.toStrict $ AP.encodePretty (jsonStdioExample pluginId cmddesc))

cleanupDescription :: T.Text -> T.Text
cleanupDescription =
  -- Replace haddock code marks by rst code marks
  replace ((\t -> "``" <> T.pack t <> "``") <$> (sym '`' *> few anySym <* sym '\''))

ghciHelper :: IO ()
ghciHelper = flip runReaderT conf $ runDocGenM $ generateDocs
  where conf =
          O.Config O.pluginList "/home/moritz/code/haskell/haskell-ide-engine/docs/source"
