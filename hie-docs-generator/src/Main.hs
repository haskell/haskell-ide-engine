{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson.Encode.Pretty as AP
import           Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Examples
import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.ExamplePluginAsync
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin
import qualified Options as O
import           Options.Applicative
import           System.FilePath
import           Text.Regex.PCRE.Heavy

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
  do path <- pluginIndexPath
     index <- pluginIndex
     liftIO $ T.writeFile path index
     plugins <- asks O.plugins
     let pluginDocs = map pluginDoc $ M.toList plugins
     pluginDocPaths <- mapM pluginDocPath $ M.keys plugins
     mapM_ (\(path,doc) -> liftIO $ T.writeFile path doc) $ zip pluginDocPaths pluginDocs

pluginIndexPath :: MonadReader O.Config m => m FilePath
pluginIndexPath =
  reader $ \(O.Config _ prefix) -> prefix </> "plugins" </> "index.rst"

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

pluginDoc :: (PluginId,PluginDescriptor) -> T.Text
pluginDoc (pluginId,PluginDescriptor{pdCommands = commands}) =
  T.unlines ([pluginId
             ,T.replicate (T.length pluginId)
                          "="
             ,".. hie:plugin:: " <> pluginId
             ,""] ++
             commandDocs)
  where commandDocs = map (commandDoc pluginId) commands

commandDoc :: PluginId -> Command -> T.Text
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
  sub [re|`(.*?)'|]
      (\[code] -> "``" <> code <> "``" :: T.Text)

ghciHelper :: IO ()
ghciHelper = flip runReaderT conf $ runDocGenM $ generateDocs
  where conf =
          O.Config O.pluginList "/home/moritz/code/haskell/haskell-ide-engine/docs/source"
