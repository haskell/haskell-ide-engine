{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Main where


import           Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
                          "=",""] ++ commandDocs)
  where commandDocs =  map (commandDoc pluginId) commands

commandDoc :: PluginId -> Command -> T.Text
commandDoc pluginId (cmdDesc -> (CommandDesc{cmdName = name,cmdUiDescription = desc})) =
  T.unlines [".. hie:command:: " <> pluginId <> ":" <> name
            ,""
            ,"   " <> desc
            ,""]
