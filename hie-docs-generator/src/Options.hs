{-# LANGUAGE OverloadedStrings #-}
module Options where

import qualified Data.Map as Map
import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.ExamplePluginAsync
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin
import           Options.Applicative

data Config =
  Config {plugins :: Plugins
         ,prefix :: FilePath}

config :: Parser Config
config =
  Config pluginList <$>
  strOption (long "prefix" <> metavar "PREFIX" <>
             help "directory where the docs are written to (defaults to current directory)")

pluginList :: Plugins
pluginList =
  Map.fromList
    [("applyrefact",untagPluginDescriptor applyRefactDescriptor)
    ,("eg2",untagPluginDescriptor example2Descriptor)
    ,("egasync",untagPluginDescriptor exampleAsyncDescriptor)
    ,("ghcmod",untagPluginDescriptor ghcmodDescriptor)
    ,("hare",untagPluginDescriptor hareDescriptor)
    ,("base",untagPluginDescriptor baseDescriptor)]
