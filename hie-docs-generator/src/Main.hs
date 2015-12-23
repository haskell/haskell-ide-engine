{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Map as Map
import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.ExamplePluginAsync
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin
import qualified Options as O
import           Options.Applicative

main :: IO ()
main =
  do config <- execParser opts
     putStrLn "Hello World"
  where opts =
          info (helper <*> O.config)
               (fullDesc <> progDesc "Generate documentation for hie" <>
                header "hie-docs-generator")

plugins :: Plugins
plugins = Map.fromList
  [
    ("applyrefact", applyRefactDescriptor)
  , ("eg2",         example2Descriptor)
  , ("egasync",     exampleAsyncDescriptor)
  , ("ghcmod",      ghcmodDescriptor)
  , ("hare",        hareDescriptor)
  , ("base",        baseDescriptor)
  ]

pluginIndex :: Plugins -> T.Text
pluginIndex plugins = T.unlines ["Plugins"
                                ,"======="]
