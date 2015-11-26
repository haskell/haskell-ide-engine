module PluginList
  ( plugins
  ) where

import Haskell.Ide.Engine.BasePlugin
import Haskell.Ide.ExamplePlugin2
import Haskell.Ide.GhcModPlugin
import Haskell.Ide.HaRePlugin

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
plugins :: Plugins
plugins = Map.fromList
  [
    -- Note: statically including known plugins. In future this map could be set
    -- up via a config file of some kind.
    ("eg2",    example2Descriptor)
  , ("ghcmod", ghcmodDescriptor)
  , ("hare",   hareDescriptor)
    -- The base plugin, able to answer questions about the IDE Engine environment.
  , ("base",   baseDescriptor)
  ]
