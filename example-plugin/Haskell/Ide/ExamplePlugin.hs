module Haskell.Ide.ExamplePlugin where

import Haskell.Ide.Engine.Types

plugin = defaultPlugin
    { initializeHook = Just (putStrLn "Hello from example plugin!")
    }
