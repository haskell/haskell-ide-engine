module Haskell.Ide.ExamplePlugin where

import Haskell.Ide.Types

plugin = defaultPlugin
    { initializeHook = Just (putStrLn "Hello from example plugin!")
    }
