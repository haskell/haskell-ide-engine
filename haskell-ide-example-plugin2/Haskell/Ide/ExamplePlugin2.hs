module Haskell.Ide.ExamplePlugin2 where

import Haskell.Ide.Types

plugin = defaultPlugin
    { initializeHook = Just (putStrLn "Hello from example plugin 2!")
    }
