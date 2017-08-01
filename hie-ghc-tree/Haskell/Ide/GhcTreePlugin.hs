{-# LANGUAGE OverloadedStrings #-}
-- | ghc-dump-tree library plugin
module Haskell.Ide.GhcTreePlugin where

import           Data.Aeson
import           GhcMod.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.GHC.DumpTree

-- ---------------------------------------------------------------------

-- | Descriptor for the ghc-tree plugin
ghcTreeDescriptor :: PluginDescriptor
ghcTreeDescriptor = PluginDescriptor
  {
    pluginName = "GhcTree"
  , pluginDesc = "Provide GHC AST (parsed, renamed, typechecked)."
  , pluginCommands =
      [PluginCommand "trees" "Get ASTs for the given file" trees]
  }

-- ---------------------------------------------------------------------

-- | Get the AST for the given file
trees :: CommandFunc Uri Trees
trees = CmdSync treesCmd

treesCmd :: Uri -> IdeM (IdeResponse Trees)
treesCmd uri =
  pluginGetFile "trees: " uri $ \file -> do
      trs <- runGmlT' [Left file] (return . treeDumpFlags) $ treesForTargets [file]
      case trs of
          [tree] -> return (IdeResponseOk tree)
          _ -> return $ IdeResponseError (IdeError PluginError
                 "Expected one AST structure" (toJSON $ length trs))
