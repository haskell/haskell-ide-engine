{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
-- | ghc-dump-tree library plugin
module Haskell.Ide.GhcTreePlugin where

import           Data.Aeson
import qualified Data.Text as T
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.GHC.DumpTree
import           GhcMod.Monad

-- ---------------------------------------------------------------------

-- | Descriptor for the ghc-tree plugin
ghcTreeDescriptor :: TaggedPluginDescriptor _
ghcTreeDescriptor = PluginDescriptor
  {
    pdUIShortName = "GhcTree"
  , pdUIOverview = "Provide GHC AST (parsed, renamed, typechecked)."
  , pdCommands =

        buildCommand trees (Proxy :: Proxy "trees") "Get ASTs for the given file"
                   [".hs",".lhs"] (SCtxFile :& RNil) RNil SaveAll

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

-- | Get the AST for the given file
trees :: CommandFunc AST
trees = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& RNil) ->
      treesCmd uri

treesCmd :: Uri -> IdeM (IdeResponse AST)
treesCmd uri =
  pluginGetFile "trees: " uri $ \file -> do
      trs <- runGmlT' [Left file] (return . treeDumpFlags) $ treesForTargets [file]
      case trs of
          [tree] -> return (IdeResponseOk $ treesToAST tree)
          _ -> return $ IdeResponseError (IdeError PluginError
                 "Expected one AST structure" (toJSON $ length trs))

-- | Convert from ghc-dump-tree type to our own type
-- (avoids dependency on ghc-dump-tree from hie-base)
treesToAST :: Trees -> AST
treesToAST Trees{..} = AST (T.pack treeModule) (toJSON treeParsed)
  (toJSON treeRenamed) (toJSON treeTypechecked) (toJSON treeExports)
