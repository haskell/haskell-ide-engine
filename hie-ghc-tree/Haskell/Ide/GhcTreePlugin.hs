{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Haskell.Ide.GhcTreePlugin where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.GHC.DumpTree
import           Language.Haskell.GhcMod.Monad


ghcTreeDescriptor :: TaggedPluginDescriptor _
ghcTreeDescriptor = PluginDescriptor
  {
    pdUIShortName = "GhcTree"
  , pdUIOverview = "Provide GHC AST (parsed, renamed, typechecked)."
  , pdCommands =

        buildCommand trees (Proxy :: Proxy "trees") "Get ASTs for the given file"
                   [".hs",".lhs"] (SCtxFile :& RNil) RNil

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

trees :: CommandFunc AST
trees = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      trs <- runGmlT' [Left $ T.unpack fileName] (return . treeDumpFlags)$ treesForSession
      -- logm $ "getTrees:res=" ++ show trs
      case trs of
          [tree] -> return (IdeResponseOk $ treesToAST tree)
          _ -> return $ IdeResponseError (IdeError PluginError
                 "Expected one AST structure" (toJSON $ length trs))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcTreePlugin.getTrees: ghc’s exhaustiveness checker is broken" Null)

treesToAST :: Trees -> AST
treesToAST Trees{..} = AST (T.pack treeModule) (toJSON treeParsed)
  (toJSON treeRenamed) (toJSON treeTypechecked) (toJSON treeExports)
