{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Pragmas where

import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict           as H
import qualified Data.Text as T
import qualified GHC.Generics                  as Generics
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
-- import           Haskell.Ide.Engine.Plugin.HieExtras
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J

-- ---------------------------------------------------------------------

pragmasDescriptor :: PluginId -> PluginDescriptor
pragmasDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "Add Missing Pragmas"
  , pluginDesc = "Provide code actions to add missing pragmas when GHC suggests this"
  , pluginCommands =
      [ PluginCommand "addPragma" "add the given pragma" addPragmaCmd
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  }

-- ---------------------------------------------------------------------

data AddPragmaParams = AddPragmaParams
  { file   :: Uri
  , pragma :: T.Text
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

addPragmaCmd :: CommandFunc AddPragmaParams J.WorkspaceEdit
addPragmaCmd = CmdSync $ \_vf (AddPragmaParams uri pragmaName) -> do
  let
    pos = (J.Position 0 0)
    textEdits = J.List
      [J.TextEdit (J.Range pos pos)
                  ("{-# LANGUAGE " <> pragmaName <> " #-}\n")
      ]
    res = J.WorkspaceEdit
      (Just $ H.singleton uri textEdits)
      Nothing
  return $ IdeResultOk res

-- ---------------------------------------------------------------------

codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ _ _ (J.CodeActionContext (J.List diags) _monly) = do
  cmds <- mapM mkCommand pragmas
  return $ IdeResultOk cmds
  where
    ghcDiags = filter (\d -> d ^. J.source == Just "ghcmod") diags
    pragmas = concatMap (\d -> findPragma (d ^. J.message)) ghcDiags
    mkCommand pragmaName = do
      let
        codeAction :: J.Command -> J.CodeAction
        codeAction cmd = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing (Just cmd)
        title = "Add \"" <> pragmaName <> "\""
        cmdParams = [toJSON (AddPragmaParams (docId ^. J.uri) pragmaName )]
      cmd <- mkLspCommand plId "addPragma" title  (Just cmdParams)
      return $ codeAction cmd

-- ---------------------------------------------------------------------

findPragma :: T.Text -> [T.Text]
findPragma str = concatMap check possiblePragmas
  where
    check p =
      if T.isInfixOf p str
        then [p]
        else []

-- ---------------------------------------------------------------------

possiblePragmas :: [T.Text]
possiblePragmas =
  [
    "ConstraintKinds"
  , "DefaultSignatures"
  , "DeriveAnyClass"
  , "DeriveDataTypeable"
  , "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveLift"
  , "DeriveTraversable"
  , "DerivingStrategies"
  , "DerivingVia"
  , "EmptyCase"
  , "EmptyDataDeriving"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "GADTs"
  , "GHCForeignImportPrim"
  , "IncoherentInstances"
  , "KindSignatures"
  , "MultiParamTypeClasses"
  , "NamedFieldPuns"
  , "ParallelListComp"
  , "PatternSynonyms"
  , "Rank2Types"
  , "RankNTypes"
  , "RecordWildCards"
  , "RecursiveDo"
  , "RelaxedPolyRec"
  , "ScopedTypeVariables"
  , "StandaloneDeriving"
  , "StaticPointers"
  , "TransformListComp"
  , "TypeFamilies"
  , "TypeFamilyDependencies"
  , "TypeOperators"
  , "TypeSynonymInstances"
  , "UndecidableInstances"
  , "UndecidableSuperClasses"
  , "ViewPatterns"

  ]
