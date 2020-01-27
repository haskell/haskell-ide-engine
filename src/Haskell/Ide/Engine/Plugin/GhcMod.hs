{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
module Haskell.Ide.Engine.Plugin.GhcMod
  (
    ghcmodDescriptor

  -- * For tests
  -- , Bindings(..)
  -- , FunctionSig(..)
  -- , TypeDef(..)
  -- , TypeParams(..)
  -- , TypedHoles(..) -- only to keep the GHC 8.4 and below unused field warning happy
  -- , ValidSubstitutions(..)
  -- , extractHoleSubstitutions
  -- , extractMissingSignature
  -- , extractRenamableTerms
  -- , extractUnusedTerm
  -- , newTypeCmd
  -- , symbolProvider
  , splitCaseCmd
  ) where

import           Data.Aeson
#if __GLASGOW_HASKELL__ < 808
import           Data.Monoid ((<>))
#endif
import           GHC.Generics
import qualified Haskell.Ide.Engine.Ghc as HIE
import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Plugin.Generic as PG
import qualified Haskell.Ide.Engine.Support.HieExtras as Hie

-- ---------------------------------------------------------------------

ghcmodDescriptor :: PluginId -> PluginDescriptor
ghcmodDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "ghc-mod"
  , pluginDesc = "ghc-mod is a backend program to enrich Haskell programming "
              <> "in editors. It strives to offer most of the features one has come to expect "
              <> "from modern IDEs in any editor."
  , pluginCommands =
      [
        -- This one is used in the dispatcher tests, and is a wrapper around what we are already using anyway
        PluginCommand "check" "check a file for GHC warnings and errors" checkCmd

        -- PluginCommand "info" "Look up an identifier in the context of FILE (like ghci's `:info')" infoCmd
      , PluginCommand "type" "Get the type of the expression under (LINE,COL)" PG.typeCmd

        -- This one is registered in the vscode plugin, for some reason
      , PluginCommand "casesplit" "Generate a pattern match for a binding under (LINE,COL)" splitCaseCmd
      ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

-- checkCmd :: CommandFunc Uri (Diagnostics, AdditionalErrs)
-- checkCmd = CmdSync setTypecheckedModule

checkCmd :: Uri -> IdeGhcM (IdeResult (HIE.Diagnostics, HIE.AdditionalErrs))
checkCmd = HIE.setTypecheckedModule

-- ---------------------------------------------------------------------

splitCaseCmd :: Hie.HarePoint -> IdeGhcM (IdeResult WorkspaceEdit)
splitCaseCmd (Hie.HP _uri _pos)
  = ideError @String PluginError "splitCaseCmd not implemented"

-- ---------------------------------------------------------------------

customOptions :: Options
customOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2}

-- ---------------------------------------------------------------------

data TypeParams =
  TP { tpIncludeConstraints :: Bool
     , tpFile               :: Uri
     , tpPos                :: Position
     } deriving (Eq,Show,Generic)

instance FromJSON TypeParams where
  parseJSON = genericParseJSON customOptions
instance ToJSON TypeParams where
  toJSON = genericToJSON customOptions

-- -- ---------------------------------------------------------------------
