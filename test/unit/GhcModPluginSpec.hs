{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

import           Control.Exception
import qualified Data.HashMap.Strict                 as H
import qualified Data.Map                            as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Set                            as S
import qualified Data.Text                           as T
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Support.HieExtras
import           Language.Haskell.LSP.Types          (TextEdit (..))
import           System.Directory
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------
