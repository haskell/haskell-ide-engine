{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Simple example plugin showing how easy it is to make a plugin, using the operations from
-- http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html
module Haskell.Ide.Engine.Plugin.HfaAlign where

import           Control.Lens
-- import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.HashMap.Strict           as H
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
-- import qualified Data.Map                      as Map
-- import qualified Data.Set                      as S
-- import qualified Data.Text                     as T
import qualified GHC.Generics                  as Generics
-- import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J

-- blog post imports
import Data.Text (Text)

import qualified Data.Text
-- import qualified Data.Text.IO
import qualified Safe

-- ---------------------------------------------------------------------

example2Descriptor :: PluginId -> PluginDescriptor
example2Descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "Align Equals"
  , pluginDesc = "An example of writing an HIE plugin\nbased on http://www.haskellforall.com/2018/10/detailed-walkthrough-for-beginner.html"
  , pluginCommands =
      [ PluginCommand "align" "Align = in active range" alignCmd
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  }

-- ---------------------------------------------------------------------

data AlignParams = AlignParams
  { file  :: Uri
  , range :: J.Range
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

alignCmd :: CommandFunc AlignParams J.WorkspaceEdit
alignCmd = CmdSync $ \_ (AlignParams uri r) -> return $ IdeResultOk $ makeAlign uri r

makeAlign :: J.Uri -> J.Range -> J.WorkspaceEdit
makeAlign uri (J.Range (J.Position startLine _) _) = res
  where
    pos = (J.Position startLine 0)
    textEdits = J.List
      [J.TextEdit (J.Range pos pos)
                  "-- TODO: from example2 plugin\n"
      ]
    res = J.WorkspaceEdit
      (Just $ H.singleton uri textEdits)
      Nothing

-- ---------------------------------------------------------------------


codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ _ r _context = do
  cmd <- mkLspCommand plId "todo" title  (Just cmdParams)
  return $ IdeResultOk [codeAction cmd]
  where
    codeAction :: J.Command -> J.CodeAction
    codeAction cmd = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing (Just cmd)
    title = "Add TODO marker"
    cmdParams = [toJSON (AlignParams (docId ^. J.uri) r )]

-- ---------------------------------------------------------------------
-- Blog post code
-- ---------------------------------------------------------------------

prefixLength :: Text -> Int
prefixLength line = Data.Text.length prefix
  where
    (prefix, _suffix) = Data.Text.breakOn "=" line

adjustLine :: Int -> Text -> Text
adjustLine desiredPrefixLength oldLine = newLine
  where
    (prefix, suffix) = Data.Text.breakOn "=" oldLine

    actualPrefixLength = Data.Text.length prefix

    additionalSpaces = desiredPrefixLength - actualPrefixLength

    spaces = Data.Text.replicate additionalSpaces " "

    newLine = Data.Text.concat [ prefix, spaces, suffix ]

adjustText :: Text -> Text
adjustText oldText = newText
  where
    oldLines = Data.Text.lines oldText

    prefixLengths = map prefixLength oldLines

    newLines =
        case Safe.maximumMay prefixLengths of
            Nothing ->
                []
            Just desiredPrefixLength ->
                map (adjustLine desiredPrefixLength) oldLines

    newText = Data.Text.unlines newLines

-- main :: IO ()
-- main = Data.Text.IO.interact adjustText
