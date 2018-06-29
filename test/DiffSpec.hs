{-# LANGUAGE OverloadedStrings #-}
module DiffSpec where

import Haskell.Ide.Engine.PluginUtils
import Language.Haskell.LSP.Types
import Test.Hspec

dummyUri :: Uri
dummyUri = Uri "/foo/bar"

spec :: Spec
spec =
  describe "diffText" $
    it "generates deletions correctly" $
      let old = "hello\nworld\nfoo\nbar"
          new = "hello\nworld\nbar"
          (WorkspaceEdit _ (Just (List [TextDocumentEdit _ (List [e])]))) =
            diffText (dummyUri, old) new IncludeDeletions
        in e `shouldBe` TextEdit (Range (Position 2 0) (Position 3 0)) "" 