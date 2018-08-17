{-# LANGUAGE OverloadedStrings #-}
module CodeActionsSpec where

import Test.Hspec
import qualified Data.Text.IO as T
import Haskell.Ide.Engine.Plugin.HsImport
import Haskell.Ide.Engine.Plugin.GhcMod
import Haskell.Ide.Engine.Plugin.Package

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "import code actions" $ do
    it "pick up variable not in scope" $
      let msg =  "Variable not in scope: fromJust :: Maybe Integer -> t"
        in extractImportableTerm msg `shouldBe` Just "fromJust :: Maybe Integer -> t"
    it "pick up variable not in scope with 'perhaps you meant'" $
      let msg =  "• Variable not in scope: msgs :: T.Text\n• Perhaps you meant ‘msg’ (line 90)"
        in extractImportableTerm msg `shouldBe` Just "msgs :: T.Text"
    it "pick up multi-line variable not in scope" $
      let msg = "Variable not in scope:\nliftIO\n:: IO [FilePath]\n-> GhcMod.Monad.Newtypes.GmT\n                (GhcMod.Monad.Newtypes.GmOutT IdeM) [[t0]]"
        in extractImportableTerm msg `shouldBe` Just "liftIO :: IO [FilePath] -> GhcMod.Monad.Newtypes.GmT (GhcMod.Monad.Newtypes.GmOutT IdeM) [[t0]]"
    it "pick up when" $
      let msg = "Variable not in scope: when :: Bool -> IO () -> t"
        in extractImportableTerm msg `shouldBe` Just "when :: Bool -> IO () -> t"

  describe "rename code actions" $ do
    it "pick up variable not in scope perhaps you meant" $
      let msg = "• Variable not in scope: fromBust\n• Perhaps you meant ‘fromJust’ (imported from Data.Maybe)"
        in extractRenamableTerms msg `shouldBe` ["fromJust"]
    it "pick up multiline perhaps you meant" $
      let msg = "• Variable not in scope: fromBust :: [Char] -> t\n• Perhaps you meant one of these:\n‘fromJust’ (imported from Data.Maybe),\n‘BM.fromList’ (imported from Data.Bimap),\n‘HM.fromList’ (imported from Data.HashMap.Strict)"
        in extractRenamableTerms msg `shouldBe` ["fromJust", "BM.fromList", "HM.fromList"]
    it "doen't pick up irrelvant messages" $
      let msg = "The import of ‘Control.Exception’ is redundant\n      except perhaps to import instances from ‘Control.Exception’"
        in extractRenamableTerms msg `shouldBe` []
      
    it "picks up variable not in scope with multiple suggestions" $
      let msg = "• Variable not in scope: uri\n• Perhaps you meant one of these:\n‘J.uri’ (imported from Language.Haskell.LSP.Types),\ndata constructor ‘J.Uri’ (imported from Language.Haskell.LSP.Types)"
        in extractRenamableTerms msg `shouldBe` ["J.uri", "J.Uri"]
    
    it "picks up data constructors" $
      let msg = "• Data constructor not in scope:\n    MarkupContent :: J.MarkupKind -> Maybe T.Text -> t0\n• Perhaps you meant ‘J.MarkupContent’ (imported from Language.Haskell.LSP.Types)"
        in extractRenamableTerms msg `shouldBe` ["J.MarkupContent"]

    it "returns nothing when there's no suggetsions" $
      let msg = "Variable not in scope:\n  fromJust\n    :: Maybe (Maybe (List CommandOrCodeAction)) -> Maybe (List a)"
        in extractRenamableTerms msg `shouldBe` []
    it "picks up variables not in scope on new line" $
      let msg = "• Variable not in scope:\n    forM_ :: [CodeAction] -> (s0 -> Expectation) -> IO a0\n• Perhaps you meant ‘iforM_’ (imported from Control.Lens)"
        in extractRenamableTerms msg `shouldBe` ["iforM_"]
  
  describe "typed holes" $
    it "picks them up" $ do
      msg <- T.readFile "test/testdata/typedHoleDiag.txt"
      extractHoleSubstitutions msg `shouldBe` ["Nothing", "mempty", "undefined", "GM.mzero"]

  describe "missing package code actions" $ do
    it "pick up relevant messages" $ 
      let msg = "Could not find module ‘Foo.Bar’\n      Use -v to see a list of the files searched for."
        in extractModuleName msg `shouldBe` Just "Foo.Bar"
    it "don't pick up irrelevant messages" $ 
      let msg = "Could not find modulez ‘Foo.Bar’\n      Use -v to see a list of the files searched for."
        in extractModuleName msg `shouldBe` Nothing