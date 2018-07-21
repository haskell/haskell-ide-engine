{-# LANGUAGE OverloadedStrings #-}
module CodeActionsSpec where

import Test.Hspec
import Haskell.Ide.Engine.LSP.CodeActions

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

  describe "missing package code actions" $ do
    it "pick up relevant messages" $ 
      let msg = "Could not find module ‘Foo.Bar’\n      Use -v to see a list of the files searched for."
        in extractModuleName msg `shouldBe` Just "Foo.Bar"
    it "don't pick up irrelevant messages" $ 
      let msg = "Could not find modulez ‘Foo.Bar’\n      Use -v to see a list of the files searched for."
        in extractModuleName msg `shouldBe` Nothing


  {-

  TODO: Test these
  
  Variable not in scope:
  liftIO
    :: IO b0
       -> conduit-parse-0.2.1.0:Data.Conduit.Parser.Internal.ConduitParser
            Language.Haskell.LSP.Messages.FromServerMessage
            (Control.Monad.Trans.State.Lazy.StateT
               haskell-lsp-test-0.1.0.0:Language.Haskell.LSP.Test.Session.SessionState
               (Control.Monad.Trans.Reader.ReaderT
                  haskell-lsp-test-0.1.0.0:Language.Haskell.LSP.Test.Session.SessionContext
                  IO))
            a0

Variable not in scope:
  fromJust
    :: Maybe (Maybe (List CommandOrCodeAction)) -> Maybe (List a)

    • Variable not in scope:
    forM_ :: [CodeAction] -> (s0 -> Expectation) -> IO a0
• Perhaps you meant ‘iforM_’ (imported from Control.Lens)
  -}
