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

  describe "rename code actions" $ do
    it "pick up variable not in scope perhaps you meant" $
      let msg = "• Variable not in scope: fromBust\n• Perhaps you meant ‘fromJust’ (imported from Data.Maybe)"
        in extractRenamableTerms msg `shouldBe` ["fromJust"]
      
    it "picks up variable not in scope with multiple suggestions" $
      let msg = "• Variable not in scope: uri\n• Perhaps you meant one of these:\n‘J.uri’ (imported from Language.Haskell.LSP.Types),\ndata constructor ‘J.Uri’ (imported from Language.Haskell.LSP.Types)"
        in extractRenamableTerms msg `shouldBe` ["J.uri", "J.Uri"]

        -- TODO: write a test for this:
        -- "Variable not in scope:
        -- asks
        --   :: (Core.LspFuncs c0 -> J.Uri -> IO (Maybe VirtualFile))
        --      -> Control.Monad.Trans.Reader.ReaderT
        --           (Core.LspFuncs Haskell.Ide.Engine.LSP.Config.Config) IO (t1 -> t0)"