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
    it "pick up data constructors" $
      let msg = "Data constructor not in scope: ExitFailure :: Integer -> t"
        in extractImportableTerm msg `shouldBe` Just "ExitFailure :: Integer -> t"

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

    it "picks up qualified functions" $
      let msg = "    Not in scope: ‘Foo.printResul’\n\
                \    Perhaps you meant one of these:\n\
                \      ‘Foo.printResult’ (imported from Mod.Bar.Foo),\n\
                \      ‘Foo.formatResult’ (imported from Mod.Bar.Foo)\n\
                \    Module ‘Mod.Bar.Foo’ does not export ‘printResul’"
        in extractRenamableTerms msg `shouldBe` ["Foo.printResult", "Foo.formatResult"]

    it "picks up local definitions" $
      let msg = "• Variable not in scope: as :: f Foo\n\
                \    • Perhaps you meant one of these:\n\
                \        ‘ast’ (line 235)"
        in extractRenamableTerms msg `shouldBe` ["ast"]

    it "picks up definitions in same line" $
      let msg = "• Variable not in scope: as :: f Foo\n\
                \    • Perhaps you meant one of these:\n\
                \        ‘ast’ (line 235), ‘abs’ (imported from Prelude)"
        in extractRenamableTerms msg `shouldBe` ["ast", "abs"]

  describe "typed holes" $ do
    it "picks them up" $ do
      msg <- T.readFile "test/testdata/typedHoleDiag.txt"
      let substitutions = ValidSubstitutions [ FunctionSig "Nothing" (TypeDef "forall a. Maybe a")
                                             , FunctionSig "mempty" (TypeDef "forall a. Monoid a => a")
                                             , FunctionSig "undefined" (TypeDef "forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a")
                                             , FunctionSig "GM.mzero" (TypeDef "forall (m :: * -> *). GM.MonadPlus m => forall a. m a")
                                             ]


          bindings = Bindings [ FunctionSig "diag" (TypeDef "T.Text")
                              , FunctionSig "extractHoles" (TypeDef "T.Text -> Maybe T.Text")
                              ]

          expected = Just (TypeDef "Maybe T.Text", substitutions, bindings)
      extractHoleSubstitutions msg `shouldBe` expected

    it "removes bound at" $ do
      msg <- T.readFile "test/testdata/typedHoleDiag2.txt"
      let substitutions = ValidSubstitutions [FunctionSig "undefined" (TypeDef "forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a")]

          bindings = Bindings [ FunctionSig "stuff" (TypeDef "A -> A")
                              , FunctionSig "x" (TypeDef "[A]")
                              , FunctionSig "foo2" (TypeDef "[A] -> A")
                              ]

          expected = Just (TypeDef "A", substitutions, bindings)
      extractHoleSubstitutions msg `shouldBe` expected

    it "tolerates long signatures" $ do
      msg <- T.readFile "test/testdata/typedHoleDiag3.txt"
      let substitutions = ValidSubstitutions [ FunctionSig "mempty" (TypeDef "forall a. Monoid a => a")
                                             , FunctionSig "undefined" (TypeDef "forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a")
                                             , FunctionSig "idm" (TypeDef "forall m. Monoid m => m")
                                             ]
          longSig = "Either Language.Docker.Parser.Error Dockerfile -> Either Language.Docker.Parser.Error [Rules.RuleCheck]"
          longSig2 = "[IgnoreRule] -> t -> IO (Either Language.Docker.Parser.Error [Rules.RuleCheck])"
          bindings = Bindings [ FunctionSig "processedFile" (TypeDef longSig)
                              , FunctionSig "processRules" (TypeDef "Dockerfile -> [Rules.RuleCheck]")
                              , FunctionSig "ignoredRules" (TypeDef "Rules.RuleCheck -> Bool")
                              , FunctionSig "dockerFile" (TypeDef "t")
                              , FunctionSig "ignoreRules" (TypeDef "[IgnoreRule]")
                              , FunctionSig "lintDockerfile" (TypeDef longSig2)
                              ]

          expected = Just (TypeDef "t -> FilePath", substitutions, bindings)
      extractHoleSubstitutions msg `shouldBe` expected

  describe "missing package code actions" $ do
    it "pick up relevant messages" $ 
      let msg = "Could not find module ‘Foo.Bar’\n      Use -v to see a list of the files searched for."
        in extractModuleName msg `shouldBe` Just "Foo.Bar"
    it "don't pick up irrelevant messages" $ 
      let msg = "Could not find modulez ‘Foo.Bar’\n      Use -v to see a list of the files searched for."
        in extractModuleName msg `shouldBe` Nothing
