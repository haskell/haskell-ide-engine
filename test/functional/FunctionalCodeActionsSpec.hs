{-# LANGUAGE OverloadedStrings #-}

module FunctionalCodeActionsSpec where

import           Control.Applicative.Combinators
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Language.Haskell.LSP.Test as Test
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           Test.Hspec
import           TestUtils

spec :: Spec
spec = describe "code actions" $ do
  describe "hlint suggestions" $ do
    it "provides 3.8 code actions" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "ApplyRefact2.hs" "haskell"

      mdiags <- waitForDiagnostics
      (diags,reduceDiag) <- case mdiags of
        (reduceDiag:_) -> return (mdiags,reduceDiag)
        _ -> fail "match fail"

      liftIO $ do
        length diags `shouldBe` 2
        reduceDiag ^. L.range `shouldBe` Range (Position 1 0) (Position 1 12)
        reduceDiag ^. L.severity `shouldBe` Just DsInfo
        reduceDiag ^. L.code `shouldBe` Just "Eta reduce"
        reduceDiag ^. L.source `shouldBe` Just "hlint"

      mcas <- getAllCodeActions doc
      ca <- case mcas of
        (CACodeAction ca:_) -> return ca
        _ -> fail "match fail"

      -- Evaluate became redundant id in later hlint versions
      liftIO $ ["Apply hint:Redundant id", "Apply hint:Evaluate"] `shouldContain` [ca ^. L.title]

      executeCodeAction ca

      contents <- getDocumentEdit doc
      liftIO $ contents `shouldBe` "main = undefined\nfoo x = x\n"

      noDiagnostics

    -- ---------------------------------

    it "falls back to pre 3.8 code actions" $ runSession hieCommand noLiteralCaps "test/testdata" $ do
      doc <- openDoc "ApplyRefact2.hs" "haskell"

      _ <- waitForDiagnostics

      mcmd <- getAllCodeActions doc
      cmd <- case mcmd of
        (CACommand cmd:_) -> return cmd
        _ -> fail "match fail"

      -- Evaluate became redundant id in later hlint versions
      liftIO $ ["Apply hint:Redundant id", "Apply hint:Evaluate"] `shouldContain` [cmd ^. L.title ]

      executeCommand cmd

      contents <- skipManyTill publishDiagnosticsNotification $ getDocumentEdit doc
      liftIO $ contents `shouldBe` "main = undefined\nfoo x = x\n"

      noDiagnostics

  -- -----------------------------------

  describe "rename suggestions" $ do
    it "works" $ runSession hieCommand noLiteralCaps "test/testdata" $ do
      doc <- openDoc "CodeActionRename.hs" "haskell"

      _ <- waitForDiagnosticsSource "ghcmod"

      mcmds <- getAllCodeActions doc
      cmd <- case mcmds of
        CACommand cmd:_ -> return cmd
        _ -> fail "match fail"
      executeCommand cmd

      mxs <- T.lines <$> documentContents doc
      x <- case mxs of
        x:_ -> return x
        _ -> fail "match fail"

      liftIO $ x `shouldBe` "main = putStrLn \"hello\""
    it "doesn't give both documentChanges and changes" $
      runSession hieCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "CodeActionRename.hs" "haskell"

        _ <- waitForDiagnosticsSource "ghcmod"

        mcmd <- (!! 2) <$> getAllCodeActions doc
        cmd <- case mcmd of
          CACommand cmd -> return cmd
          _ -> fail "match fail"
        let Just (List [Object args]) = cmd ^. L.arguments
            Object editParams = args HM.! "fallbackWorkspaceEdit"
        liftIO $ do
          editParams `shouldSatisfy` HM.member "changes"
          editParams `shouldNotSatisfy` HM.member "documentChanges"

        executeCommand cmd

        mxs <- T.lines <$> documentContents doc
        x <- case mxs of
          _:x:_ -> return x
          _ -> fail "match fail"
        liftIO $ x `shouldBe` "foo = putStrLn \"world\""

  -- -----------------------------------

  it "provides import suggestions and 3.8 code action kinds" $
    runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImport.hs" "haskell"

      -- ignore the first empty hlint diagnostic publish
      mdiag <- count 2 waitForDiagnostics
      diag <- case mdiag of
        [_,diag:_] -> return diag
        _ -> fail "match fail"
      liftIO $ diag ^. L.message `shouldBe` "Variable not in scope: when :: Bool -> IO () -> IO ()"

      actionsOrCommands <- getAllCodeActions doc
      let actns = map fromAction actionsOrCommands

      liftIO $ do
        head actns ^. L.title `shouldBe` "Import module Control.Monad"
        forM_ actns $ \a -> do
          a ^. L.kind `shouldBe` Just CodeActionQuickFix
          a ^. L.command `shouldSatisfy` isJust
          a ^. L.edit `shouldBe` Nothing
          let hasOneDiag (Just (List [_])) = True
              hasOneDiag _ = False
          a ^. L.diagnostics `shouldSatisfy` hasOneDiag
        length actns `shouldBe` 5

      executeCodeAction (head actns)

      contents <- getDocumentEdit doc
      liftIO $ contents `shouldBe` "import Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""

  -- -----------------------------------

  describe "add package suggestions" $ do
    it "adds to .cabal files" $ runSession hieCommand fullCaps "test/testdata/addPackageTest/cabal" $ do
      doc <- openDoc "AddPackage.hs" "haskell"

      -- ignore the first empty hlint diagnostic publish
      mdiag <- count 2 waitForDiagnostics
      diag <- case mdiag of
        [_,diag:_] -> return diag
        _ -> fail "match fail"

      liftIO $ diag ^. L.message `shouldSatisfy` T.isPrefixOf "Could not find module ‘Data.Text’"

      maction <- getAllCodeActions doc
      action <- case maction of
        (CACodeAction action:_) -> return action
        _ -> fail "match fail"

      liftIO $ do
        action ^. L.title `shouldBe` "Add text as a dependency"
        action ^. L.kind `shouldBe` Just CodeActionQuickFix
        action ^. L.command . _Just . L.command `shouldSatisfy` T.isSuffixOf "package:add"

      executeCodeAction action

      contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "add-package-test.cabal"
      liftIO $ T.lines contents `shouldSatisfy` \x -> any (\l -> "text -any" `T.isSuffixOf` (x !! l)) [15, 16]

    it "adds to hpack package.yaml files" $
      runSession hieCommand fullCaps "test/testdata/addPackageTest/hpack" $ do
        doc <- openDoc "app/Asdf.hs" "haskell"

        -- ignore the first empty hlint diagnostic publish
        mdiag <- count 2 waitForDiagnostics
        diag <- case mdiag of
          [_,diag:_] -> return diag
          _ -> fail "match fail"

        liftIO $ diag ^. L.message `shouldSatisfy` T.isPrefixOf "Could not find module ‘Codec.Compression.GZip’"

        mActions <- getAllCodeActions doc
        let allActions = map fromAction mActions
            action = head allActions

        liftIO $ do
          action ^. L.title `shouldBe` "Add zlib as a dependency"
          forM_ allActions $ \a -> a ^. L.kind `shouldBe` Just CodeActionQuickFix
          forM_ allActions $ \a -> a ^. L.command . _Just . L.command `shouldSatisfy` T.isSuffixOf "package:add"

        executeCodeAction action

        contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "package.yaml"
        liftIO $ do
          T.lines contents !! 33 `shouldSatisfy` T.isSuffixOf "zlib"
          T.lines contents !! 12 `shouldNotSatisfy` T.isSuffixOf "zlib"
          T.lines contents !! 13 `shouldNotSatisfy` T.isSuffixOf "zlib"

  -- -----------------------------------

  describe "redundant import code actions" $ do
    it "remove solitary redundant imports" $
      runSession hieCommand fullCaps "test/testdata/redundantImportTest/" $ do
        doc <- openDoc "src/CodeActionRedundant.hs" "haskell"

        -- ignore the first empty hlint diagnostic publish
        mdiag <- count 2 waitForDiagnostics
        diag <- case mdiag of
          [_,diag:_] -> return diag
          _ -> fail "match fail"

        liftIO $ diag ^. L.message `shouldSatisfy` T.isPrefixOf "The import of ‘Data.List’ is redundant"

        mActions <- getAllCodeActions doc

        let allActions@[removeAction, changeAction] = map fromAction mActions

        liftIO $ do
          removeAction ^. L.title `shouldBe` "Remove redundant import"
          changeAction ^. L.title `shouldBe` "Import instances"
          forM_ allActions $ \a -> a ^. L.kind `shouldBe` Just CodeActionQuickFix
          forM_ allActions $ \a -> a ^. L.command `shouldBe` Nothing
          forM_ allActions $ \a -> a ^. L.edit `shouldSatisfy` isJust

        executeCodeAction removeAction

        -- No command/applyworkspaceedit should be here, since action
        -- provides workspace edit property which skips round trip to
        -- the server
        contents <- documentContents doc
        liftIO $ contents `shouldBe` "main :: IO ()\nmain = putStrLn \"hello\""
    it "doesn't touch other imports" $ runSession hieCommand noLiteralCaps "test/testdata/redundantImportTest/" $ do
      doc <- openDoc "src/MultipleImports.hs" "haskell"

      _ <- count 2 waitForDiagnostics

      mcmd <- getAllCodeActions doc
      cmd <- case mcmd of
        [CACommand cmd, _] -> return cmd
        _ -> fail "match fail"

      executeCommand cmd

      contents <- documentContents doc

      liftIO $ contents `shouldBe`
        "module MultipleImports where\n\
        \import Data.Maybe\n\
        \foo :: Int\n\
        \foo = fromJust (Just 3)\n"

  -- -----------------------------------

  describe "typed hole code actions" $ do
      it "works" $
        runSession hieCommand fullCaps "test/testdata" $ do
          doc <- openDoc "TypedHoles.hs" "haskell"
          _ <- waitForDiagnosticsSource "ghcmod"
          cas <- map (\(CACodeAction x)-> x) <$> getAllCodeActions doc

          suggestion <-
            if ghc84 then do
              liftIO $ map (^. L.title) cas `shouldMatchList`
                [ "Substitute hole (Int) with maxBound (forall a. Bounded a => a)"
                , "Substitute hole (Int) with minBound (forall a. Bounded a => a)"
                , "Substitute hole (Int) with undefined (forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a)"
                ]
              return "maxBound"
            else do
              liftIO $ map (^. L.title) cas `shouldMatchList`
                [ "Substitute hole (Int) with x ([Int])"
                , "Substitute hole (Int) with foo ([Int] -> Int)"
                ]
              return "x"

          executeCodeAction $ head cas

          contents <- documentContents doc

          liftIO $ contents `shouldBe`
            "module TypedHoles where\n\
            \foo :: [Int] -> Int\n\
            \foo x = " <> suggestion

      it "shows more suggestions" $
        runSession hieCommand fullCaps "test/testdata" $ do
          doc <- openDoc "TypedHoles2.hs" "haskell"
          _ <- waitForDiagnosticsSource "ghcmod"
          cas <- map fromAction <$> getAllCodeActions doc

          suggestion <-
            if ghc84 then do
              liftIO $ map (^. L.title) cas `shouldMatchList`
                [ "Substitute hole (A) with undefined (forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a)"
                , "Substitute hole (A) with stuff (A -> A)"
                , "Substitute hole (A) with x ([A])"
                , "Substitute hole (A) with foo2 ([A] -> A)"
                ]
              return "undefined"
          else do
              liftIO $ map (^. L.title) cas `shouldMatchList`
                [ "Substitute hole (A) with stuff (A -> A)"
                , "Substitute hole (A) with x ([A])"
                , "Substitute hole (A) with foo2 ([A] -> A)"
                ]
              return "stuff"

          executeCodeAction $ head cas

          contents <- documentContents doc

          liftIO $ contents `shouldBe`
            "module TypedHoles2 (foo2) where\n\
            \newtype A = A Int\n\
            \foo2 :: [A] -> A\n\
            \foo2 x = " <> suggestion <> "\n\
            \  where\n\
            \    stuff (A a) = A (a + 1)\n"

  -- -----------------------------------

  describe "missing top level signature code actions" $
    it "Adds top level signature" $
      runSession hieCommand fullCaps "test/testdata/" $ do
        doc <- openDoc "TopLevelSignature.hs" "haskell"

        _ <- waitForDiagnosticsSource "ghcmod"
        cas <- map fromAction <$> getAllCodeActions doc

        liftIO $ map (^. L.title) cas `shouldContain` [ "Add signature: main :: IO ()"]

        executeCodeAction $ head cas

        contents <- documentContents doc

        let expected = "{-# OPTIONS_GHC -Wall #-}\n\
                       \module TopLevelSignature where\n\
                       \main :: IO ()\n\
                       \main = do\n\
                       \  putStrLn \"Hello\"\n\
                       \  return ()\n"

        liftIO $ contents `shouldBe` expected

  -- -----------------------------------

  describe "missing pragma warning code actions" $
    it "Adds TypeSynonymInstances pragma" $
      runSession hieCommand fullCaps "test/testdata/addPragmas" $ do
        doc <- openDoc "NeedsPragmas.hs" "haskell"

        _ <- waitForDiagnosticsSource "ghcmod"
        cas <- map fromAction <$> getAllCodeActions doc

        liftIO $ map (^. L.title) cas `shouldContain` [ "Add \"TypeSynonymInstances\""]
        liftIO $ map (^. L.title) cas `shouldContain` [ "Add \"FlexibleInstances\""]

        executeCodeAction $ head cas

        contents <- getDocumentEdit doc

        let expected = "{-# LANGUAGE TypeSynonymInstances #-}\n\n\
                       \import GHC.Generics\n\n\
                       \main = putStrLn \"hello\"\n\n\
                       \type Foo = Int\n\n\
                       \instance Show Foo where\n\
                       \  show x = undefined\n\n\
                       \instance Show (Int,String) where\n\
                       \  show  = undefined\n\n\
                       \data FFF a = FFF Int String a\n\
                       \           deriving (Generic,Functor,Traversable)\n"

        liftIO $ contents `shouldBe` expected

  -- -----------------------------------

  describe "unused term code actions" $
    it "Prefixes with '_'" $
      runSession hieCommand fullCaps "test/testdata/" $ do
        doc <- openDoc "UnusedTerm.hs" "haskell"

        _ <- waitForDiagnosticsSource "ghcmod"
        cas <- map fromAction <$> getAllCodeActions doc

        liftIO $ map (^. L.title) cas `shouldContain` [ "Prefix imUnused with _"]

        executeCodeAction $ head cas

        edit <- getDocumentEdit doc

        let expected = "{-# OPTIONS_GHC -Wall #-}\n\
                        \module UnusedTerm () where\n\
                        \_imUnused :: Int -> Int\n\
                        \_imUnused 1 = 1\n\
                        \_imUnused 2 = 2\n\
                        \_imUnused _ = 3\n"

        liftIO $ edit `shouldBe` expected

-- ---------------------------------------------------------------------

fromAction :: CAResult -> CodeAction
fromAction (CACodeAction action) = action
fromAction _ = error "Not a code action"

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) Nothing
