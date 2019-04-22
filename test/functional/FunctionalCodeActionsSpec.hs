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
import           Haskell.Ide.Engine.Config
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
      diags@(reduceDiag:_) <- waitForDiagnostics

      liftIO $ do
        length diags `shouldBe` 2
        reduceDiag ^. L.range `shouldBe` Range (Position 1 0) (Position 1 12)
        reduceDiag ^. L.severity `shouldBe` Just DsInfo
        reduceDiag ^. L.code `shouldBe` Just "Eta reduce"
        reduceDiag ^. L.source `shouldBe` Just "hlint"

      (CACodeAction ca:_) <- getAllCodeActions doc

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

      (CACommand cmd:_) <- getAllCodeActions doc

      -- Evaluate became redundant id in later hlint versions
      liftIO $ ["Apply hint:Redundant id", "Apply hint:Evaluate"] `shouldContain` [cmd ^. L.title ]

      executeCommand cmd

      contents <- skipManyTill publishDiagnosticsNotification $ getDocumentEdit doc
      liftIO $ contents `shouldBe` "main = undefined\nfoo x = x\n"

      noDiagnostics

    it "runs diagnostics on save" $ runSession hieCommand fullCaps "test/testdata" $ do
      let config = def { diagnosticsOnChange = False }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      doc <- openDoc "ApplyRefact2.hs" "haskell"
      diags@(reduceDiag:_) <- waitForDiagnostics

      liftIO $ do
        length diags `shouldBe` 2
        reduceDiag ^. L.range `shouldBe` Range (Position 1 0) (Position 1 12)
        reduceDiag ^. L.severity `shouldBe` Just DsInfo
        reduceDiag ^. L.code `shouldBe` Just "Eta reduce"
        reduceDiag ^. L.source `shouldBe` Just "hlint"

      (CACodeAction ca:_) <- getAllCodeActions doc

      -- Evaluate became redundant id in later hlint versions
      liftIO $ ["Apply hint:Redundant id", "Apply hint:Evaluate"] `shouldContain` [ca ^. L.title]

      executeCodeAction ca

      contents <- getDocumentEdit doc
      liftIO $ contents `shouldBe` "main = undefined\nfoo x = x\n"
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

      noDiagnostics

  -- -----------------------------------

  describe "rename suggestions" $ do
    it "works" $ runSession hieCommand noLiteralCaps "test/testdata" $ do
      doc <- openDoc "CodeActionRename.hs" "haskell"

      _ <- waitForDiagnosticsSource "ghcmod"

      CACommand cmd:_ <- getAllCodeActions doc
      executeCommand cmd

      x:_ <- T.lines <$> documentContents doc
      liftIO $ x `shouldBe` "main = putStrLn \"hello\""
    it "doesn't give both documentChanges and changes" $
      runSession hieCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "CodeActionRename.hs" "haskell"

        _ <- waitForDiagnosticsSource "ghcmod"

        CACommand cmd <- (!! 2) <$> getAllCodeActions doc
        let Just (List [Object args]) = cmd ^. L.arguments
            Object editParams = args HM.! "fallbackWorkspaceEdit"
        liftIO $ do
          editParams `shouldSatisfy` HM.member "changes"
          editParams `shouldNotSatisfy` HM.member "documentChanges"

        executeCommand cmd

        _:x:_ <- T.lines <$> documentContents doc
        liftIO $ x `shouldBe` "foo = putStrLn \"world\""

  describe "import suggestions" $ do
    it "works with 3.8 code action kinds" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImport.hs" "haskell"

      -- ignore the first empty hlint diagnostic publish
      [_,diag:_] <- count 2 waitForDiagnostics
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
      liftIO $ contents `shouldBe` "import           Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""
    it "formats with brittany" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportBrittany.hs" "haskell"
      _ <- waitForDiagnosticsSource "ghcmod"

      actionsOrCommands <- getAllCodeActions doc
      let action:_ = map fromAction actionsOrCommands
      executeCodeAction action

      contents <- getDocumentEdit doc
      liftIO $ do
        let l1:l2:_ = T.lines contents
        l1 `shouldBe` "import qualified Data.Maybe"
        l2 `shouldBe` "import           Control.Monad"
    it "respects format config" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportBrittany.hs" "haskell"
      _ <- waitForDiagnosticsSource "ghcmod"

      let config = def { formatOnImportOn = False }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      actionsOrCommands <- getAllCodeActions doc
      let action:_ = map fromAction actionsOrCommands
      executeCodeAction action

      contents <- getDocumentEdit doc
      liftIO $ do
        let l1:l2:_ = T.lines contents
        l1 `shouldBe` "import qualified Data.Maybe"
        l2 `shouldBe` "import Control.Monad"

  describe "add package suggestions" $ do
    it "adds to .cabal files" $ runSession hieCommand fullCaps "test/testdata/addPackageTest/cabal" $ do
      doc <- openDoc "AddPackage.hs" "haskell"

      -- ignore the first empty hlint diagnostic publish
      [_,diag:_] <- count 2 waitForDiagnostics

      if ghcVersion == GHC86
        then
          liftIO $ diag ^. L.message `shouldSatisfy` T.isPrefixOf "Could not load module \8216Data.Text\8217"
         else
          liftIO $ diag ^. L.message `shouldSatisfy` T.isPrefixOf "Could not find module ‘Data.Text’"

      acts <- getAllCodeActions doc
      let (CACodeAction action:_) = acts

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
        [_,diag:_] <- count 2 waitForDiagnostics

        let preds = [ T.isPrefixOf "Could not load module ‘Codec.Compression.GZip’"
                    , T.isPrefixOf "Could not find module ‘Codec.Compression.GZip’"
                    ]
          in liftIO $ diag ^. L.message `shouldSatisfy` \x -> any (\f -> f x) preds

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
        [_,diag:_] <- count 2 waitForDiagnostics

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

      [CACommand cmd, _] <- getAllCodeActions doc

      executeCommand cmd

      contents <- documentContents doc

      liftIO $ (T.lines contents) `shouldBe`
        [ "module MultipleImports where"
        , "import Data.Maybe"
        , "foo :: Int"
        , "foo = fromJust (Just 3)"
        ]

  -- -----------------------------------

  describe "typed hole code actions" $ do
      it "works" $
        runSession hieCommand fullCaps "test/testdata" $ do
          doc <- openDoc "TypedHoles.hs" "haskell"
          _ <- waitForDiagnosticsSource "ghcmod"
          cas <- map (\(CACodeAction x)-> x) <$> getAllCodeActions doc

          suggestion <-
            case ghcVersion of
              GHC86 -> do
                liftIO $ map (^. L.title) cas `shouldMatchList`
                  [ "Substitute hole (Int) with x ([Int])"
                  , "Substitute hole (Int) with foo ([Int] -> Int Valid hole fits include)"
                  , "Substitute hole (Int) with maxBound (forall a. Bounded a => a with maxBound @Int)"
                  , "Substitute hole (Int) with minBound (forall a. Bounded a => a with minBound @Int)"
                  ]
                return "x"
              GHC84 -> do
                liftIO $ map (^. L.title) cas `shouldMatchList`
                  [ "Substitute hole (Int) with maxBound (forall a. Bounded a => a)"
                  , "Substitute hole (Int) with minBound (forall a. Bounded a => a)"
                  , "Substitute hole (Int) with undefined (forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a)"
                  ]
                return "maxBound"
              GHCPre84 -> do
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
            case ghcVersion of
              GHC86 -> do
                liftIO $ map (^. L.title) cas `shouldMatchList`
                  [ "Substitute hole (A) with stuff (A -> A)"
                  , "Substitute hole (A) with x ([A])"
                  , "Substitute hole (A) with foo2 ([A] -> A)"
                  ]
                return "stuff"
              GHC84 -> do
                liftIO $ map (^. L.title) cas `shouldMatchList`
                  [ "Substitute hole (A) with undefined (forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a)"
                  , "Substitute hole (A) with stuff (A -> A)"
                  , "Substitute hole (A) with x ([A])"
                  , "Substitute hole (A) with foo2 ([A] -> A)"
                  ]
                return "undefined"
              GHCPre84 -> do
                liftIO $ map (^. L.title) cas `shouldMatchList`
                  [ "Substitute hole (A) with stuff (A -> A)"
                  , "Substitute hole (A) with x ([A])"
                  , "Substitute hole (A) with foo2 ([A] -> A)"
                  ]
                return "stuff"

          executeCodeAction $ head cas

          contents <- documentContents doc

          liftIO $ (T.lines contents) `shouldBe`
            [ "module TypedHoles2 (foo2) where"
            , "newtype A = A Int"
            , "foo2 :: [A] -> A"
            , "foo2 x = " <> suggestion <> ""
            , "  where"
            , "    stuff (A a) = A (a + 1)"
            ]

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

        let expected = [ "{-# OPTIONS_GHC -Wall #-}"
                       , "module TopLevelSignature where"
                       , "main :: IO ()"
                       , "main = do"
                       , "  putStrLn \"Hello\""
                       , "  return ()"
                       ]

        liftIO $ (T.lines contents) `shouldBe` expected

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

        let expected = [ "{-# LANGUAGE TypeSynonymInstances #-}"
                       , ""
                       , "import GHC.Generics"
                       , ""
                       , "main = putStrLn \"hello\""
                       , ""
                       , "type Foo = Int"
                       , ""
                       , "instance Show Foo where"
                       , "  show x = undefined"
                       , ""
                       , "instance Show (Int,String) where"
                       , "  show  = undefined"
                       , ""
                       , "data FFF a = FFF Int String a"
                       , "           deriving (Generic,Functor,Traversable)"
                       ]

        liftIO $ (T.lines contents) `shouldBe` expected

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

        let expected = [ "{-# OPTIONS_GHC -Wall #-}"
                       , "module UnusedTerm () where"
                       , "_imUnused :: Int -> Int"
                       , "_imUnused 1 = 1"
                       , "_imUnused 2 = 2"
                       , "_imUnused _ = 3"
                       ]

        liftIO $ edit `shouldBe` T.unlines expected

  it "respect 'only' parameter" $ runSession hieCommand fullCaps "test/testdata" $ do
    doc <- openDoc "CodeActionOnly.hs" "haskell"
    _ <- count 2 waitForDiagnostics -- need to wait for both hlint and ghcmod
    diags <- getCurrentDiagnostics doc
    let params = CodeActionParams doc (Range (Position 2 10) (Position 4 0)) caContext
        caContext = CodeActionContext (List diags) (Just (List [CodeActionRefactorInline]))
    ResponseMessage _ _ (Just (List res)) _ <- request TextDocumentCodeAction params :: Session CodeActionResponse
    let cas = map fromAction res
        kinds = map (^. L.kind) cas
    liftIO $ do
      kinds `shouldNotSatisfy` null
      kinds `shouldNotSatisfy` any (Just CodeActionRefactorInline /=)
      kinds `shouldSatisfy` all (Just CodeActionRefactorInline ==)

-- ---------------------------------------------------------------------

fromAction :: CAResult -> CodeAction
fromAction (CACodeAction action) = action
fromAction _ = error "Not a code action"

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) Nothing
