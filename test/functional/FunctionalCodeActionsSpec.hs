{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module FunctionalCodeActionsSpec where

import           Control.Applicative.Combinators
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
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
        reduceDiag ^. L.code `shouldBe` Just (StringValue "Eta reduce")
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
        reduceDiag ^. L.code `shouldBe` Just (StringValue "Eta reduce")
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

      _ <- waitForDiagnosticsSource "bios"

      CACommand cmd:_ <- getAllCodeActions doc
      executeCommand cmd

      x:_ <- T.lines <$> documentContents doc
      liftIO $ x `shouldBe` "main = putStrLn \"hello\""
    it "doesn't give both documentChanges and changes" $
      runSession hieCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "CodeActionRename.hs" "haskell"

        _ <- waitForDiagnosticsSource "bios"

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
    describe "formats with brittany" $ hsImportSpec "brittany"
      [ -- Expected output for simple format.
        [ "import qualified Data.Maybe"
        , "import           Control.Monad"
        , "main :: IO ()"
        , "main = when True $ putStrLn \"hello\""
        ]
      , -- Use an import list and format the output.
        [ "import qualified Data.Maybe"
        , "import           Control.Monad                  ( when )"
        , "main :: IO ()"
        , "main = when True $ putStrLn \"hello\""
        ]
      , -- Multiple import lists, should not introduce multiple newlines.
        [ "import           System.IO                      ( stdout"
        , "                                                , hPutStrLn"
        , "                                                )"
        , "import           Control.Monad                  ( when )"
        , "import           Data.Maybe                     ( fromMaybe )"
        , "-- | Main entry point to the program"
        , "main :: IO ()"
        , "main ="
        , "    when True"
        , "        $ hPutStrLn stdout"
        , "        $ fromMaybe \"Good night, World!\" (Just \"Hello, World!\")"
        ]
      , -- Complex imports for Constructos and functions
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "import           System.IO                      ( IO"
        , "                                                , hPutStrLn"
        , "                                                , stderr"
        , "                                                )"
        , "import           Prelude                        ( Bool(..) )"
        , "import           Control.Monad                  ( when )"
        , "import           Data.Function                  ( ($) )"
        , "import           Data.Maybe                     ( fromMaybe"
        , "                                                , Maybe(Just)"
        , "                                                )"
        , "-- | Main entry point to the program"
        , "main :: IO ()"
        , "main ="
        , "    when True"
        , "        $ hPutStrLn stderr"
        , "        $ fromMaybe \"Good night, World!\" (Just \"Hello, World!\")"
        ]
      ]
    describe "formats with floskell" $ hsImportSpec "floskell"
      [ -- Expected output for simple format.
        [ "import qualified Data.Maybe"
        , "import           Control.Monad"
        , "main :: IO ()"
        , "main = when True $ putStrLn \"hello\""
        ]
      , -- Use an import list and format the output.
        [ "import qualified Data.Maybe"
        , "import           Control.Monad (when)"
        , "main :: IO ()"
        , "main = when True $ putStrLn \"hello\""
        ]
      , -- Multiple import lists, should not introduce multiple newlines.
        [ "import           System.IO (stdout, hPutStrLn)"
        , "import           Control.Monad (when)"
        , "import           Data.Maybe (fromMaybe)"
        , "-- | Main entry point to the program"
        , "main :: IO ()"
        , "main ="
        , "    when True"
        , "        $ hPutStrLn stdout"
        , "        $ fromMaybe \"Good night, World!\" (Just \"Hello, World!\")"
        ]
      ,  -- Complex imports for Constructos and functions
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "import           System.IO (IO, hPutStrLn, stderr)"
        , "import           Prelude (Bool(..))"
        , "import           Control.Monad (when)"
        , "import           Data.Function (($))"
        , "import           Data.Maybe (fromMaybe, Maybe(Just))"
        , "-- | Main entry point to the program"
        , "main :: IO ()"
        , "main ="
        , "    when True"
        , "        $ hPutStrLn stderr"
        , "        $ fromMaybe \"Good night, World!\" (Just \"Hello, World!\")"
        ]
      ]
  describe "add package suggestions" $ do
    -- Only execute this test with ghc 8.4.4, below seems to be broken in the package.
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
    it "adds to .cabal files" $ runSession hieCommand fullCaps "test/testdata/addPackageTest/cabal-exe" $ do
      doc <- openDoc "AddPackage.hs" "haskell"

      -- ignore the first empty hlint diagnostic publish
      [_,diag:_] <- count 2 waitForDiagnostics

      let prefixes = [ "Could not load module `Data.Text'" -- Windows && GHC >= 8.6
                     , "Could not find module `Data.Text'" -- Windows
                     , "Could not load module ‘Data.Text’" -- GHC >= 8.6
                     , "Could not find module ‘Data.Text’"
                     ]
        in liftIO $ diag ^. L.message `shouldSatisfy` \m -> any (`T.isPrefixOf` m) prefixes

      acts <- getAllCodeActions doc
      let (CACodeAction action:_) = acts

      liftIO $ do
        action ^. L.title `shouldBe` "Add text as a dependency"
        action ^. L.kind `shouldBe` Just CodeActionQuickFix
        action ^. L.command . _Just . L.command `shouldSatisfy` T.isSuffixOf "package:add"

      executeCodeAction action

      contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "add-package-test.cabal"
      liftIO $ T.lines contents `shouldSatisfy` \x -> any (\l -> "text -any" `T.isSuffixOf` (x !! l)) [15, 16]
#endif
    it "adds to hpack package.yaml files" $
      runSession hieCommand fullCaps "test/testdata/addPackageTest/hpack-exe" $ do
        doc <- openDoc "app/Asdf.hs" "haskell"

        -- ignore the first empty hlint diagnostic publish
        [_,_:diag:_] <- count 2 waitForDiagnostics

        let prefixes = [ "Could not load module `Codec.Compression.GZip'" -- Windows && GHC >= 8.6
                       , "Could not find module `Codec.Compression.GZip'" -- Windows
                       , "Could not load module ‘Codec.Compression.GZip’" -- GHC >= 8.6
                       , "Could not find module ‘Codec.Compression.GZip’"
                       ]
          in liftIO $ diag ^. L.message `shouldSatisfy` \m -> any (`T.isPrefixOf` m) prefixes

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
          T.lines contents !! 3 `shouldSatisfy` T.isSuffixOf "zlib"
          T.lines contents !! 21 `shouldNotSatisfy` T.isSuffixOf "zlib"


  -- -----------------------------------

  describe "redundant import code actions" $ do
    it "remove solitary redundant imports" $
      runSession hieCommand fullCaps "test/testdata/redundantImportTest/" $ do
        doc <- openDoc "src/CodeActionRedundant.hs" "haskell"

        -- ignore the first empty hlint diagnostic publish
        [_,diag:_] <- count 2 waitForDiagnostics

        let prefixes = [ "The import of `Data.List' is redundant" -- Windows
                       , "The import of ‘Data.List’ is redundant"
                       ]
          in liftIO $ diag ^. L.message `shouldSatisfy` \m -> any (`T.isPrefixOf` m) prefixes

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
        liftIO $ contents `shouldBe` "module CodeActionRedundant where\nmain :: IO ()\nmain = putStrLn \"hello\""
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
          _ <- waitForDiagnosticsSource "bios"
          cas <- map (\(CACodeAction x)-> x) <$> getAllCodeActions doc

          suggestion <-
            case ghcVersion of
              GHC86 -> do
                liftIO $ map (^. L.title) cas `shouldMatchList`
                  [ "Substitute hole (Int) with x ([Int])"
                  , "Substitute hole (Int) with foo ([Int] -> Int)"
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

          liftIO $ contents `shouldBe` T.concat
            [ "module TypedHoles where\n"
            , "foo :: [Int] -> Int\n"
            , "foo x = " <> suggestion
            ]

      it "shows more suggestions" $
        runSession hieCommand fullCaps "test/testdata" $ do
          doc <- openDoc "TypedHoles2.hs" "haskell"
          _ <- waitForDiagnosticsSource "bios"
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

        _ <- waitForDiagnosticsSource "bios"
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

        _ <- waitForDiagnosticsSource "bios"
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

        _ <- waitForDiagnosticsSource "bios"
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
    let params = CodeActionParams doc (Range (Position 2 10) (Position 4 0)) caContext Nothing
        caContext = CodeActionContext (List diags) (Just (List [CodeActionRefactorInline]))
    ResponseMessage _ _ (Just (List res)) _ <- request TextDocumentCodeAction params
    let cas = map fromAction res
        kinds = map (^. L.kind) cas
    liftIO $ do
      kinds `shouldNotSatisfy` null
      kinds `shouldNotSatisfy` any (Just CodeActionRefactorInline /=)
      kinds `shouldSatisfy` all (Just CodeActionRefactorInline ==)

-- ---------------------------------------------------------------------
-- Parameterized HsImport Spec.
-- ---------------------------------------------------------------------
hsImportSpec :: T.Text -> [[T.Text]]-> Spec
hsImportSpec formatterName [e1, e2, e3, e4] =
  describe ("Execute HsImport with formatter " <> T.unpack formatterName) $ do
    it "works with 3.8 code action kinds" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImport.hs" "haskell"
      -- No Formatting:
      let config = def { formattingProvider = "none" }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      -- ignore the first empty hlint diagnostic publish
      [_,diag:_] <- count 2 waitForDiagnostics
      liftIO $ diag ^. L.message `shouldBe` "Variable not in scope: when :: Bool -> IO () -> IO ()"

      actionsOrCommands <- getAllCodeActions doc
      let actns = map fromAction actionsOrCommands

      liftIO $ do
        head actns        ^. L.title `shouldBe` "Import module Control.Monad"
        head (tail actns) ^. L.title `shouldBe` "Import module Control.Monad (when)"
        forM_ actns $ \a -> do
          a ^. L.kind `shouldBe` Just CodeActionQuickFix
          a ^. L.command `shouldSatisfy` isJust
          a ^. L.edit `shouldBe` Nothing
          let hasOneDiag (Just (List [_])) = True
              hasOneDiag _ = False
          a ^. L.diagnostics `shouldSatisfy` hasOneDiag
        length actns `shouldBe` 10

      executeCodeAction (head actns)

      contents <- getDocumentEdit doc
      liftIO $ contents `shouldBe` "import Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""

    it "formats" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportBrittany.hs" "haskell"
      _ <- waitForDiagnosticsSource "bios"

      let config = def { formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      actionsOrCommands <- getAllCodeActions doc
      let action:_ = map fromAction actionsOrCommands
      executeCodeAction action

      contents <- getDocumentEdit doc
      liftIO $ T.lines contents `shouldMatchList` e1

    it "import-list formats" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportBrittany.hs" "haskell"
      _ <- waitForDiagnosticsSource "bios"

      let config = def { formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      actionsOrCommands <- getAllCodeActions doc
      let _:action:_ = map fromAction actionsOrCommands
      executeCodeAction action

      contents <- getDocumentEdit doc
      liftIO $ T.lines contents `shouldMatchList` e2

    it "multiple import-list formats" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportList.hs" "haskell"

      let config = def { formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      let wantedCodeActionTitles = [ "Import module System.IO (hPutStrLn)"
                                   , "Import module System.IO (stdout)"
                                   , "Import module Control.Monad (when)"
                                   , "Import module Data.Maybe (fromMaybe)"
                                   ]

      contents <- executeAllCodeActions doc wantedCodeActionTitles

      liftIO $ Set.fromList (T.lines contents) `shouldBe` Set.fromList e3

    it "respects format config, multiple import-list" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportList.hs" "haskell"

      let config = def { formatOnImportOn = False, formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      let wantedCodeActionTitles = [ "Import module System.IO (hPutStrLn)"
                                   , "Import module System.IO (stdout)"
                                   , "Import module Control.Monad (when)"
                                   , "Import module Data.Maybe (fromMaybe)"
                                   ]

      contents <- executeAllCodeActions doc wantedCodeActionTitles
      liftIO $ Set.fromList (T.lines contents) `shouldBe`
        Set.fromList
          [ "import System.IO (stdout, hPutStrLn)"
          , "import Control.Monad (when)"
          , "import Data.Maybe (fromMaybe)"
          , "-- | Main entry point to the program"
          , "main :: IO ()"
          , "main ="
          , "    when True"
          , "        $ hPutStrLn stdout"
          , "        $ fromMaybe \"Good night, World!\" (Just \"Hello, World!\")"
          ]
    it "respects format config" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportBrittany.hs" "haskell"
      _ <- waitForDiagnosticsSource "bios"

      let config = def { formatOnImportOn = False, formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      actionsOrCommands <- getAllCodeActions doc
      let action:_ = map fromAction actionsOrCommands
      executeCodeAction action

      contents <- getDocumentEdit doc
      liftIO $ do
        let [l1, l2, l3, l4] = T.lines contents
        l1 `shouldBe` "import qualified Data.Maybe"
        l2 `shouldBe` "import Control.Monad"
        l3 `shouldBe` "main :: IO ()"
        l4 `shouldBe` "main = when True $ putStrLn \"hello\""

    it "import-list respects format config" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportBrittany.hs" "haskell"
      _ <- waitForDiagnosticsSource "bios"

      let config = def { formatOnImportOn = False, formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      actionsOrCommands <- getAllCodeActions doc
      let _:action:_ = map fromAction actionsOrCommands
      executeCodeAction action

      contents <- getDocumentEdit doc
      liftIO $ do
        let [l1, l2, l3, l4] = T.lines contents
        l1 `shouldBe` "import qualified Data.Maybe"
        l2 `shouldBe` "import Control.Monad (when)"
        l3 `shouldBe` "main :: IO ()"
        l4 `shouldBe` "main = when True $ putStrLn \"hello\""

    it "complex import-list" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportListElaborate.hs" "haskell"
      _ <- waitForDiagnosticsSource "bios"

      let config = def { formatOnImportOn = True, formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      let wantedCodeActionTitles = [ "Import module System.IO (hPutStrLn)"
                                   , "Import module Control.Monad (when)"
                                   , "Import module Data.Maybe (fromMaybe)"
                                   , "Import module Data.Function (($))"
                                   , "Import module Data.Maybe (Maybe (Just))"
                                   , "Import module Prelude (Bool (..))"
                                   , "Import module System.IO (stderr)"
                                   ]

      contents <- executeAllCodeActions doc wantedCodeActionTitles

      liftIO $
        T.lines contents `shouldBe` e4

    it "complex import-list respects format config" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "CodeActionImportListElaborate.hs" "haskell"
      _ <- waitForDiagnosticsSource "bios"

      let config = def { formatOnImportOn = False, formattingProvider = formatterName }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      let wantedCodeActionTitles = [ "Import module System.IO (hPutStrLn)"
                                   , "Import module Control.Monad (when)"
                                   , "Import module Data.Maybe (fromMaybe)"
                                   , "Import module Data.Function (($))"
                                   , "Import module Data.Maybe (Maybe (Just))"
                                   , "Import module Prelude (Bool (..))"
                                   , "Import module System.IO (stderr)"
                                   ]

      contents <- executeAllCodeActions doc wantedCodeActionTitles

      liftIO $
        T.lines contents `shouldBe`
          [ "{-# LANGUAGE NoImplicitPrelude #-}"
          , "import System.IO (IO, hPutStrLn, stderr)"
          , "import Prelude (Bool(..))"
          , "import Control.Monad (when)"
          , "import Data.Function (($))"
          , "import Data.Maybe (fromMaybe, Maybe(Just))"
          , "-- | Main entry point to the program"
          , "main :: IO ()"
          , "main ="
          , "    when True"
          , "        $ hPutStrLn stderr"
          , "        $ fromMaybe \"Good night, World!\" (Just \"Hello, World!\")"
          ]

  where
    executeAllCodeActions :: TextDocumentIdentifier -> [T.Text] -> Session T.Text
    executeAllCodeActions doc names =
      foldM (\_ _ -> do
          _ <- waitForDiagnosticsSource "bios"
          executeCodeActionByName doc names
          content <- skipManyTill publishDiagnosticsNotification $ getDocumentEdit doc
          _ <- waitForDiagnosticsSource "bios"
          return content
        )
        (T.pack "")
        [ 1 .. length names ]

    executeCodeActionByName :: TextDocumentIdentifier -> [T.Text] -> Session ()
    executeCodeActionByName doc names = do
      actionsOrCommands <- getAllCodeActions doc
      let allActions = map fromAction actionsOrCommands
      let actions = filter (\actn -> actn ^. L.title `elem` names) allActions
      case actions of
        (action:_) -> executeCodeAction action
        [] ->
          error
            $  "No action found to be executed!"
            ++ "\n Actual actions titles: " ++ show (map (^. L.title) allActions)
            ++ "\n Expected actions titles: " ++ show names

-- Silence warnings
hsImportSpec formatter args =
  error $ "Not the right amount of arguments for \"hsImportSpec ("
    ++ T.unpack formatter
    ++ ")\", expected 4, got "
    ++ show (length args)
-- ---------------------------------------------------------------------

fromAction :: CAResult -> CodeAction
fromAction (CACodeAction action) = action
fromAction _ = error "Not a code action"

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) Nothing
