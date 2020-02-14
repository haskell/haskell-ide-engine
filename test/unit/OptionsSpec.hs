module OptionsSpec where

import Prelude hiding (unzip)
import Data.List.NonEmpty(unzip)
import Test.Hspec
import Options.Applicative
import Haskell.Ide.Engine.Options(GlobalOpts(..), RunMode(..), ProjectLoadingOpts(..), optionParser)
import System.Exit(ExitCode(..))
import Data.List(isPrefixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let defaultGlobalOptions = GlobalOpts False Nothing Nothing False Nothing False (ProjectLoadingMode $ ProjectLoadingOpts False [])
  let getParseFailure (Failure x) = Just (renderFailure x "hie")
      getParseFailure _           = Nothing
  let sut         = optionParser
  let parserInfo  = info sut mempty
  let parserPrefs = prefs mempty
  let runSut :: [String] -> ParserResult GlobalOpts 
      runSut      = execParserPure parserPrefs parserInfo
  
  describe "cmd option parsing" $ do
    describe "compiler flag" $ do
      let input  = ["--compiler"]
      let result = runSut input
      let (maybeMessage, maybeStatusCode) = unzip $ getParseFailure result

      it "should return ghc version" $
        maybeMessage `shouldSatisfy` any ("ghc" `isPrefixOf`)
      it "should return exit code 0" $
        maybeStatusCode `shouldBe` Just ExitSuccess

    describe "numeric version flag" $ do
      let input  = ["--numeric-version"]
      let result = runSut input
      let (maybeMessage, maybeStatusCode) = unzip $ getParseFailure result

      it "should return version" $
        maybeMessage `shouldBe` Just "1.1"
      it "shoud return exit code 0" $
        maybeStatusCode `shouldBe` Just ExitSuccess

    describe "not providing arguments" $ do
      let input  = []
      let result = runSut input
      let maybeGlobalOptions = getParseResult result
      
      it "should result in default options" $
        maybeGlobalOptions `shouldBe` Just defaultGlobalOptions

    describe "lsp flag" $ do
      let input  = ["--lsp"]
      let result = runSut input
      let maybeGlobalOptions = getParseResult result
      
      it "should result in default lsp options" $
        maybeGlobalOptions `shouldBe` Just (GlobalOpts False Nothing Nothing False Nothing False LspMode)
    
    describe "providing two unmatching arguments" $ do
      let input  = ["--lsp", "--dry-run"]
      let result = runSut input
      let (maybeMessage, maybeStatusCode) = unzip $ getParseFailure result
      
      it "should return expected error message" $
        maybeMessage `shouldSatisfy` any ("Invalid option `--dry-run'" `isPrefixOf`)
      it "should return error exit code 1" $
        maybeStatusCode `shouldBe` Just (ExitFailure 1)
