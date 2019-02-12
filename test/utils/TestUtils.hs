{-# LANGUAGE CPP, OverloadedStrings, NamedFieldPuns #-}
module TestUtils
  (
    testOptions
  , withFileLogging
  , setupStackFiles
  , testCommand
  , runSingleReq
  , makeRequest
  , runIGM
  , ghcVersion, GhcVersion(..)
  , logFilePath
  , hieCommand
  , hieCommandVomit
  , hieCommandExamplePlugin
  , getHspecFormattedConfig
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson.Types (typeMismatch)
import           Data.List (intercalate)
import           Data.Text (pack)
import           Data.Typeable
import           Data.Yaml
import qualified Data.Map as Map
import           Data.Maybe
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import qualified Language.Haskell.LSP.Core as Core
import           Haskell.Ide.Engine.MonadTypes
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified System.Log.Logger as L
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.Core.Formatters
import           Text.Blaze.Renderer.String (renderMarkup)
import           Text.Blaze.Internal

-- ---------------------------------------------------------------------

testOptions :: GM.Options
testOptions = GM.defaultOptions {
    GM.optOutput     = GM.OutputOpts {
      GM.ooptLogLevel       = GM.GmError
      -- GM.ooptLogLevel       = GM.GmVomit
    , GM.ooptStyle          = GM.PlainStyle
    , GM.ooptLineSeparator  = GM.LineSeparator "\0"
    , GM.ooptLinePrefix     = Nothing
    }

    }


testCommand :: (ToJSON a, Typeable b, ToJSON b, Show b, Eq b)
            => IdePlugins -> IdeGhcM (IdeResult b) -> PluginId -> CommandId -> a -> IdeResult b -> IO ()
testCommand testPlugins act plugin cmd arg res = do
  (newApiRes, oldApiRes) <- runIGM testPlugins $ do
    new <- act
    old <- makeRequest plugin cmd arg
    return (new, old)
  newApiRes `shouldBe` res
  fmap fromDynJSON oldApiRes `shouldBe` fmap Just res

runSingleReq :: ToJSON a
             => IdePlugins -> PluginId -> CommandId -> a -> IO (IdeResult DynamicJSON)
runSingleReq testPlugins plugin com arg = runIGM testPlugins (makeRequest plugin com arg)

makeRequest :: ToJSON a => PluginId -> CommandId -> a -> IdeGhcM (IdeResult DynamicJSON)
makeRequest plugin com arg = runPluginCommand plugin com (toJSON arg)

runIGM :: IdePlugins -> IdeGhcM a -> IO a
runIGM testPlugins f = do
  stateVar <- newTVarIO $ IdeState emptyModuleCache Map.empty Map.empty Nothing
  runIdeGhcM testOptions testPlugins Nothing stateVar f

withFileLogging :: FilePath -> IO a -> IO a
withFileLogging logFile f = do
  let logDir = "./test-logs"
      logPath = logDir </> logFile

  dirExists <- doesDirectoryExist logDir
  unless dirExists $ createDirectory logDir

  exists <- doesFileExist logPath
  when exists $ removeFile logPath

  Core.setupLogger (Just logPath) ["hie"] L.DEBUG

  f

-- ---------------------------------------------------------------------

setupStackFiles :: IO ()
setupStackFiles =
  forM_ files $ \f -> do
    resolver <- readResolver
    writeFile (f ++ "stack.yaml") $ stackFileContents resolver
    removePathForcibly (f ++ ".stack-work")

-- ---------------------------------------------------------------------

files :: [FilePath]
files =
  [  "./test/testdata/"
   , "./test/testdata/addPackageTest/cabal/"
   , "./test/testdata/addPackageTest/hpack/"
   , "./test/testdata/addPragmas/"
   , "./test/testdata/badProjects/cabal/"
   , "./test/testdata/completion/"
   , "./test/testdata/definition/"
   , "./test/testdata/gototest/"
   , "./test/testdata/redundantImportTest/"
   , "./test/testdata/wErrorTest/"
  ]

data GhcVersion
  = GHC86
  | GHC84
  | GHCPre84
  deriving (Eq,Show)

ghcVersion :: GhcVersion
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)))
ghcVersion = GHC86
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
ghcVersion = GHC84
#else
ghcVersion = GHCPre84
#endif

stackYaml :: FilePath
stackYaml =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,3,0)))
  "stack-8.6.3.yaml"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,2,0)))
  "stack-8.6.2.yaml"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)))
  "stack-8.6.1.yaml"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,4,0)))
  "stack-8.4.4.yaml"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,3,0)))
  "stack-8.4.3.yaml"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)))
  "stack-8.4.2.yaml"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
  "stack-8.2.2.yaml"
#elif __GLASGOW_HASKELL__ >= 802
  "stack-8.2.1.yaml"
#else
  "stack-8.0.2.yaml"
#endif

logFilePath :: String
logFilePath = "functional-hie-" ++ stackYaml ++ ".log"

-- | The command to execute the version of hie for the current compiler.
-- Make sure to disable the STACK_EXE and GHC_PACKAGE_PATH environment
-- variables or else it messes up -- ghc-mod.
-- We also need to unset STACK_EXE manually inside the tests if they are
-- run with `stack test`
hieCommand :: String
hieCommand = "stack exec --no-stack-exe --no-ghc-package-path --stack-yaml=" ++ stackYaml ++
             " hie -- -d -l test-logs/" ++ logFilePath

hieCommandVomit :: String
hieCommandVomit = hieCommand ++ " --vomit"

hieCommandExamplePlugin :: String
hieCommandExamplePlugin = hieCommand ++ " --example"

-- |Choose a resolver based on the current compiler, otherwise HaRe/ghc-mod will
-- not be able to load the files
readResolver :: IO String
readResolver = readResolverFrom stackYaml

newtype StackResolver = StackResolver String

instance FromJSON StackResolver where
  parseJSON (Object x) = StackResolver <$> x .: pack "resolver"
  parseJSON invalid = typeMismatch "StackResolver" invalid

readResolverFrom :: FilePath -> IO String
readResolverFrom yamlPath = do
  result <- decodeFileEither yamlPath
  case result of
    Left err -> error $ yamlPath ++ " parsing failed: " ++ show err
    Right (StackResolver res) -> return res

-- ---------------------------------------------------------------------

stackFileContents :: String -> String
stackFileContents resolver = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/Main.hs. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , "resolver: " ++ resolver
  , "packages:"
  , "- '.'"
  , "extra-deps: []"
  , "flags: {}"
  , "extra-package-dbs: []"
  ]

-- ---------------------------------------------------------------------

getHspecFormattedConfig :: String -> IO Config
getHspecFormattedConfig name = do
  -- https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables
  isCI <- isJust <$> lookupEnv "CI"

  -- Only use the xml formatter on CI since it hides console output
  if isCI
    then do
      let subdir = "test-results" </> name
      createDirectoryIfMissing True subdir

      return $ defaultConfig { configFormatter = Just xmlFormatter
                             , configOutputFile = Right $ subdir </> "results.xml"
                             }
    else return defaultConfig

-- | A Hspec formatter for CircleCI.
-- Originally from https://github.com/LeastAuthority/hspec-jenkins
xmlFormatter :: Formatter
xmlFormatter = silent {
    headerFormatter = do
      writeLine "<?xml version='1.0' encoding='UTF-8'?>"
      writeLine "<testsuite>"
  , exampleSucceeded
  , exampleFailed
  , examplePending
  , footerFormatter = writeLine "</testsuite>"
  }
  where

#if MIN_VERSION_hspec(2,5,0)
    exampleSucceeded path _ =
#else
    exampleSucceeded path =
#endif
      writeLine $ renderMarkup $ testcase path ""

#if MIN_VERSION_hspec(2,5,0)
    exampleFailed path _ err =
#else
    exampleFailed path (Left err) =
      writeLine $ renderMarkup $ testcase path $
        failure ! message (show err) $ ""
    exampleFailed path (Right err) =
#endif
      writeLine $ renderMarkup $ testcase path $
        failure ! message (reasonAsString err) $ ""

#if MIN_VERSION_hspec(2,5,0)
    examplePending path _ reason = 
#else
    examplePending path reason =
#endif
      writeLine $ renderMarkup $ testcase path $
        case reason of
          Just desc -> skipped ! message desc  $ ""
          Nothing -> skipped ""

    failure, skipped :: Markup -> Markup
    failure = customParent "failure"
    skipped = customParent "skipped"

    name, className, message :: String -> Attribute
    name = customAttribute "name" . stringValue
    className = customAttribute "classname" . stringValue
    message = customAttribute "message" . stringValue

    testcase :: Path -> Markup -> Markup
    testcase (xs,x) = customParent "testcase" ! name x ! className (intercalate "." xs)

    reasonAsString :: FailureReason -> String
    reasonAsString NoReason = "no reason given"
    reasonAsString (Reason x) = x
    reasonAsString (ExpectedButGot Nothing expected got) = "Expected " ++ expected ++ " but got " ++ got
    reasonAsString (ExpectedButGot (Just src) expected got) = src ++ " expected " ++ expected ++ " but got " ++ got
#if MIN_VERSION_hspec(2,5,0)
    reasonAsString (Error Nothing err ) = show err
    reasonAsString (Error (Just s) err) = s ++ show err
#endif

-- ---------------------------------------------------------------------

