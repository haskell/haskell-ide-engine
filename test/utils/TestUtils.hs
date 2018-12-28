{-# LANGUAGE CPP #-}
module TestUtils
  (
    testOptions
  , cdAndDo
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
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (pack)
import           Data.Typeable
import           Data.Yaml
import qualified Data.Map as Map
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import qualified Language.Haskell.LSP.Core as Core
import           Haskell.Ide.Engine.MonadTypes
import           System.Directory
import           System.FilePath
import qualified System.Log.Logger as L

import           Test.Hspec

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

cdAndDo :: FilePath -> IO a -> IO a
cdAndDo path fn = do
  old <- getCurrentDirectory
  bracket (setCurrentDirectory path) (\_ -> setCurrentDirectory old)
          $ const fn


testCommand :: (ToJSON a, Typeable b, ToJSON b, Show b, Eq b)
            => IdePlugins -> IdeGhcM (IdeResult b) -> PluginId -> CommandName -> a -> IdeResult b -> IO ()
testCommand testPlugins act plugin cmd arg res = do
  (newApiRes, oldApiRes) <- runIGM testPlugins $ do
    new <- act
    old <- makeRequest plugin cmd arg
    return (new, old)
  newApiRes `shouldBe` res
  fmap fromDynJSON oldApiRes `shouldBe` fmap Just res

runSingleReq :: ToJSON a
             => IdePlugins -> PluginId -> CommandName -> a -> IO (IdeResult DynamicJSON)
runSingleReq testPlugins plugin com arg = runIGM testPlugins (makeRequest plugin com arg)

makeRequest :: ToJSON a => PluginId -> CommandName -> a -> IdeGhcM (IdeResult DynamicJSON)
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
