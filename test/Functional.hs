{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-

Start up an actual instance of the HIE server, and interact with it.

The startup code is based on that in MainHie.hs

TODO: extract the commonality

-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import qualified Data.Map                              as Map
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import           Data.Typeable
import           GHC.Generics
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import           Language.Haskell.LSP.Types hiding (error)
import           System.Directory
import           System.FilePath
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.Engine.Plugin.ApplyRefact
import           Haskell.Ide.Engine.Plugin.Base
import           Haskell.Ide.Engine.Plugin.Example2
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.Plugin.HaRe
import           Haskell.Ide.Engine.Plugin.HieExtras

{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
-- ---------------------------------------------------------------------

plugins :: IdePlugins
plugins = pluginDescToIdePlugins
  [("applyrefact", applyRefactDescriptor)
  ,("eg2"        , example2Descriptor)
  ,("ghcmod"     , ghcmodDescriptor)
  ,("hare"       , hareDescriptor)
  ,("base"       , baseDescriptor)
  ]

startServer :: IO (TChan (PluginRequest IO),TChan LogVal)
startServer = do
  cin      <- atomically newTChan
  logChan  <- atomically newTChan

  cancelTVar      <- atomically $ newTVar S.empty
  wipTVar         <- atomically $ newTVar S.empty
  versionTVar     <- atomically $ newTVar Map.empty
  let dispatcherEnv = DispatcherEnv
        { cancelReqsTVar     = cancelTVar
        , wipReqsTVar        = wipTVar
        , docVersionTVar     = versionTVar
        }

  void $ forkIO $ dispatcherP cin plugins testOptions dispatcherEnv
                    (\_ _ _ -> logToChan logChan ("received an error",Left ""))
                    (\g x -> g x)
  return (cin,logChan)

-- ---------------------------------------------------------------------

type LogVal = (String,Either String DynamicJSON)

logToChan :: TChan LogVal -> LogVal -> IO ()
logToChan c t = atomically $ writeTChan c t

-- ---------------------------------------------------------------------

dispatchGhcRequest :: ToJSON a
                   => String -> Int
                   -> TChan (PluginRequest IO) -> TChan LogVal
                   -> PluginId -> CommandName -> a -> IO ()
dispatchGhcRequest ctx n cin lc plugin com arg = do
  let
    logger :: RequestCallback IO DynamicJSON
    logger x = logToChan lc (ctx, Right x)

  let req = GReq Nothing Nothing (Just (IdInt n)) logger $
        runPluginCommand plugin com (toJSON arg)
  atomically $ writeTChan cin req

dispatchIdeRequest :: (Typeable a, ToJSON a)
                   => String -> TChan (PluginRequest IO)
                   -> TChan LogVal -> LspId -> IdeM (IdeResponse a) -> IO () 
dispatchIdeRequest ctx cin lc lid f = do
  let
    logger :: (Typeable a, ToJSON a) => RequestCallback IO a
    logger x = logToChan lc (ctx, Right (toDynJSON x))

  let req = IReq lid logger f
  atomically $ writeTChan cin req

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  let logfile = "./test-functional.log"
  exists <- doesFileExist logfile
  when exists $ removeFile logfile
  withFileLogging logfile $ cdAndDo "./test/testdata"  $ hspec spec

spec :: Spec
spec = do
  describe "functional spec" functionalSpec


-- ---------------------------------------------------------------------

data Cached = Cached | NotCached deriving (Show,Eq,Generic)

-- Don't care instances via GHC.Generic
instance FromJSON Cached where
instance ToJSON   Cached where

functionalSpec :: Spec
functionalSpec = do

  (cin,logChan) <- runIO startServer
  cwd <- runIO getCurrentDirectory
  let testUri = filePathToUri $ cwd </> "FuncTest.hs"

  let
    logger :: (Typeable a, ToJSON a) => String -> RequestCallback IO a
    logger ctx x = logToChan logChan (ctx, Right (toDynJSON x))

    hreq idVal doc = IReq idVal (logger ("IReq " ++ (show idVal))) $ do
      pluginGetFileResponse ("Req:" <> (T.pack $ show idVal)) doc $ \fp -> do
        liftIO $ putStrLn $ "hreq:" ++ show idVal
        cached <- isCached fp
        if cached
          then return (IdeResponseOk Cached)
          else return (IdeResponseOk NotCached)

    unpackRes (r,Right md) = (r, fromDynJSON md)
    
  
  describe "consecutive plugin commands" $ do
    it "defers responses until module is loaded" $ do

      -- Returns immediately, no cached value
      atomically $ writeTChan cin $ hreq (IdInt 0) testUri
      hr1 <- atomically $ readTChan logChan
      unpackRes hr1 `shouldBe` ("IReq IdInt 0",Just NotCached)

      let req = IReq (IdInt 1) (logger "req1") $ getSymbols testUri
      atomically $ writeTChan cin req

      rrr <- atomically $ tryReadTChan logChan
      (show rrr) `shouldBe` "Nothing"

      -- need to typecheck the module to trigger deferred response
      dispatchGhcRequest "req2" 2 cin logChan "ghcmod" "check" (toJSON testUri)

      ("req1",Right res) <- atomically $ readTChan logChan

      let Just ss = fromDynJSON res :: Maybe [SymbolInformation]
      head ss `shouldBe`
                  SymbolInformation
                     { _name          = "main"
                     , _kind          = SkFunction
                     , _location      = Location
                       { _uri   = testUri
                       , _range = Range
                         { _start = Position {_line = 2, _character = 0}
                         , _end   = Position {_line = 2, _character = 4}
                         }
                       }
                     , _containerName = Nothing
                     }

      -- pick up the diagnostics ...
      ("req2",Right res) <- atomically $ readTChan logChan

      rrr <- atomically $ tryReadTChan logChan
      (show rrr) `shouldBe` "Nothing"

      -- Returns immediately, there is a cached value
      atomically $ writeTChan cin $ hreq (IdInt 3) testUri
      hr2 <- atomically $ readTChan logChan
      unpackRes hr1 `shouldBe` ("IReq IdInt 3",Just Cached)
      -- unpackRes hr1 `shouldBe` ("IReq IdInt 0",Just NotCached)

    it "instantly responds to deferred requests if cache is available" $ do
      -- deferred responses should return something now immediately
      -- as long as the above test ran before
      references <- dispatchIdeRequest "references" cin logChan (IdInt 4)
        $ getReferencesInDoc testUri (Position 7 0)
      "references" `shouldBe` "update"
  {-
      references
        `shouldBe` [ DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 5, _character = 6}
                       , _end   = Position {_line = 5, _character = 8}
                       }
                     , _kind  = Just HkRead
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 5, _character = 6}
                       , _end   = Position {_line = 5, _character = 8}
                       }
                     , _kind  = Just HkRead
                     }
                   ]
  -}

    it "returns hints as diagnostics" $ do

      r1 <- dispatchGhcRequest "r1" 5 cin logChan "applyrefact" "lint" testUri
      {-
      fromDynJSON r1
        `shouldBe` (Just $ PublishDiagnosticsParams
                     { _uri         = filePathToUri $ cwd </> "FuncTest.hs"
                     , _diagnostics = List
                       [ Diagnostic
                           (Range (Position 9 6) (Position 10 18))
                           (Just DsInfo)
                           (Just "Redundant do")
                           (Just "hlint")
                           "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
                           Nothing
                       ]
                     }
                   )
         -}

      let req3 = HP testUri (toPos (8, 1))
      r3 <- dispatchGhcRequest "r3" 6 cin logChan "hare" "demote" req3
  {-
      fromDynJSON r3 `shouldBe` Just
        (WorkspaceEdit
          ( Just
          $ H.singleton (filePathToUri $ cwd </> "FuncTest.hs")
          $ List
              [ TextEdit (Range (Position 6 0) (Position 7 6))
                         "  where\n    bb = 5"
              ]
          )
          Nothing
        )
-}
      "r3" `shouldBe` "update"

