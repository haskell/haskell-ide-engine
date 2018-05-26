{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
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
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import qualified Data.Map                              as Map
import qualified Data.Set                              as S
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
  ,("hieextras"       , baseDescriptor)
  ]

startServer :: IO (TChan (PluginRequest IO))
startServer = do
  cin  <- atomically newTChan

  cancelTVar      <- atomically $ newTVar S.empty
  wipTVar         <- atomically $ newTVar S.empty
  versionTVar     <- atomically $ newTVar Map.empty
  let dispatcherEnv = DispatcherEnv
        { cancelReqsTVar     = cancelTVar
        , wipReqsTVar        = wipTVar
        , docVersionTVar     = versionTVar
        }

  void $ forkIO $ dispatcherP cin plugins testOptions dispatcherEnv (\_ _ _ -> error "received an error") (\g x -> g x)
  return cin

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "./test-functional.log" $ cdAndDo "./test/testdata"  $ hspec spec

spec :: Spec
spec = do
  describe "functional spec" functionalSpec


-- ---------------------------------------------------------------------

dispatchGhcRequest :: ToJSON a => TChan (PluginRequest IO) -> PluginId -> CommandName -> a -> IO DynamicJSON
dispatchGhcRequest cin plugin com arg = do
  mv <- newEmptyMVar
  let req = GReq Nothing Nothing Nothing (putMVar mv) $
        runPluginCommand plugin com (toJSON arg)
  atomically $ writeTChan cin req
  takeMVar mv

dispatchIdeRequest :: TChan (PluginRequest IO) -> LspId -> IdeM (IdeResponse a) -> IO a
dispatchIdeRequest cin lid f = do
  mv <- newEmptyMVar
  let req = IReq lid (putMVar mv) f
  atomically $ writeTChan cin req
  takeMVar mv

-- ---------------------------------------------------------------------

functionalSpec :: Spec
functionalSpec = do

  cin <- runIO startServer
  cwd <- runIO getCurrentDirectory
  let testUri = filePathToUri $ cwd </> "FuncTest.hs"

  describe "consecutive plugin commands" $ do
    it "defers responses until module is loaded" $ do


      reqVar <- newEmptyMVar
      let req = IReq (IdInt 0) (putMVar reqVar) $ getSymbols testUri
      atomically $ writeTChan cin req

      -- need to typecheck the module to trigger deferred response
      _   <- dispatchGhcRequest cin "ghcmod" "check" (toJSON testUri)

      res <- takeMVar reqVar

      res
        `shouldBe` [ SymbolInformation
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
                   , SymbolInformation
                     { _name          = "foo"
                     , _kind          = SkFunction
                     , _location      = Location
                       { _uri   = testUri
                       , _range = Range
                         { _start = Position {_line = 5, _character = 0}
                         , _end   = Position {_line = 5, _character = 3}
                         }
                       }
                     , _containerName = Nothing
                     }
                   , SymbolInformation
                     { _name          = "bb"
                     , _kind          = SkFunction
                     , _location      = Location
                       { _uri   = testUri
                       , _range = Range
                         { _start = Position {_line = 7, _character = 0}
                         , _end   = Position {_line = 7, _character = 2}
                         }
                       }
                     , _containerName = Nothing
                     }
                   , SymbolInformation
                     { _name          = "baz"
                     , _kind          = SkFunction
                     , _location      = Location
                       { _uri   = testUri
                       , _range = Range
                         { _start = Position {_line = 9, _character = 0}
                         , _end   = Position {_line = 9, _character = 3}
                         }
                       }
                     , _containerName = Nothing
                     }
                   , SymbolInformation
                     { _name          = "f"
                     , _kind          = SkFunction
                     , _location      = Location
                       { _uri   = testUri
                       , _range = Range
                         { _start = Position {_line = 12, _character = 0}
                         , _end   = Position {_line = 12, _character = 1}
                         }
                       }
                     , _containerName = Nothing
                     }
                   ]

    it "instantly responds to deferred requests if cache is available" $ do
      -- deferred responses should return something now immediately
      -- as long as the above test ran before
      references <- dispatchIdeRequest cin (IdInt 1)
        $ getReferencesInDoc testUri (Position 7 0)
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

    it "returns hints as diagnostics" $ do

      r1 <- dispatchGhcRequest cin "applyrefact" "lint" testUri
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

      let req3 = HP testUri (toPos (8, 1))
      r3 <- dispatchGhcRequest cin "hare" "demote" req3
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

