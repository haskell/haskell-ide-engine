{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import qualified Data.Map as Map
import qualified Data.Set as S
import           Data.Typeable
import qualified Data.Text as T
import           Data.Default
import           GHC.Generics
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import           Language.Haskell.LSP.Types hiding (error)
import           TestUtils
import           System.Directory
import           System.FilePath

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

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "main-dispatcher.log" $ do
    hspec newPluginSpec
    hspec funcSpec

-- main :: IO ()
-- main = do
--   summary <- withFile "results.xml" WriteMode $ \h -> do
--     let c = defaultConfig
--           { configFormatter = xmlFormatter
--           , configHandle = h
--           }
--     hspecWith c Spec.spec
--   unless (summaryFailures summary == 0) $
--     exitFailure

-- ---------------------------------------------------------------------

plugins :: IdePlugins
plugins = mkIdePlugins
  [ applyRefactDescriptor
  , example2Descriptor
  , ghcmodDescriptor
  , hareDescriptor
  , baseDescriptor
  ]

startServer :: IO (TChan (PluginRequest IO), TChan LogVal, ThreadId)
startServer = do

  cin      <- newTChanIO
  logChan  <- newTChanIO
    
  cancelTVar      <- newTVarIO S.empty
  wipTVar         <- newTVarIO S.empty
  versionTVar     <- newTVarIO Map.empty
  let dispatcherEnv = DispatcherEnv
        { cancelReqsTVar     = cancelTVar
        , wipReqsTVar        = wipTVar
        , docVersionTVar     = versionTVar
        }

  dispatcher <- forkIO $
    dispatcherP cin plugins testOptions dispatcherEnv
    (\lid errCode e -> logToChan logChan ("received an error", Left (lid, errCode, e)))
    (\g x -> g x)
    def

  return (cin, logChan, dispatcher)

-- ---------------------------------------------------------------------

type LogVal = (String, Either (LspId, ErrorCode, T.Text) DynamicJSON)

logToChan :: TChan LogVal -> LogVal -> IO ()
logToChan c t = atomically $ writeTChan c t

-- ---------------------------------------------------------------------

dispatchGhcRequest :: ToJSON a
                   => TrackingNumber -> String -> Int
                   -> TChan (PluginRequest IO) -> TChan LogVal
                   -> CommandId -> a -> IO ()
dispatchGhcRequest tn ctx n cin lc cmdId arg = do
  let
    logger :: RequestCallback IO DynamicJSON
    logger x = logToChan lc (ctx, Right x)

  let req = GReq tn Nothing Nothing (Just (IdInt n)) logger $
        runPluginCommand cmdId (toJSON arg)
  atomically $ writeTChan cin req

dispatchIdeRequest :: (Typeable a, ToJSON a)
                   => TrackingNumber -> String -> TChan (PluginRequest IO)
                   -> TChan LogVal -> LspId -> IdeM (IdeResponse a) -> IO ()
dispatchIdeRequest tn ctx cin lc lid f = do
  let
    logger :: (Typeable a, ToJSON a) => RequestCallback IO a
    logger x = logToChan lc (ctx, Right (toDynJSON x))

  let req = IReq tn lid logger f
  atomically $ writeTChan cin req

-- ---------------------------------------------------------------------

data Cached = Cached | NotCached deriving (Show,Eq,Generic)

-- Don't care instances via GHC.Generic
instance FromJSON Cached where
instance ToJSON   Cached where

-- ---------------------------------------------------------------------

newPluginSpec :: Spec
newPluginSpec = do
  describe "New plugin dispatcher operation" $
    it "dispatches response correctly" $ do
      inChan <- atomically newTChan
      outChan <- atomically newTChan
      cancelTVar <- newTVarIO S.empty
      wipTVar <- newTVarIO S.empty
      versionTVar <- newTVarIO $ Map.singleton (filePathToUri "test") 3
      let req1 = GReq 1 Nothing Nothing                          (Just $ IdInt 1) (atomically . writeTChan outChan) $ return $ IdeResultOk $ T.pack "text1"
          req2 = GReq 2 Nothing Nothing                          (Just $ IdInt 2) (atomically . writeTChan outChan) $ return $ IdeResultOk $ T.pack "text2"
          req3 = GReq 3 Nothing (Just (filePathToUri "test", 2)) Nothing            (atomically . writeTChan outChan) $ return $ IdeResultOk $ T.pack "text3"
          req4 = GReq 4 Nothing Nothing                          (Just $ IdInt 3) (atomically . writeTChan outChan) $ return $ IdeResultOk $ T.pack "text4"

      pid <- forkIO $ dispatcherP inChan
                              (mkIdePlugins [])
                              testOptions
                              (DispatcherEnv cancelTVar wipTVar versionTVar)
                              (\_ _ _ -> return ())
                              (\f x -> f x)
                              def
      atomically $ writeTChan inChan req1
      atomically $ modifyTVar cancelTVar (S.insert (IdInt 2))
      atomically $ writeTChan inChan req2
      atomically $ writeTChan inChan req3
      atomically $ writeTChan inChan req4
      resp1 <- atomically $ readTChan outChan
      resp2 <- atomically $ readTChan outChan
      killThread pid
      resp1 `shouldBe` "text1"
      resp2 `shouldBe` "text4"

funcSpec :: Spec
funcSpec = describe "functional dispatch" $ do
    runIO $ setCurrentDirectory "test/testdata"
    (cin, logChan, dispatcher) <- runIO startServer

    cwd <- runIO getCurrentDirectory
    
    let testUri = filePathToUri $ cwd </> "FuncTest.hs"
        testFailUri = filePathToUri $ cwd </> "FuncTestFail.hs"

    let
      -- Model a hover request
      hoverReq tn idVal doc = dispatchIdeRequest tn ("IReq " ++ show idVal) cin logChan idVal $ do
        pluginGetFileResponse "hoverReq" doc $ \fp -> do
          cached <- isCached fp
          if cached
            then return (IdeResponseOk Cached)
            else return (IdeResponseOk NotCached)

      unpackRes (r,Right md) = (r, fromDynJSON md)
      unpackRes r            = error $ "unpackRes:" ++ show r


    it "defers responses until module is loaded" $ do

      -- Returns immediately, no cached value
      hoverReq 0 (IdInt 0) testUri

      hr0 <- atomically $ readTChan logChan
      unpackRes hr0 `shouldBe` ("IReq IdInt 0",Just NotCached)

      -- This request should be deferred, only return when the module is loaded
      dispatchIdeRequest 1 "req1" cin logChan (IdInt 1) $ getSymbols testUri

      rrr <- atomically $ tryReadTChan logChan
      show rrr `shouldBe` "Nothing"

      -- need to typecheck the module to trigger deferred response
      dispatchGhcRequest 2 "req2" 2 cin logChan (CommandId "ghcmod" "check") (toJSON testUri)

      -- And now we get the deferred response (once the module is loaded)
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

      -- followed by the diagnostics ...
      ("req2",Right res2) <- atomically $ readTChan logChan
      show res2 `shouldBe` "((Map Uri (Set Diagnostic)),[Text])"

      -- No more pending results
      rr3 <- atomically $ tryReadTChan logChan
      show rr3 `shouldBe` "Nothing"

      -- Returns immediately, there is a cached value
      hoverReq 3 (IdInt 3) testUri
      hr3 <- atomically $ readTChan logChan
      unpackRes hr3 `shouldBe` ("IReq IdInt 3",Just Cached)

    it "instantly responds to deferred requests if cache is available" $ do
      -- deferred responses should return something now immediately
      -- as long as the above test ran before
      dispatchIdeRequest 0 "references" cin logChan (IdInt 4)
        $ getReferencesInDoc testUri (Position 7 0)

      hr4 <- atomically $ readTChan logChan
      -- show hr4 `shouldBe` "hr4"
      unpackRes hr4 `shouldBe` ("references",Just
                    [ DocumentHighlight
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
                    ])

    it "returns hints as diagnostics" $ do

      dispatchGhcRequest 5 "r5" 5 cin logChan (CommandId "applyrefact" "lint") testUri

      hr5 <- atomically $ readTChan logChan
      unpackRes hr5 `shouldBe` ("r5",
              Just $ PublishDiagnosticsParams
                      { _uri         = testUri
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

      let req6 = HP testUri (toPos (8, 1))
      dispatchGhcRequest 6 "r6" 6 cin logChan (CommandId "hare" "demote") req6

      hr6 <- atomically $ readTChan logChan
      -- show hr6 `shouldBe` "hr6"
      let textEdits = List [TextEdit (Range (Position 6 0) (Position 7 6)) "  where\n    bb = 5"]
          r6uri = testUri
      unpackRes hr6 `shouldBe` ("r6",Just
        (WorkspaceEdit
          (Just $ H.singleton r6uri textEdits)
          Nothing
        ))

    it "instantly responds to failed modules with no cache" $ do

      dispatchIdeRequest 7 "req7" cin logChan (IdInt 7) $ getSymbols testFailUri

      dispatchGhcRequest 8 "req8" 8 cin logChan (CommandId "ghcmod" "check") (toJSON testFailUri)

      (_, Left symbolError) <- atomically $ readTChan logChan
      symbolError `shouldBe` (IdInt 7, Language.Haskell.LSP.Types.InternalError, "")

      ("req8", Right diags) <- atomically $ readTChan logChan
      show diags `shouldBe` "((Map Uri (Set Diagnostic)),[Text])"

      killThread dispatcher

-- ---------------------------------------------------------------------
