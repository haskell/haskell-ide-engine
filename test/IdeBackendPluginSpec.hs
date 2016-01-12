{-# LANGUAGE OverloadedStrings #-}
module IdeBackendPluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.IdeBackend
import           System.Directory
import           System.FilePath

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ide-backend plugin" idebackendSpec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("ide-backend",untagPluginDescriptor idebackendDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "ide-backend" 1 req testChan
  r <- withStdoutLogging $ runIdeM (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

idebackendSpec :: Spec
idebackendSpec =
  do describe "ide-backend plugin commands" $
       do cwd <- runIO $ getCurrentDirectory
          it "runs the type command, incorrect params" $
            do let req =
                     IdeRequest
                       "type"
                       (Map.fromList
                          [("file"
                           ,ParamFileP
                              (T.pack $
                               cwd </> "test/testdata/FileWithWarning.hs"))])
               r <- dispatchRequest req
               r `shouldBe`
                 Just (IdeResponseFail
                         (IdeError {ideCode = MissingParameter
                                   ,ideMessage = "need `end_pos` parameter"
                                   ,ideInfo = String "end_pos"}))
          -- ---------------------------------
          it "runs the type command, correct params" $
            do let req =
                     IdeRequest
                       "type"
                       (Map.fromList
                          [("file"
                           ,ParamFileP
                              (T.pack $ cwd </> "test/testdata/HaReRename.hs"))
                          ,("start_pos",ParamPosP (5,9))
                          ,("end_pos",ParamPosP (5,9))])
               r <- dispatchRequest req
               r `shouldBe`
                 Just (IdeResponseOk
                         (H.fromList
                            ["type_info" .=
                             toJSON [TypeResult (5,9)
                                                      (5,10)
                                                      "Int"
                                          ,TypeResult (5,9)
                                                      (5,14)
                                                      "Int"]]))
