{-# LANGUAGE OverloadedStrings #-}
module PluginUtilsSpec where

import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.PluginDescriptor

import qualified Data.Map as Map
import qualified Data.Text as T

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PluginUtils" pluginUtilsSpec

pluginUtilsSpec :: Spec
pluginUtilsSpec = do
  describe "validatePlugins" $ do

    it "accepts plugins without parameter name collisions" $ do
      validatePlugins pluginsWithoutCollisions `shouldBe` Nothing



pluginsWithoutCollisions :: Plugins
pluginsWithoutCollisions = Map.fromList [("plugin1", PluginDescriptor
    {
      pdCommands =
        [
          Command
            { cmdDesc = CommandDesc
                          { cmdName = "cmd1"
                          , cmdUiDescription = "description"
                          , cmdFileExtensions = []
                          , cmdContexts = [CtxRegion, CtxPoint] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                          , cmdAdditionalParams =
                            [
                              RP
                                { pName = "uniqueParamName1"
                                , pHelp = "shoud not collide"
                                , pType = PtText
                                }
                            , RP
                                { pName = "uniqueParamName2"
                                , pHelp = "shoud not collide"
                                , pType = PtText
                                }
                            ]
                          }
            , cmdFunc = CmdSync $ \_ _ -> return (IdeResponseOk ("" :: T.Text))
            }
        ]
      , pdExposedServices = []
      , pdUsedServices    = []
    })]
