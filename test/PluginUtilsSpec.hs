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

    it "reports collisions for plugins with parameter name collisions" $ do
      fmap pdeCollisions (validatePlugins pluginsWithCollisions) `shouldBe` Just
            [
              ("plugin1",
                  [
                    ("cmd1", [("file", [ fileParam
                                       , RP
                                         { pName = "file"
                                         , pHelp = "shoud collide"
                                         , pType = PtText
                                         }
                                       ])])
                  , ("cmd2", [("end_pos", [ endPosParam
                                          , RP
                                            { pName = "end_pos"
                                            , pHelp = "shoud collide"
                                            , pType = PtText
                                            }
                                          ])])
                  , ("cmd3", [ ("a", [])
                             , ("b", [])])
                  ]
              )
            , ("plugin2",
                  [
                    ("cmd1", [("file", [])])
                  ]
              )
            ]


    it "pretty prints the error message" $ do
      fmap pdeErrorMsg (validatePlugins pluginsWithCollisions) `shouldBe` Just (
            "In plugin \"plugin1\" the command \"cmd1\" has multiple parameters named \"file\"" ++
            "\nIn plugin \"plugin1\" the command \"cmd2\" has multiple parameters named \"end_pos\"" ++
            "\nIn plugin \"plugin1\" the command \"cmd3\" has multiple parameters named \"a\", \"b\"" ++
            "\nIn plugin \"plugin2\" the command \"cmd1\" has multiple parameters named \"file\""
          )


pluginsWithCollisions :: Plugins
pluginsWithCollisions = Map.fromList [("plugin1", PluginDescriptor
    {
      pdCommands =
        [
          Command
            { cmdDesc = CommandDesc
                          { cmdName = "cmd1"
                          , cmdUiDescription = ""
                          , cmdFileExtensions = []
                          , cmdContexts = [CtxRegion, CtxPoint] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                          , cmdAdditionalParams =
                            [
                              RP
                                { pName = "file"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                }
                            , RP
                                { pName = "uniqueParamName1"
                                , pHelp = "shoud not collide"
                                , pType = PtText
                                }
                            ]
                          }
            , cmdFunc = CmdSync $ \_ _ -> return (IdeResponseOk ("" :: T.Text))
            }
        , Command
            { cmdDesc = CommandDesc
                          { cmdName = "cmd2"
                          , cmdUiDescription = ""
                          , cmdFileExtensions = []
                          , cmdContexts = [CtxRegion]
                          , cmdAdditionalParams =
                            [
                              RP
                                { pName = "end_pos"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                }
                            , OP
                                { pName = "uniqueParamName1"
                                , pHelp = "shoud not collide"
                                , pType = PtText
                                }
                            ]
                          }
            , cmdFunc = CmdSync $ \_ _ -> return (IdeResponseOk ("" :: T.Text))
            }
            , Command
                { cmdDesc = CommandDesc
                              { cmdName = "cmd3"
                              , cmdUiDescription = ""
                              , cmdFileExtensions = []
                              , cmdContexts = [] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                              , cmdAdditionalParams =
                                [
                                  RP
                                    { pName = "a"
                                    , pHelp = "shoud collide"
                                    , pType = PtText
                                    }
                                , OP
                                    { pName = "a"
                                    , pHelp = "shoud collide"
                                    , pType = PtText
                                    }
                                , OP
                                    { pName = "a"
                                    , pHelp = "shoud collide"
                                    , pType = PtText
                                    }
                                , RP
                                  { pName = "b"
                                  , pHelp = "shoud collide"
                                  , pType = PtText
                                  }
                                , RP
                                  { pName = "b"
                                  , pHelp = "shoud collide"
                                  , pType = PtText
                                  }
                                ]
                              }
                , cmdFunc = CmdSync $ \_ _ -> return (IdeResponseOk ("" :: T.Text))
                }
        ]
      , pdExposedServices = []
      , pdUsedServices    = []
    })
    , ("plugin2", PluginDescriptor
        {
          pdCommands =
            [
              Command
                { cmdDesc = CommandDesc
                              { cmdName = "cmd1"
                              , cmdUiDescription = ""
                              , cmdFileExtensions = []
                              , cmdContexts = [CtxRegion, CtxPoint] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                              , cmdAdditionalParams =
                                [
                                  RP
                                    { pName = "file"
                                    , pHelp = "shoud collide"
                                    , pType = PtText
                                    }
                                , RP
                                    { pName = "uniqueParamName1"
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
        })
    ]


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
