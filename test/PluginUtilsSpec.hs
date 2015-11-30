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
            [ ParamCollision "plugin1" "cmd1" "file"
              [ AdditionalParam
                  RP
                    { pName = "file"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    }
              , ContextParam fileParam CtxRegion
              ]
            , ParamCollision "plugin1" "cmd2" "end_pos"
              [ AdditionalParam
                  RP
                    { pName = "end_pos"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    }
              , ContextParam endPosParam CtxRegion
              ]
            , ParamCollision "plugin1" "cmd3" "a"
              [ AdditionalParam
                  RP
                    { pName = "a"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    }
              , AdditionalParam
                  OP
                    { pName = "a"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    }
              , AdditionalParam
                  OP
                    { pName = "a"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    }
              ]
              , ParamCollision "plugin1" "cmd3" "b"
                [ AdditionalParam
                    RP
                      { pName = "b"
                      , pHelp = "shoud collide"
                      , pType = PtText
                      }
                , AdditionalParam
                    RP
                      { pName = "b"
                      , pHelp = "shoud collide"
                      , pType = PtText
                      }
                ]
            , ParamCollision "plugin2" "cmd1" "file"
              [ AdditionalParam
                  RP
                    { pName = "file"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    }
              , ContextParam fileParam CtxRegion
              , ContextParam fileParam CtxPoint
              ]
            ]

    it "pretty prints the error message" $ do
      fmap pdeErrorMsg (validatePlugins pluginsWithCollisions) `shouldBe` Just (
            "Error: Parameter name collision in plugin description\n"++
            "Parameter names must be unique for each command. The following collisions were found:\n" ++
            "In \"plugin1\":\"cmd1\" the parameter \"file\" is defined in:\n" ++
            "    cmdAdditionalParams = RP {pName = \"file\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "    cmdContexts = [CtxRegion]: RP {pName = \"file\", pHelp = \"a file name\", pType = PtFile}\n"++
            "In \"plugin1\":\"cmd2\" the parameter \"end_pos\" is defined in:\n"++
            "    cmdAdditionalParams = RP {pName = \"end_pos\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "    cmdContexts = [CtxRegion]: RP {pName = \"end_pos\", pHelp = \"end line and col\", pType = PtPos}\n"++
            "In \"plugin1\":\"cmd3\" the parameter \"a\" is defined in:\n"++
            "    cmdAdditionalParams = RP {pName = \"a\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "    cmdAdditionalParams = OP {pName = \"a\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "    cmdAdditionalParams = OP {pName = \"a\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "In \"plugin1\":\"cmd3\" the parameter \"b\" is defined in:\n"++
            "    cmdAdditionalParams = RP {pName = \"b\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "    cmdAdditionalParams = RP {pName = \"b\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "In \"plugin2\":\"cmd1\" the parameter \"file\" is defined in:\n"++
            "    cmdAdditionalParams = RP {pName = \"file\", pHelp = \"shoud collide\", pType = PtText}\n"++
            "    cmdContexts = [CtxRegion]: RP {pName = \"file\", pHelp = \"a file name\", pType = PtFile}\n"++
            "    cmdContexts = [CtxPoint]: RP {pName = \"file\", pHelp = \"a file name\", pType = PtFile}\n"
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
                          , cmdReturnType = ""
                          , cmdContexts = [CtxRegion] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
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
                          , cmdReturnType = ""
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
                              , cmdReturnType = ""
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
      , pdUIShortName     = ""
      , pdUIOverview      = ""
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
                              , cmdReturnType = ""
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
                            , cmdReturnType = ""
                            , cmdContexts = [] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                            , cmdAdditionalParams =
                              [
                                RP
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
          , pdUIShortName     = ""
          , pdUIOverview      = ""
        })
    , ("plugin3", PluginDescriptor
        {
          pdCommands =
            [
              Command
                { cmdDesc = CommandDesc
                              { cmdName = "cmd1"
                              , cmdUiDescription = ""
                              , cmdFileExtensions = []
                              , cmdReturnType = ""
                              , cmdContexts = [CtxRegion, CtxPoint] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                              , cmdAdditionalParams = []
                              }
                , cmdFunc = CmdSync $ \_ _ -> return (IdeResponseOk ("" :: T.Text))
                }
            ]
          , pdExposedServices = []
          , pdUsedServices    = []
          , pdUIShortName     = ""
          , pdUIOverview      = ""
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
                          , cmdReturnType = ""
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
      , pdUIShortName     = ""
      , pdUIOverview      = ""
    })]
