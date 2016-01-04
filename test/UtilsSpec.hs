{-# LANGUAGE OverloadedStrings #-}
module UtilsSpec where

import           Haskell.Ide.Engine.Utils
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
                  ParamDesc
                    { pName = "file"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    , pRequired = Required
                    }
              , ContextParam fileParam CtxRegion
              ]
            , ParamCollision "plugin1" "cmd2" "end_pos"
              [ AdditionalParam
                  ParamDesc
                    { pName = "end_pos"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    , pRequired = Required
                    }
              , ContextParam endPosParam CtxRegion
              ]
            , ParamCollision "plugin1" "cmd3" "a"
              [ AdditionalParam
                  ParamDesc
                    { pName = "a"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    , pRequired = Required
                    }
              , AdditionalParam
                  ParamDesc
                    { pName = "a"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    , pRequired = Optional
                    }
              , AdditionalParam
                  ParamDesc
                    { pName = "a"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    , pRequired = Optional
                    }
              ]
              , ParamCollision "plugin1" "cmd3" "b"
                [ AdditionalParam
                    ParamDesc
                      { pName = "b"
                      , pHelp = "shoud collide"
                      , pType = PtText
                      , pRequired = Required
                      }
                , AdditionalParam
                    ParamDesc
                      { pName = "b"
                      , pHelp = "shoud collide"
                      , pType = PtText
                      , pRequired = Required
                      }
                ]
            , ParamCollision "plugin2" "cmd1" "file"
              [ AdditionalParam
                  ParamDesc
                    { pName = "file"
                    , pHelp = "shoud collide"
                    , pType = PtText
                    , pRequired = Required
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
            "    cmdAdditionalParams = ParamDesc {pName = \"file\", pHelp = \"shoud collide\", pType = PtText, pRequired = Required}\n"++
            "    cmdContexts = [CtxRegion]: ParamDesc {pName = \"file\", pHelp = \"a file name\", pType = PtFile, pRequired = Required}\n"++
            "In \"plugin1\":\"cmd2\" the parameter \"end_pos\" is defined in:\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"end_pos\", pHelp = \"shoud collide\", pType = PtText, pRequired = Required}\n"++
            "    cmdContexts = [CtxRegion]: ParamDesc {pName = \"end_pos\", pHelp = \"end line and col\", pType = PtPos, pRequired = Required}\n"++
            "In \"plugin1\":\"cmd3\" the parameter \"a\" is defined in:\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"a\", pHelp = \"shoud collide\", pType = PtText, pRequired = Required}\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"a\", pHelp = \"shoud collide\", pType = PtText, pRequired = Optional}\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"a\", pHelp = \"shoud collide\", pType = PtText, pRequired = Optional}\n"++
            "In \"plugin1\":\"cmd3\" the parameter \"b\" is defined in:\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"b\", pHelp = \"shoud collide\", pType = PtText, pRequired = Required}\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"b\", pHelp = \"shoud collide\", pType = PtText, pRequired = Required}\n"++
            "In \"plugin2\":\"cmd1\" the parameter \"file\" is defined in:\n"++
            "    cmdAdditionalParams = ParamDesc {pName = \"file\", pHelp = \"shoud collide\", pType = PtText, pRequired = Required}\n"++
            "    cmdContexts = [CtxRegion]: ParamDesc {pName = \"file\", pHelp = \"a file name\", pType = PtFile, pRequired = Required}\n"++
            "    cmdContexts = [CtxPoint]: ParamDesc {pName = \"file\", pHelp = \"a file name\", pType = PtFile, pRequired = Required}\n"
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
                              ParamDesc
                                { pName = "file"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                , pRequired = Required
                                }
                            , ParamDesc
                                { pName = "uniqueParamName1"
                                , pHelp = "shoud not collide"
                                , pType = PtText
                                , pRequired = Required
                                }
                            ]
                          }
            , cmdFunc = CmdSync $ \_ _ ->
                return (IdeResponseOk ("" :: T.Text) :: IdeResponse T.Text)
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
                              ParamDesc
                                { pName = "end_pos"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                , pRequired = Required
                                }
                            , ParamDesc
                                { pName = "uniqueParamName1"
                                , pHelp = "shoud not collide"
                                , pType = PtText
                                , pRequired = Optional
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
                              ParamDesc
                                { pName = "a"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                , pRequired = Required
                                }
                            , ParamDesc
                                { pName = "a"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                , pRequired = Optional
                                }
                            , ParamDesc
                                { pName = "a"
                                , pHelp = "shoud collide"
                                , pType = PtText
                                , pRequired = Optional
                                }
                            , ParamDesc
                              { pName = "b"
                              , pHelp = "shoud collide"
                              , pType = PtText
                              , pRequired = Required
                              }
                            , ParamDesc
                              { pName = "b"
                              , pHelp = "shoud collide"
                              , pType = PtText
                              , pRequired = Required
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
                                  ParamDesc
                                    { pName = "file"
                                    , pHelp = "shoud collide"
                                    , pType = PtText
                                    , pRequired = Required
                                    }
                                , ParamDesc
                                    { pName = "uniqueParamName1"
                                    , pHelp = "shoud not collide"
                                    , pType = PtText
                                    , pRequired = Required
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
                                  ParamDesc
                                    { pName = "uniqueParamName1"
                                    , pHelp = "shoud not collide"
                                    , pType = PtText
                                    , pRequired = Required
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
           testCommand
        ]
      , pdExposedServices = []
      , pdUsedServices    = []
      , pdUIShortName     = ""
      , pdUIOverview      = ""
    })]
  where func :: CommandFunc T.Text
        func = CmdSync $ \_ _ -> return (IdeResponseOk ("" :: T.Text))
        testCommand = Command
                        { cmdDesc = CommandDesc
                                      { cmdName = "cmd1"
                                      , cmdUiDescription = "description"
                                      , cmdFileExtensions = []
                                      , cmdReturnType = ""
                                      , cmdContexts = [CtxRegion, CtxPoint] -- ["file", "start_pos", "file", "start_pos", "end_pos"]
                                      , cmdAdditionalParams =
                                        [
                                          ParamDesc
                                            { pName = "uniqueParamName1"
                                            , pHelp = "shoud not collide"
                                            , pType = PtText
                                            , pRequired = Required
                                            }
                                        , ParamDesc
                                            { pName = "uniqueParamName2"
                                            , pHelp = "shoud not collide"
                                            , pType = PtText
                                            , pRequired = Required
                                            }
                                        ]
                                      } :: UntaggedCommandDescriptor
                        , cmdFunc = func
                        }
