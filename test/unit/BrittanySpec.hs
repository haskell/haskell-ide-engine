{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module BrittanySpec where

import           Haskell.Ide.Engine.PluginDescriptor
import           Language.Haskell.LSP.Types
import           System.Directory
import           Haskell.Ide.Engine.Plugin.Brittany
import           Haskell.Ide.Engine.MonadTypes
import           TestUtils
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "brittany plugin" brittanySpec

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("brittany", brittanyDescriptor)]

brittanySpec :: Spec
brittanySpec = describe "brittany plugin commands" $ do
  lfFile <- runIO $ filePathToUri <$> makeAbsolute
    "./test/testdata/BrittanyLF.hs"
  crlfFile <- runIO $ filePathToUri <$> makeAbsolute
    "./test/testdata/BrittanyCRLF.hs"

  it "formats a document with LF endings" $ do
    let
      act = brittanyCmd 4 lfFile Nothing
      arg = FormatParams 4 lfFile Nothing
      res = return
        [ TextEdit
            { _range = Range
              { _start = Position {_line = 0, _character = 0}
              , _end   = Position {_line = 3, _character = 0}
              }
            , _newText = "foo :: Int -> String -> IO ()\nfoo x y = do\n    print x\n    return 42\n"
            }
        ]
    testCommand testPlugins act "brittany" "format" arg res

  it "formats a document with CRLF endings" $ do
    let
      act = brittanyCmd 4 crlfFile Nothing
      arg = FormatParams 4 crlfFile Nothing
      res = return
        [ TextEdit
            { _range = Range
              { _start = Position {_line = 0, _character = 0}
              , _end   = Position {_line = 3, _character = 0}
              }
            , _newText = "foo :: Int -> String -> IO ()\nfoo x y = do\n    print x\n    return 42\n"
            }
        ]
    testCommand testPlugins act "brittany" "format" arg res

  it "formats a range with LF endings" $ do
    let r   = Range (Position 1 0) (Position 2 22)
        act = brittanyCmd 4 lfFile (Just r)
        arg = FormatParams 4 lfFile (Just r)
        res = return
          [ TextEdit
              { _range   = Range
                { _start = Position {_line = 1, _character = 0}
                , _end   = Position {_line = 3, _character = 0}
                }
              , _newText = "foo x y = do\n    print x\n    return 42\n"
              }
          ]
    testCommand testPlugins act "brittany" "format" arg res

  it "formats a range with CRLF endings" $ do
    let r   = Range (Position 1 0) (Position 2 22)
        act = brittanyCmd 4 crlfFile (Just r)
        arg = FormatParams 4 crlfFile (Just r)
        res = return
          [ TextEdit
              { _range   = Range
                { _start = Position {_line = 1, _character = 0}
                , _end   = Position {_line = 3, _character = 0}
                }
              , _newText = "foo x y = do\n    print x\n    return 42\n"
              }
          ]
    testCommand testPlugins act "brittany" "format" arg res
