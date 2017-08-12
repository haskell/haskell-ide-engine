module TestUtils
  (
    testOptions
  , cdAndDo
  , withTestLogging
  , withFileLogging
  ) where

import           Control.Exception
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import qualified Language.Haskell.LSP.Core as Core
import           System.Directory
import qualified System.Log.Logger as L


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
  r <- bracket (setCurrentDirectory path) (\_ -> setCurrentDirectory old)
          $ \_ -> fn
  return r

withTestLogging :: IO a -> IO a
withTestLogging = withFileLogging "./test-main.log"



withFileLogging :: FilePath -> IO a -> IO a
withFileLogging filePath f = do
  Core.setupLogger (Just filePath) ["hie"] L.DEBUG
  f
