module TestUtils
  (
    testOptions
  , cdAndDo
  ) where

import           Control.Exception
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           System.Directory

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
