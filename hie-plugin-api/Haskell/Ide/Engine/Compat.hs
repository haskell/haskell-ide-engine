{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Compat where

#if MIN_VERSION_filepath(1,4,2)
#else
import Data.List
import System.FilePath
#endif

#ifdef mingw32_HOST_OS

import qualified System.Win32.Process as P (getCurrentProcessId)
getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getCurrentProcessId

#else

import qualified System.Posix.Process as P (getProcessID)
getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getProcessID

#endif

#if MIN_VERSION_filepath(1,4,2)
#else
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
