{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Config (cProjectVersion)

import Control.Exception (Exception, Handler(..), ErrorCall(..))
import qualified Control.Exception as E
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)

import HIE.Bios
import HIE.Bios.Types
import HIE.Bios.Lang
import HIE.Bios.Flag
import HIE.Bios.Check
import HIE.Bios.Debug
import Paths_hie_bios

----------------------------------------------------------------

progVersion :: String
progVersion = "biosc version " ++ showVersion version ++ " compiled by GHC " ++ cProjectVersion ++ "\n"

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =    progVersion
        ++ "Usage:\n"
        ++ "\t biosc check" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t biosc version\n"
        ++ "\t biosc help\n"

----------------------------------------------------------------

data HhpcError = SafeList
               | TooManyArguments String
               | NoSuchCommand String
               | CmdArg [String]
               | FileNotExist String deriving (Show, Typeable)

instance Exception HhpcError

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
    hSetEncoding stdout utf8
    args <- getArgs
    cradle <- getCurrentDirectory >>= findCradle
    let cmdArg0 = args !. 0
        remainingArgs = tail args
        opt = defaultOptions
    res <- case cmdArg0 of
      "lang"    -> listLanguages opt
      "flag"    -> listFlags opt
      "check"   -> checkSyntax opt cradle remainingArgs
      "expand"  -> expandTemplate opt cradle remainingArgs
      "debug"   -> debugInfo opt cradle
      "root"    -> rootInfo opt cradle
      "version" -> return progVersion
      cmd       -> E.throw (NoSuchCommand cmd)
    putStr res
  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler e = handler e >> exitFailure
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: HhpcError -> IO ()
    handler2 SafeList = return ()
    handler2 (TooManyArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
    handler2 (FileNotExist file) = do
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
    xs !. idx
      | length xs <= idx = E.throw SafeList
      | otherwise = xs !! idx
