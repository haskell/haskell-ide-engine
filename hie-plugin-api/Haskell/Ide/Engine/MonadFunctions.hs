{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- For MonadLogger IO instance
module Haskell.Ide.Engine.MonadFunctions
  (
  -- * Logging functions
    withStdoutLogging
  , withStderrLogging
  , withFileLogging
  , setLogLevel
  , setLogTimeFormat
  , logm
  , debugm
  , logOtherm
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Text as T
import           Prelude hiding (log)
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans.Control
import Data.IORef
import Data.Monoid
import Data.Text as T
import Data.Time
import System.IO.Unsafe
import System.Log.FastLogger

-- ---------------------------------------------------------------------

logm :: MonadIO m => String -> m ()
logm s = do
  liftIO (logInfoN $ T.pack s)
  flushLog

debugm :: MonadIO m => String -> m ()
debugm s = do
  liftIO $ logDebugN $ T.pack s
  flushLog

logOtherm :: MonadIO m => LogLevel -> T.Text -> m ()
logOtherm level s = do liftIO $ logOtherN level s
                       flushLog
-- ---------------------------------------------------------------------

-- instance MonadLoggerIO IO where
instance MonadLogger IO where
    monadLoggerLog loc src lvl msg = loggingLogger loc lvl src msg

-- ---------------------------------------------------------------------

{-
From https://hackage.haskell.org/package/logging-3.0.2/docs/src/Control-Logging.html
-}


-- |Create a new global variable to hold the system-wide log level
logLevel :: IORef LogLevel
{-# NOINLINE logLevel #-}
logLevel = unsafePerformIO $ newIORef LevelDebug

-- | Set the global verbosity level.  Messages at our higher than this level are
--   displayed. It defaults to 'LevelDebug'.
setLogLevel :: LogLevel -> IO ()
setLogLevel = atomicWriteIORef logLevel

-- |Create a new global variable to hold the system-wide log output device
logSet :: IORef LoggerSet
{-# NOINLINE logSet #-}
logSet = unsafePerformIO $
    newIORef (error "Must call withStdoutLogging or withStderrLogging")

-- |Create a new global variable to hold the system-wide log time format
logTimeFormat :: IORef String
{-# NOINLINE logTimeFormat #-}
logTimeFormat = unsafePerformIO $ newIORef "%Y %b-%d %H:%M:%S%Q"

-- | Set the global format used for log timestamps.
setLogTimeFormat :: String -> IO ()
setLogTimeFormat = atomicWriteIORef logTimeFormat

-- | This function, or 'withStderrLogging' or 'withFileLogging', must be wrapped
--   around whatever region of your application will be alive for the duration
--   that logging will be used. Typically it would be wrapped around the body of
--   'main'.
withStdoutLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withStdoutLogging f = do
    liftIO $ do
        set <- newStdoutLoggerSet defaultBufSize
        atomicWriteIORef logSet set
    f `finally` flushLog

withStderrLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withStderrLogging f = do
    liftIO $ do
        set <- newStderrLoggerSet defaultBufSize
        atomicWriteIORef logSet set
    f `finally` flushLog

withFileLogging :: (MonadBaseControl IO m, MonadIO m) => FilePath -> m a -> m a
withFileLogging path f = do
    liftIO $ do
        set <- newFileLoggerSet defaultBufSize path
        atomicWriteIORef logSet set
    f `finally` flushLog

-- | Flush all collected logging messages.  This is automatically called by
--   'withStdoutLogging', 'withStderrLogging' and 'withFileLogging' when those
--   blocks are exited by whatever means.
flushLog :: MonadIO m => m ()
flushLog = liftIO $ do
    set <- readIORef logSet
    flushLogStr set

-- ---------------------------------------------------------------------
-- Taken from Control.Monad.Logger source


-- NOTE: general principle: a log line should not have more than one "\n" in it,
-- else grepping the log becomes impossible.
loggingLogger :: ToLogStr msg => Loc -> LogLevel -> LogSource -> msg -> IO ()
loggingLogger !loc !lvl !src str = do
    maxLvl <- readIORef logLevel
    when (lvl >= maxLvl) $ do
        let willLog = True
        when willLog $ do
            now <- getCurrentTime
            fmt <- readIORef logTimeFormat
            let stamp = formatTime defaultTimeLocale fmt now
            set <- readIORef logSet
            pushLogStr set
                $ toLogStr (stamp ++ " " ++ renderLevel lvl
                                  ++ " " ++ renderSource src)
                <> toLogStr str
                <> toLogStr locStr
                <> toLogStr (pack "\n")
  where
    renderSource :: Text -> String
    renderSource txt
        | T.null txt = ""
        | otherwise  = unpack txt ++ ": "

    renderLevel LevelDebug = "[DEBUG]"
    renderLevel LevelInfo  = "[INFO]"
    renderLevel LevelWarn  = "[WARN]"
    renderLevel LevelError = "[ERROR]"
    renderLevel (LevelOther txt) = "[" ++ unpack txt ++ "]"

    locStr =
      if isDefaultLoc loc
        then ""
        else (" @(" ++ fileLocStr ++ ")" )

    -- taken from file-location package
    -- turn the TH Loc loaction information into a human readable string
    -- leaving out the loc_end parameter
    fileLocStr = (loc_package loc) ++ ':' : (loc_module loc) ++
      ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False
