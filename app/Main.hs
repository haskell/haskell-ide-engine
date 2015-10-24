{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef
import           Data.Traversable
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Monad
import           Haskell.Ide.Options
import           Haskell.Ide.Plugin
import           Haskell.Ide.Types
import qualified Language.Haskell.GhcMod.LightGhc as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           Module (mkModuleName)
import           Options.Applicative.Simple
import qualified Paths_haskell_ide as Meta

main :: IO ()
main = do
    -- Version code from stack. Maybe move this stuff into optparse-simple ?
    let commitCount = $gitCommitCount
        versionString' = concat $ concat
            [ [$(simpleVersion Meta.version)]
              -- Leave out number of commits for --depth=1 clone
              -- See https://github.com/commercialhaskell/stack/issues/792
            , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                                    commitCount /= ("UNKNOWN" :: String)]
            , [" ", display buildArch]
            ]
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
    -- Parse the options and run the specified command.
    (global, run) <-
        simpleOptions
            versionString'
            "haskell-ide - Provide a common engine to power any Haskell IDE"
            ""
            (numericVersion <*> globalOptsParser)
            (do addCommand "start"
                           "Start haskell-ide"
                           startCmd
                           startCmdOpts)
    run global

startCmdOpts :: Parser [FilePath]
startCmdOpts =
    many (strArgument
        ( metavar "TARGET"
       <> help "Specify Haskell files to load"
        ))

startCmd :: [FilePath] -> GlobalOpts -> IO ()
startCmd targets opts = runIdeM $ do
    let pkgOpts
            | null (optPluginPackages opts) = []
            | otherwise =
                "-hide-all-packages" :
                concatMap (\pkg -> ["-package", pkg])
                          ("haskell-ide" : optPluginPackages opts)
    -- Load and initialize the plugins.
    liftIO $ putStrLn "Initializing plugins"
    _plugins <- GM.withLightHscEnv pkgOpts $ \hsc_env ->
        liftIO $ forM (optPluginModules opts) $ \mn -> do
            plugin <- loadPlugin hsc_env (mkModuleName mn)
            forM_ (initializeHook plugin) id
            return plugin
    liftIO $ putStrLn "Done initializing plugins"
    -- Load target filepaths specified on the CLI.
    setTargets (map Left targets)
