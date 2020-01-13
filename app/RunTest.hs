{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module RunTest
  ( findAllSourceFiles
  , compileTarget
  , runServer
  , prettyPrintDiags
  )
where

import           GhcMonad
import qualified GHC
import           Control.Monad
import qualified Control.Concurrent.STM        as STM
import           Data.List                      ( isPrefixOf )
import qualified Data.Text                     as T
import qualified Data.Map                      as Map
import           Data.Default
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , canonicalizePath
                                                , doesFileExist
                                                )
import           System.FilePath
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.Types
import           Haskell.Ide.Engine.PluginsIdeMonads
                                         hiding ( withIndefiniteProgress
                                                , withProgress
                                                )
import           Haskell.Ide.Engine.GhcModuleCache
import qualified Haskell.Ide.Engine.ModuleCache
                                               as MC
import qualified Haskell.Ide.Engine.Ghc        as Ghc

findAllSourceFiles :: FilePath -> IO [FilePath]
findAllSourceFiles fp = do
  absFp <- canonicalizePath fp
  isDir <- doesDirectoryExist absFp
  if isDir
    then findFilesRecursively
      isHaskellSource
      (\path -> any (\p -> p path) [isHidden, isSpecialDir])
      absFp
    else filterM doesFileExist [absFp]
 where
  isHaskellSource = (== ".hs") . takeExtension
  isHidden        = ("." `isPrefixOf`) . takeFileName
  isSpecialDir    = (== "dist-newstyle") . takeFileName

findFilesRecursively
  :: (FilePath -> Bool) -> (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFilesRecursively p exclude dir = do
  dirContents' <- listDirectory dir
  let dirContents = map (dir </>) dirContents'

  files <- forM dirContents $ \fp -> do
    isDirectory <- doesDirectoryExist fp
    if isDirectory
      then if not $ exclude fp
        then findFilesRecursively p exclude fp
        else return []
      else if p fp then return [fp] else return []

  return $ concat files


-- ---------------------------------------------------------------------

compileTarget
  :: GHC.DynFlags
  -> FilePath
  -> IdeGhcM (IdeResult (Ghc.Diagnostics, Ghc.AdditionalErrs))
compileTarget dynFlags fp = do
  let pubDiags _ _ _ = return ()
  let defAction = return (mempty, mempty)
  let action    = Ghc.setTypecheckedModule (filePathToUri fp)
  actionResult <- MC.runActionWithContext pubDiags
                                          dynFlags
                                          (Just fp)
                                          defAction
                                          action
  return $ join actionResult

-- ---------------------------------------------------------------------

runServer
  :: Maybe FilePath
  -> IdePlugins
  -> [FilePath]
  -> IO [(FilePath, IdeResult (Ghc.Diagnostics, Ghc.AdditionalErrs))]
runServer mlibdir ideplugins targets = do
  let initialState = IdeState emptyModuleCache Map.empty Map.empty Nothing
  stateVar <- STM.newTVarIO initialState

  runIdeGhcM mlibdir ideplugins dummyLspFuncs stateVar $ do
    dynFlags <- getSessionDynFlags
    mapM (\fp -> (fp, ) <$> compileTarget dynFlags fp) targets

-- ---------------------------------------------------------------------

prettyPrintDiags
  :: FilePath -> IdeResult (Ghc.Diagnostics, Ghc.AdditionalErrs) -> T.Text
prettyPrintDiags fp diags = T.pack fp <> ": " <> case diags of
  IdeResultFail IdeError { ideMessage } -> "FAILED\n\t" <> ideMessage
  IdeResultOk (_diags, errs) ->
    if null errs then "OK" else T.unlines (map (T.append "\t") errs)

-- ---------------------------------------------------------------------

dummyLspFuncs :: Default a => LspFuncs a
dummyLspFuncs = LspFuncs
  { clientCapabilities           = def
  , config                       = return (Just def)
  , sendFunc                     = const (return ())
  , getVirtualFileFunc           = const (return Nothing)
  , persistVirtualFileFunc       = \uri ->
                                     return (uriToFilePath (fromNormalizedUri uri))
  , reverseFileMapFunc           = return id
  , publishDiagnosticsFunc       = mempty
  , flushDiagnosticsBySourceFunc = mempty
  , getNextReqId                 = pure (IdInt 0)
  , rootPath                     = Nothing
  , getWorkspaceFolders          = return Nothing
  , withProgress                 = \_ _ f -> f (const (return ()))
  , withIndefiniteProgress       = \_ _ f -> f
  }
