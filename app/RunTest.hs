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
import           GHC
import qualified Data.Text                     as T
import           System.Directory               ( doesDirectoryExist
                                                , listDirectory
                                                , canonicalizePath
                                                , getCurrentDirectory
                                                )
import           Haskell.Ide.Engine.PluginsIdeMonads
import qualified Haskell.Ide.Engine.ModuleCache
                                               as MC
import qualified Haskell.Ide.Engine.Ghc        as Ghc
import           System.FilePath
import           Control.Monad
import           Data.List                      ( isPrefixOf )
import           TestUtils                      ( runIGM )

findAllSourceFiles :: FilePath -> IO [FilePath]
findAllSourceFiles dir = do
  absDir <- canonicalizePath dir
  findFilesRecursively isHaskellSource
                       (\fp -> any (\p -> p fp) [isHidden, isSpecialDir])
                       absDir
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
  :: DynFlags
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
  :: IdePlugins
  -> [FilePath]
  -> IO [(FilePath, IdeResult (Ghc.Diagnostics, Ghc.AdditionalErrs))]
runServer ideplugins targets = do
  cwd <- getCurrentDirectory
  runIGM ideplugins (cwd </> "File.hs") $ do
    dynFlags <- getSessionDynFlags
    mapM (\fp -> (fp, ) <$> compileTarget dynFlags fp) targets

-- ---------------------------------------------------------------------

prettyPrintDiags
  :: FilePath -> IdeResult (Ghc.Diagnostics, Ghc.AdditionalErrs) -> T.Text
prettyPrintDiags fp diags =
    T.pack fp <> ": " <>
      case diags of
        IdeResultFail IdeError { ideMessage }  -> "FAILED\n\t" <> ideMessage
        IdeResultOk (_diags, errs) ->
          if null errs
            then "OK"
            else T.unlines (map (T.append "\t") errs)
