{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Plugin.Package.Compat where

#if MIN_VERSION_Cabal(2,2,0)
#else
import Control.Lens
import Distribution.Types.GenericPackageDescription (GenericPackageDescription(GenericPackageDescription), ConfVar (..))
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Benchmark (Benchmark(..))
import Distribution.Types.Library (Library, libBuildInfo)
import Distribution.Types.TestSuite (TestSuite, testBuildInfo)
import qualified Distribution.Types.Executable as E (Executable, buildInfo)
import qualified Distribution.Types.BuildInfo as BI (BuildInfo(..))
#endif

#if MIN_VERSION_Cabal(2,2,0)
#else

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

condBenchmarks :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
condBenchmarks f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 y1) (f x8)
{-# INLINE condBenchmarks #-}

condExecutables :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] E.Executable)]
condExecutables f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 y1 x7 x8) (f x6)
{-# INLINE condExecutables #-}

condLibrary :: Lens' GenericPackageDescription (Maybe (CondTree ConfVar [Dependency] Library))
condLibrary f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 y1 x4 x5 x6 x7 x8) (f x3)
{-# INLINE condLibrary #-}

condSubLibraries :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] Library)]
condSubLibraries f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 y1 x5 x6 x7 x8) (f x4)
{-# INLINE condSubLibraries #-}

condTestSuites :: Lens' GenericPackageDescription [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)]
condTestSuites f (GenericPackageDescription x1 x2 x3 x4 x5 x6 x7 x8) = fmap (\y1 -> GenericPackageDescription x1 x2 x3 x4 x5 x6 y1 x8) (f x7)
{-# INLINE condTestSuites #-}

class HasBuildInfo a where
  buildInfo :: Lens' a BI.BuildInfo

  hsSourceDirs :: Lens' a [FilePath]
  hsSourceDirs = buildInfo . hsSourceDirs
  {-# INLINE hsSourceDirs #-}

  targetBuildDepends :: Lens' a [Dependency]
  targetBuildDepends = buildInfo . targetBuildDepends
  {-# INLINE targetBuildDepends #-}

instance HasBuildInfo BI.BuildInfo where
  buildInfo = id

  hsSourceDirs f s = fmap (\x -> s { BI.hsSourceDirs = x }) (f (BI.hsSourceDirs s))
  {-# INLINE hsSourceDirs #-}

  targetBuildDepends f s = fmap (\x -> s { BI.targetBuildDepends = x }) (f (BI.targetBuildDepends s))
  {-# INLINE targetBuildDepends #-}

instance HasBuildInfo E.Executable where
  buildInfo f l = (\x -> l { E.buildInfo = x }) <$> f (E.buildInfo l)

instance HasBuildInfo Benchmark where
  buildInfo f (Benchmark x1 x2 x3) = fmap (\y1 -> Benchmark x1 x2 y1) (f x3)

instance HasBuildInfo TestSuite where
  buildInfo f l = (\x -> l { testBuildInfo = x }) <$> f (testBuildInfo l)

instance HasBuildInfo Library where
  buildInfo f l = (\x -> l { libBuildInfo = x }) <$> f (libBuildInfo l)

#endif
