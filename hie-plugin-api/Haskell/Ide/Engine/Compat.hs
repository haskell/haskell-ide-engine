{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Haskell.Ide.Engine.Compat where


import qualified GHC
import qualified Type
import qualified TcHsSyn
import qualified TysWiredIn

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


#if MIN_VERSION_ghc(8, 6, 0)

pattern HsOverLitType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern HsOverLitType t <- GHC.HsOverLit _ (GHC.overLitType -> t)

pattern HsLitType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern HsLitType t <- GHC.HsLit     _ (TcHsSyn.hsLitType -> t)

pattern HsLamType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern HsLamType t <- GHC.HsLam _ ((\(GHC.MG { GHC.mg_ext = groupTy }) -> matchGroupType groupTy) -> t)

pattern HsLamCaseType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern HsLamCaseType t <- GHC.HsLamCase _ ((\(GHC.MG { GHC.mg_ext = groupTy }) -> matchGroupType groupTy) -> t)

pattern HsCaseType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern HsCaseType t <- GHC.HsCase _ _ ((\(GHC.MG { GHC.mg_ext = groupTy }) -> matchGroupType groupTy) -> t)

pattern ExplicitListType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern ExplicitListType t <- GHC.ExplicitList (TysWiredIn.mkListTy -> t) _ _

pattern ExplicitSumType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern ExplicitSumType t <- GHC.ExplicitSum (TysWiredIn.mkSumTy -> t) _ _ _

pattern HsMultiIfType :: Type.Type -> GHC.HsExpr GHC.GhcTc
pattern HsMultiIfType t <- GHC.HsMultiIf t _

matchGroupType :: GHC.MatchGroupTc -> GHC.Type
matchGroupType (GHC.MatchGroupTc args res) = Type.mkFunTys args res

#endif