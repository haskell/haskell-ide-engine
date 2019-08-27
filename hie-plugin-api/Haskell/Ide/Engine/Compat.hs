{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Haskell.Ide.Engine.Compat where

import qualified GHC
import qualified Type
import qualified TcHsSyn
import qualified TysWiredIn
import qualified Var

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


#if MIN_VERSION_ghc(8, 4, 0)
type GhcTc = GHC.GhcTc
#else
type GhcTc = GHC.Id
#endif

pattern HsOverLitType :: Type.Type -> GHC.HsExpr GhcTc
pattern HsOverLitType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.HsOverLit _ (GHC.overLitType -> t)
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.HsOverLit (GHC.overLitType -> t)
#else
    GHC.HsOverLit (GHC.overLitType -> t)
#endif

pattern HsLitType :: Type.Type -> GHC.HsExpr GhcTc
pattern HsLitType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.HsLit _ (TcHsSyn.hsLitType -> t)
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.HsLit (TcHsSyn.hsLitType -> t)
#else
    GHC.HsLit (TcHsSyn.hsLitType -> t)
#endif

pattern HsLamType :: Type.Type -> GHC.HsExpr GhcTc
pattern HsLamType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.HsLam _ ((\(GHC.MG { GHC.mg_ext = groupTy }) -> matchGroupType groupTy) -> t)
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.HsLam (\GHC.MG { GHC.mg_res_ty = res, GHC.mg_arg_tys = args } -> Type.mkFunTys args res -> t)
#else
    GHC.HsLam (\GHC.MG { GHC.mg_res_ty = res, GHC.mg_arg_tys = args } -> Type.mkFunTys args res -> t)
#endif

pattern HsLamCaseType :: Type.Type -> GHC.HsExpr GhcTc
pattern HsLamCaseType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.HsLamCase _ ((\(GHC.MG { GHC.mg_ext = groupTy }) -> matchGroupType groupTy) -> t)
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.HsLamCase (\GHC.MG { GHC.mg_res_ty = res, GHC.mg_arg_tys = args } -> Type.mkFunTys args res -> t)
#else
    GHC.HsLamCase (\GHC.MG { GHC.mg_res_ty = res, GHC.mg_arg_tys = args } -> Type.mkFunTys args res -> t)
#endif

pattern HsCaseType :: Type.Type -> GHC.HsExpr GhcTc
pattern HsCaseType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.HsCase _ _ ((\(GHC.MG { GHC.mg_ext = groupTy }) -> matchGroupType groupTy) -> t)
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.HsCase _ (\GHC.MG { GHC.mg_res_ty = res, GHC.mg_arg_tys = args } -> Type.mkFunTys args res -> t)
#else
    GHC.HsCase _ (\GHC.MG { GHC.mg_res_ty = res, GHC.mg_arg_tys = args } -> Type.mkFunTys args res -> t)
#endif

pattern ExplicitListType :: Type.Type -> GHC.HsExpr GhcTc
pattern ExplicitListType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.ExplicitList (TysWiredIn.mkListTy -> t) _ _
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.ExplicitList (TysWiredIn.mkListTy -> t) _ _
#else
    GHC.ExplicitList (TysWiredIn.mkListTy -> t) _ _
#endif

pattern ExplicitSumType :: Type.Type -> GHC.HsExpr GhcTc
pattern ExplicitSumType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.ExplicitSum (TysWiredIn.mkSumTy -> t) _ _ _
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.ExplicitSum _ _ _ (TysWiredIn.mkSumTy -> t)
#else
    GHC.ExplicitSum _ _ _ (TysWiredIn.mkSumTy -> t)
#endif


pattern HsMultiIfType :: Type.Type -> GHC.HsExpr GhcTc
pattern HsMultiIfType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.HsMultiIf t _
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.HsMultiIf t _
#else
    GHC.HsMultiIf t _
#endif

pattern FunBindType :: Type.Type -> GHC.HsBindLR GhcTc GhcTc
pattern FunBindType t <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.FunBind _ (GHC.L _ (Var.varType -> t)) _ _ _
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.FunBind (GHC.L _ (Var.varType -> t)) _ _ _ _
#else
    GHC.FunBind (GHC.L _ (Var.varType -> t)) _ _ _ _
#endif

pattern FunBindGen :: Type.Type -> GHC.MatchGroup GhcTc (GHC.LHsExpr GhcTc) -> GHC.HsBindLR GhcTc GhcTc
pattern FunBindGen t fmatches <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.FunBind _ (GHC.L _ (Var.varType -> t)) fmatches _ _
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.FunBind (GHC.L _ (Var.varType -> t)) fmatches _ _ _
#else
    GHC.FunBind (GHC.L _ (Var.varType -> t)) fmatches _ _ _
#endif

pattern AbsBinds :: GHC.LHsBinds GhcTc -> GHC.HsBindLR GhcTc GhcTc
pattern AbsBinds bs <-
#if MIN_VERSION_ghc(8, 6, 0)
    GHC.AbsBinds _ _ _ _ _ bs _
#elif MIN_VERSION_ghc(8, 4, 0)
    GHC.AbsBinds _ _ _ _ bs _
#else
    GHC.AbsBinds _ _ _ _ bs
#endif

#if MIN_VERSION_ghc(8, 6, 0)
matchGroupType :: GHC.MatchGroupTc -> GHC.Type
matchGroupType (GHC.MatchGroupTc args res) = Type.mkFunTys args res
#endif

