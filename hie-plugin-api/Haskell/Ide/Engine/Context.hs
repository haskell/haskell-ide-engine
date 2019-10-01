module Haskell.Ide.Engine.Context where

import Data.Generics
import Language.Haskell.LSP.Types
import qualified GHC
import Haskell.Ide.Engine.GhcCompat (GhcPs) -- for GHC 8.2.2
import Haskell.Ide.Engine.PluginUtils
import Control.Applicative ( (<|>) )

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
-- TODO: expand this with more contexts like classes or instances for
-- smarter code completion
data Context = TypeContext
             | ValueContext
             | ModuleContext String -- ^ module context with module name
             | ImportContext String -- ^ import context with module name
             | ImportListContext String -- ^ import list context with module name
             | ImportHidingContext String -- ^ import hiding context with module name
             | ExportContext -- ^ List of exported identifiers from the current module
  deriving (Show, Eq)

-- | Generates a map of where the context is a type and where the context is a value
-- i.e. where are the value decls and the type decls
getContext :: Position -> GHC.ParsedModule -> Maybe Context
getContext pos pm
  | Just (GHC.L (GHC.RealSrcSpan r) modName) <- moduleHeader
  , pos `isInsideRange` r
  = Just (ModuleContext (GHC.moduleNameString modName))

  | Just (GHC.L (GHC.RealSrcSpan r) _) <- exportList
  , pos `isInsideRange` r
  = Just ExportContext

  | Just ctx <- something (Nothing `mkQ` go `extQ` goInline) decl
  = Just ctx

  | Just ctx <- something (Nothing `mkQ` importGo) imports
  = Just ctx

  | otherwise
  = Nothing

  where decl = GHC.hsmodDecls $ GHC.unLoc $ GHC.pm_parsed_source pm
        moduleHeader = GHC.hsmodName $ GHC.unLoc $ GHC.pm_parsed_source pm
        exportList = GHC.hsmodExports $ GHC.unLoc $ GHC.pm_parsed_source pm
        imports = GHC.hsmodImports $ GHC.unLoc $ GHC.pm_parsed_source pm

        go :: GHC.LHsDecl GhcPs -> Maybe Context
        go (GHC.L (GHC.RealSrcSpan r) GHC.SigD {})
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        go (GHC.L (GHC.RealSrcSpan r) GHC.ValD {})
          | pos `isInsideRange` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing

        goInline :: GHC.LHsType GhcPs -> Maybe Context
        goInline (GHC.L (GHC.RealSrcSpan r) _)
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        goInline _ = Nothing

        p `isInsideRange` r = sp <= p && p <= ep
          where (sp, ep) = unpackRealSrcSpan r

        importGo :: GHC.LImportDecl GhcPs -> Maybe Context
        importGo (GHC.L (GHC.RealSrcSpan r) impDecl)
          | pos `isInsideRange` r
          = importInline importModuleName (GHC.ideclHiding impDecl)
          <|> Just (ImportContext importModuleName)

          | otherwise = Nothing
          where importModuleName = GHC.moduleNameString $ GHC.unLoc $ GHC.ideclName impDecl

        importGo _ = Nothing

        importInline :: String -> Maybe (Bool,  GHC.Located [GHC.LIE GhcPs]) -> Maybe Context
        importInline modName (Just (True, GHC.L (GHC.RealSrcSpan r) _))
          | pos `isInsideRange` r = Just $ ImportHidingContext modName
          | otherwise = Nothing
        importInline modName (Just (False, GHC.L (GHC.RealSrcSpan r) _))
          | pos `isInsideRange` r = Just $ ImportListContext modName
          | otherwise = Nothing
        importInline _ _ = Nothing

