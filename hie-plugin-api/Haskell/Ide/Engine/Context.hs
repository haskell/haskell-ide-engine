module Haskell.Ide.Engine.Context where

import Data.Generics
import Language.Haskell.LSP.Types
import GHC
import qualified GhcModCore as GM (GhcPs) -- for GHC 8.2.2
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
getContext :: Position -> ParsedModule -> Maybe Context
getContext pos pm
  | Just (L (RealSrcSpan r) modName) <- moduleHeader
  , pos `isInsideRange` r
  = Just (ModuleContext (moduleNameString modName))

  | Just (L (RealSrcSpan r) _) <- exportList
  , pos `isInsideRange` r
  = Just ExportContext

  | Just ctx <- something (Nothing `mkQ` go `extQ` goInline) decl
  = Just ctx

  | Just ctx <- something (Nothing `mkQ` importGo) imports
  = Just ctx

  | otherwise
  = Nothing

  where decl = hsmodDecls $ unLoc $ pm_parsed_source pm
        moduleHeader = hsmodName $ unLoc $ pm_parsed_source pm
        exportList = hsmodExports $ unLoc $ pm_parsed_source pm
        imports = hsmodImports $ unLoc $ pm_parsed_source pm

        go :: LHsDecl GM.GhcPs -> Maybe Context
        go (L (RealSrcSpan r) SigD {})
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        go (L (GHC.RealSrcSpan r) GHC.ValD {})
          | pos `isInsideRange` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing

        goInline :: GHC.LHsType GM.GhcPs -> Maybe Context
        goInline (GHC.L (GHC.RealSrcSpan r) _)
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        goInline _ = Nothing

        p `isInsideRange` r = sp <= p && p <= ep
          where (sp, ep) = unpackRealSrcSpan r

        importGo :: GHC.LImportDecl GM.GhcPs -> Maybe Context
        importGo (L (RealSrcSpan r) impDecl)
          | pos `isInsideRange` r
          = importInline importModuleName (ideclHiding impDecl)
          <|> Just (ImportContext importModuleName)

          | otherwise = Nothing
          where importModuleName = moduleNameString $ unLoc $ ideclName impDecl

        importGo _ = Nothing

        importInline :: String -> Maybe (Bool,  GHC.Located [GHC.LIE GM.GhcPs]) -> Maybe Context
        importInline modName (Just (True, L (RealSrcSpan r) _))
          | pos `isInsideRange` r = Just $ ImportHidingContext modName
          | otherwise = Nothing
        importInline modName (Just (False, L (RealSrcSpan r) _))
          | pos `isInsideRange` r = Just $ ImportListContext modName
          | otherwise = Nothing
        importInline _ _ = Nothing

