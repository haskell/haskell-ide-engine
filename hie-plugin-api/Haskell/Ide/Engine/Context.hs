module Haskell.Ide.Engine.Context where

import Data.Generics
import Language.Haskell.LSP.Types
import qualified GHC
import Haskell.Ide.Engine.GhcCompat (GhcPs) -- for GHC 8.2.2
import Haskell.Ide.Engine.PluginUtils

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
-- TODO: expand this with more contexts like classes or instances for
-- smarter code completion
data Context = TypeContext
             | ValueContext
  deriving (Show, Eq)

-- | Generates a map of where the context is a type and where the context is a value
-- i.e. where are the value decls and the type decls
getContext :: Position -> GHC.ParsedModule -> Maybe Context
getContext pos pm = everything join (Nothing `mkQ` go `extQ` goInline) decl
  where decl = GHC.hsmodDecls $ GHC.unLoc $ GHC.pm_parsed_source pm
        go :: GHC.LHsDecl GhcPs -> Maybe Context
        go (GHC.L (GHC.RealSrcSpan r) (GHC.SigD {}))
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        go (GHC.L (GHC.RealSrcSpan r) (GHC.ValD {}))
          | pos `isInsideRange` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing
        goInline :: GHC.LHsType GhcPs -> Maybe Context
        goInline (GHC.L (GHC.RealSrcSpan r) _)
          | pos `isInsideRange` r = Just TypeContext
          | otherwise = Nothing
        goInline _ = Nothing
        join Nothing x = x
        join (Just x) _ = Just x
        p `isInsideRange` r = sp <= p && p <= ep
          where (sp, ep) = unpackRealSrcSpan r
