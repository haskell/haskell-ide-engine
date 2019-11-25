{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.Engine.GhcUtils where

import qualified Language.Haskell.LSP.Core as Core

import qualified HscMain as G
import           Module
import           HscTypes
import           GHC
import           IOEnv   as G
import qualified Data.Text as T

import qualified HIE.Bios.Flags as BIOS (CradleError)

import           Haskell.Ide.Engine.PluginUtils (ErrorHandler(..))

-- Convert progress continuation to a messager
toMessager :: (Core.Progress -> IO ()) -> G.Messager
toMessager k _hsc_env (nk, n) _rc_reason ms =
  let prog = Core.Progress (Just (fromIntegral nk/ fromIntegral n))  (Just mod_name)
      mod_name = T.pack $ moduleNameString (moduleName (ms_mod ms))
  in k prog

{-
toMessager :: Messager
toMessager hsc_env mod_index recomp mod_summary =
    case recomp of
        MustCompile -> showMsg "Compiling " ""
        UpToDate
            | verbosity (hsc_dflags hsc_env) >= 2 -> showMsg "Skipping  " ""
            | otherwise -> return ()
        RecompBecause reason -> showMsg "Compiling " (" [" ++ reason ++ "]")
    where
        dflags = hsc_dflags hsc_env
        showMsg msg reason =
            compilationProgressMsg dflags $
            (showModuleIndex mod_index ++
            msg ++ showModMsg dflags (hscTarget dflags)
                              (recompileRequired recomp) mod_summary)
                ++ reason
-}

-- Handlers for each type of error that ghc can throw
errorHandlers :: (String -> m a) -> (HscTypes.SourceError -> m a) -> [ErrorHandler m a]
errorHandlers onGhcError onSourceError = handlers
  where
      -- ghc throws GhcException, SourceError, GhcApiError and
      -- IOEnvFailure. hie-bios throws CradleError.
      handlers =
        [ ErrorHandler $ \(ex :: IOEnvFailure) ->
            onGhcError (show ex)
        , ErrorHandler $ \(ex :: GhcApiError) ->
            onGhcError (show ex)
        , ErrorHandler $ \(ex :: SourceError) ->
            onSourceError ex
        , ErrorHandler $ \(ex :: IOError) ->
            onGhcError (show ex)
        , ErrorHandler $ \(ex :: BIOS.CradleError) ->
            onGhcError (show ex)
        , ErrorHandler $ \(ex :: GhcException) ->
            onGhcError (showGhcException ex "")
        ]
