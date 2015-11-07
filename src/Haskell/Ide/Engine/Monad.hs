{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskell.Ide.Engine.Monad where

import qualified DynFlags      as GHC
import qualified GHC           as GHC
import qualified HscTypes      as GHC

import           Control.Applicative
import           Control.Exception
import           Control.Monad.State
import           Data.IORef
import           Exception
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           System.Directory

-- Monad transformer stuff
import Control.Monad.Trans.Control ( control, liftBaseOp, liftBaseOp_)

-- ---------------------------------------------------------------------

-- type IdeM a = GM.GhcModT (GM.GmOutT IO) a
newtype IdeM a = IdeM { unIdeM :: GM.GhcModT (GM.GmOutT (StateT IdeState IO)) a}
      deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadIO
               , GM.GmEnv
               , GM.GmOut
               , GM.MonadIO
               , ExceptionMonad
               )

data IdeState = IdeState
  {
    idePlugins :: Plugins
  } deriving (Show)

-- ---------------------------------------------------------------------

runIdeM :: IdeState -> IdeM a -> IO a
runIdeM initState f = do
    initializedRef <- newIORef False
    let inner' = GM.runGmOutT opts $ GM.runGhcModT opts $ do
            liftIO $ writeIORef initializedRef True
            (unIdeM f)
        inner = runStateT inner' initState
        opts = GM.defaultOptions
    ((eres, _),_s) <- inner `catch` \ex -> case ex of
        GM.GMEWrongWorkingDirectory projDir _ -> do
            -- Only switch dirs if the exception occurs during
            -- initialization. This way we don't mysteriously restart
            -- execution if the exception happens later.
            initialized <- readIORef initializedRef
            if initialized
                then throwIO ex
                else do
                    old <- getCurrentDirectory
                    bracket (setCurrentDirectory projDir)
                            (\_ -> setCurrentDirectory old)
                            (\_ -> inner)
        _ -> throwIO ex
    case eres of
        Left err -> throwIO err
        Right res -> return res

-- ---------------------------------------------------------------------

setTargets :: [Either FilePath GM.ModuleName] -> IdeM ()
setTargets targets = IdeM $ GM.runGmlT targets (return ())

-- ---------------------------------------------------------------------

instance GM.MonadIO (StateT IdeState IO) where
  liftIO = liftIO

instance MonadState IdeState IdeM where
    get   = IdeM (lift $ lift $ lift get)
    put s = IdeM (lift $ lift $ lift (put s))

instance GHC.GhcMonad IdeM where
  getSession     = IdeM $ GM.unGmlT GM.gmlGetSession
  setSession env = IdeM $ GM.unGmlT (GM.gmlSetSession env)

instance GHC.HasDynFlags IdeM where
  getDynFlags = GHC.hsc_dflags <$> GHC.getSession

instance ExceptionMonad (StateT IdeState IO) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance HasIdeState IdeM where
  getPlugins = gets idePlugins

  setTargets targets = IdeM $ GM.runGmlT (map Left targets) (return ())

-- EOF
