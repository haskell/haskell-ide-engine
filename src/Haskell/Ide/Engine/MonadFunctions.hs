module Haskell.Ide.Engine.MonadFunctions
  (
  -- * Logging functions
    logm
  , debugm
  ) where


import           Control.Logging
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Prelude hiding (log)

-- ---------------------------------------------------------------------

logm :: MonadIO m => String -> m ()
logm s = liftIO $ log $ T.pack s

debugm :: MonadIO m =>String -> m ()
debugm s = liftIO $ log $ T.pack s

