module Haskell.Ide.Engine.Utils
  (
    getParams
  ) where

import           Data.Aeson
import           Data.List
import qualified Data.Text as T
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Data.Map as Map
import           Prelude hiding (log)

-- ---------------------------------------------------------------------

-- TODO: should this be in the haskell-ide-plugins-api

-- |If all the listed params are present in the request resturn their values,
-- else return an error message.
getParams :: [ParamId] -> IdeRequest -> Either IdeResponse [ParamVal]
getParams params req = mapEithers checkOne params
  where
    checkOne :: ParamId -> Either IdeResponse ParamVal
    checkOne param = case Map.lookup param (ideParams req) of
      Nothing -> Left $ IdeResponseFail (String $ T.pack $ "need `" ++ param ++ "` parameter")
      Just v  -> Right v



-- ---------------------------------------------------------------------
-- courtesy of http://stackoverflow.com/questions/19891061/mapeithers-function-in-haskell
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []
