{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Haskell.Ide.Engine.Transport.PipesHelpers
  ( decode'
  ) where

import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Aeson as Ae
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import           Pipes
import qualified Pipes.Aeson as PAe
import           Pipes.Attoparsec (ParsingError(..))
import qualified Pipes.Parse as Pipes (Parser)

-- | Like 'parseL' from Pipes.Attoparsec but throws away input on a parse error
parseL'
    :: (Monad m)
    => Parser b                           -- ^ Attoparsec parser
    -> Pipes.Parser BS.ByteString m (Maybe (Either ParsingError (Int, b))) -- ^ Pipes parser
parseL' parser = S.StateT $ \p0 -> do
    x <- nextSkipEmpty p0
    case x of
      Left r       -> return (Nothing, return r)
      Right (a,p1) -> step (parse parser a) p1 (BS.length a)
  where
    step res p0 !len = case res of
      Fail _ c m -> return (Just (Left (ParsingError c m)), p0)
      Done a b   -> return (Just (Right (len - BS.length a, b)), yield a >> p0)
      Partial k  -> do
        x <- nextSkipEmpty p0
        case x of
          Left e -> step (k mempty) (return e) len
          Right (a,p1) -> step (k a) p1 (len + BS.length a)

-- | Not exposed by Pipes.Attoparsec
nextSkipEmpty
  :: (Monad m, Eq a, Monoid a)
  => Producer a m r
  -> m (Either r (a, Producer a m r))
nextSkipEmpty = go where
    go p0 = do
      x <- next p0
      case x of
         Left  _        -> return x
         Right (a,p1)
          | a == mempty -> go p1
          | otherwise   -> return x

-- | Like 'decodeL' from Pipes.Aeson.Internal but throws away input on
-- a parse error
decodeL'
  :: (Monad m, Ae.FromJSON a)
  => Parser Ae.Value
  -> Pipes.Parser BS.ByteString m (Maybe (Either PAe.DecodingError (Int, a))) -- ^
decodeL' parser = do
    mev <- parseL' parser
    return $ case mev of
       Nothing             -> Nothing
       Just (Left l)       -> Just (Left (PAe.AttoparsecError l))
       Just (Right (n, v)) -> case Ae.fromJSON v of
          Ae.Error e   -> Just (Left (PAe.FromJSONError e))
          Ae.Success a -> Just (Right (n, a))

-- | Like 'decode' from Pipes.Aeson but throws away input on a parse
-- error
decode'
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser BS.ByteString m (Maybe (Either PAe.DecodingError a))
decode' = fmap (fmap snd) <$> (decodeL' Ae.json')
