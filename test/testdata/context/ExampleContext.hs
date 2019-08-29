module ExampleContext (foo) where

import Data.List (find)
import Control.Monad hiding (fix)

foo :: Int -> Int
foo xs = xs + 1

data Foo a = Foo a
    deriving (Show)

class Bar a where
    bar :: a -> Integer

instance Integral a => Bar (Foo a) where
    bar (Foo a) = toInteger a

