{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe
import qualified Data.List

main :: IO ()
main = putStrLn "hello"

foo :: Either a b -> Either a b
foo = id

bar :: Int
bar = foldl (-) 0 [1,2,3]

baz :: [String]
baz = mapM head [["a"]]
