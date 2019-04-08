{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Forked from: Text.Fuzzy (MIT)
-- Original Author: Joomy Korkut, http://github.com/joom/fuzzy
-- Uses 'TextualMonoid' to be able to run on different types of strings.
module Haskell.Ide.Engine.Support.Fuzzy where

import Prelude hiding (filter, null)

import Data.Char   (toLower)
import Data.List   (sortOn)
import Data.Maybe  (isJust, mapMaybe)
import Data.Ord

import qualified Data.Monoid.Textual as T

-- | Included in the return type of @'match'@ and @'filter'@.
-- Contains the original value given, and the matching score.
data Fuzzy t s =
  Fuzzy { original :: t
        , score    :: Int
        } deriving (Show, Eq)

-- | Returns the matched value and score.
-- TODO: add more scoring factors (see Sublime's fuzzy algo)
--
-- >>> match id True "fnt" "infinite"
-- Just ("infinite", 3)
--
-- >>> match fst False "hsk" ("Haskell", 1995)
-- Just ("haskell", 5)
--
match :: (T.TextualMonoid s)
      => (t -> s) -- ^ The function to extract the text from the container.
      -> Bool     -- ^ Case sensitivity.
      -> s        -- ^ Pattern.
      -> t        -- ^ The value containing the text to search in.
      -> Maybe (Fuzzy t s) -- ^ The original value, rendered string and score.
match extract caseSensitive pattern t =
    if null pat
        then Just (Fuzzy t totalScore)
        else Nothing
  where
    null :: (T.TextualMonoid s) => s -> Bool
    null = not . T.any (const True)

    s = extract t
    (s', pattern') = let f = T.map toLower in
                     if caseSensitive
                        then (s, pattern)
                        else (f s, f pattern)

    (totalScore, _, pat) =
      T.foldl'
        undefined
        (\(tot, cur, pat') c ->
            case T.splitCharacterPrefix pat' of
              Nothing -> (tot, 0, pat')
              Just (x, xs) ->
                if x == c then
                  let cur' = cur * 2 + 1 in
                  (tot + cur', cur', xs)
                else (tot, 0, pat')
        ) (0, 0, pattern') s'

-- | The function to filter a list of values by fuzzy search on the text extracted from them.
--
-- >>> filter fst False "ML" [("Standard ML", 1990),("OCaml",1996),("Scala",2003)]
-- [ Fuzzy {original = ("Standard ML",1990), score = 4}
-- , Fuzzy {original = ("OCaml",1996), score = 4} ]
filterBy :: (T.TextualMonoid s)
       => (t -> s) -- ^ The function to extract the text from the container.
       -> s        -- ^ Pattern.
       -> [t]      -- ^ The list of values containing the text to search in.
       -> [t]      -- ^ The list of results, sorted, highest score first.
filterBy extract pattern ts =
  map original $
    sortOn (Down . score)
      (mapMaybe (match extract False pattern) ts)

-- | For filtering on normal text values
--
-- >>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
-- ["vim","virtual machine"]
simpleFilter :: (T.TextualMonoid s)
             => s   -- ^ Pattern to look for.
             -> [s] -- ^ List of texts to check.
             -> [s] -- ^ The ones that match.
simpleFilter pattern xs =
  filterBy id pattern xs


-- | Returns false if the pattern and the text do not match at all.
-- Returns true otherwise.
--
-- >>> test "brd" "bread"
-- True
test :: (T.TextualMonoid s)
     => s -> s -> Bool
test p s = isJust (match id False p s)
