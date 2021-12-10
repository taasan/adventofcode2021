{- https://adventofcode.com/2021/day/6
-}
module Advent.Door6 (open) where

import Prelude

import Advent.Lib ((<$?>), (∘))
import Data.Array (filter, length, range)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (nan)
import Data.String (Pattern(..), split, trim)

open ∷ String → Either String String
open input = pure ∘ show ∘ map solve' $ [ 80, 256 ]
  where
  solve' = solve $ fromString <$?> (split (Pattern ",") ∘ trim) input

solve ∷ Array Int → Int → Number
solve xs n = case init xs of
  Just ys -> sum ∘ foldl next ys ∘ range 0 $ n - 1
  _ -> nan
  where
  sum ∷ Buckets → Number
  sum (Buckets a b c d e f g h i) =
    a + b + c + d + e + f + g + h + i

  next ∷ Buckets → Int → Buckets
  -- We are done
  next bs m | m == n = bs
  -- Rotate left and add zeros to sixes
  ------------- 0 1 2 3 4 5 6 7 8 ----
  next (Buckets a b c d e f g h i) _ =
    -------                         --
    ------- 0 1 2 3 4 5    6    7 8 --
    Buckets b c d e f g (h + a) i a --

data Buckets = Buckets Number Number Number Number Number Number Number Number Number

init ∷ Array Int → Maybe Buckets
init xs = case count <$> range 0 8 of
  [ a, b, c, d, e, f, g, h, i ] →
    Just $ Buckets a b c d e f g h i

  _ →
    Nothing

  where
  count ∷ Int → Number
  count n = toNumber ∘ length ∘ filter (_ == n) $ xs
