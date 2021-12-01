{- https://adventofcode.com/2021/day/1
-}
module Advent.Door1 (open) where

import Prelude
import Advent.Lib (fromFoldable, lines, (<$?>), (<<$>>), (∘))
import Data.Either (Either)
import Data.Foldable (foldl, sum)
import Data.Int (fromString)
import Data.List.Lazy (List(..), Step(..), nil, repeat, step, take, zip, zipWith, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

open ∷ String → Either String String
open input = (pure ∘ show) $ countIncreased <<$>> [ identity, slidingSums ] <*> [ fromFoldable xs ]
  where
  xs ∷ Array Int
  xs = fromString <$?> lines input

countIncreased ∷ ∀ a. Ord a ⇒ List a → Int
countIncreased xs = foldl f 0 increased
  where
  f acc x = if x == Just GT then acc + 1 else acc

  increased = compare' <$> zip xs (Nothing : (Just <$> xs))

  compare' (Tuple a (Just b)) = Just $ compare a b

  compare' _ = Nothing

slidingSums ∷ ∀ a. Semiring a ⇒ List a → List a
slidingSums xs = sum <$> slidingWindows 3 xs

slidingWindows ∷ ∀ a. Int → List a → List (List a)
slidingWindows size = foldl (flip $ zipWith (:)) (repeat nil) ∘ take size ∘ tails

tails ∷ ∀ a. List a → List (List a)
tails xs = case step xs of
  Nil -> fromStep Nil
  ys'@(Cons _ ys) -> fromStep ys' : tails ys

fromStep ∷ ∀ a. Step a → List a
fromStep = List ∘ pure
