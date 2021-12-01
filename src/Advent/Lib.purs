module Advent.Lib
  ( class FromFoldable
  , fromCharList
  , fromFoldable
  , head
  , index
  , inRange
  , intersections
  , intersect
  , lines
  , mapAfterMap
  , unsafeSplit
  , (!!)
  , (∘)
  , (<$?>)
  , (<<$>>)
  , (..?)
  ) where

import Prelude
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, foldl, indexl)
import Data.List (List)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex (split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

lines ∷ ∀ f. FromFoldable String Array f ⇒ String → f String
lines str = fromFoldable $ unsafeSplit (Pattern "\n") str

head ∷ ∀ a f. Foldable f ⇒ f a → Maybe a
head = indexl 0

-- | Generic version of index
index ∷ ∀ a f. Foldable f ⇒ f a → Int → Maybe a
index = flip indexl

infixl 8 index as !!

infixl 4 filterMap as <$?>

unsafeSplit ∷ Pattern → String → Array String
unsafeSplit (Pattern pattern) = split re
  where
  re = unsafeRegex pattern noFlags

fromCharList ∷ List Char → String
fromCharList = fromCharArray ∘ fromFoldable

-- | Intersection of multiple foldables
intersections
  ∷ ∀ a f g
  . Ord a
  ⇒ Bind f
  ⇒ Foldable f
  ⇒ FromFoldable a f g
  ⇒ FromFoldable a Set f
  ⇒ f (f a)
  → g a
intersections xs = fromFoldable $ foldl intersect (join xs) xs

intersect
  ∷ ∀ f1 f2 f3 a
  . Ord a
  ⇒ Foldable f1
  ⇒ Foldable f2
  ⇒ FromFoldable a Set f3
  ⇒ f1 a
  → f2 a
  → f3 a
intersect xs ys = fromFoldable $ Set.intersection xs' ys'
  where
  xs' = Set.fromFoldable xs

  ys' = Set.fromFoldable ys

class FromFoldable a from to where
  fromFoldable ∷ Foldable from ⇒ from a → to a

instance FromFoldable a f f where
  fromFoldable = identity
else instance FromFoldable a f Array where
  fromFoldable = Array.fromFoldable
else instance FromFoldable a f List where
  fromFoldable = List.fromFoldable
else instance FromFoldable a f LazyList.List where
  fromFoldable = LazyList.fromFoldable
else instance Ord a ⇒ FromFoldable a f Set where
  fromFoldable = Set.fromFoldable

inRange ∷ ∀ a. Ord a ⇒ a → a → a → Boolean
inRange a b x
  | a > b = inRange b a x

inRange a b x = between a b x

infixl 4 inRange as ..?

infixr 9 compose as ∘

mapAfterMap ∷ ∀ g b a f. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapAfterMap = map ∘ map

infixl 4 mapAfterMap as <<$>>
