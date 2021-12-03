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

import Control.Plus (class Plus)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, foldl, indexl)
import Data.List (List)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.NonEmpty as NE
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
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
-- NonEmpty
else instance FromFoldable a (NonEmpty f) (NonEmpty f) where
  fromFoldable = identity
else instance (Foldable from, FromFoldable a from to) ⇒ FromFoldable a (NonEmpty from) (NonEmpty to) where
  fromFoldable (x :| xs) = NonEmpty x (fromFoldable xs)
else instance (Applicative to, Semigroup (to a), Plus to, Foldable1 from) ⇒ FromFoldable a from (NonEmpty to) where
  fromFoldable xs = foldMap1 NE.singleton xs
else instance FromFoldable a NonEmptyList NonEmptyArray where
  fromFoldable = NEA.fromFoldable1
else instance FromFoldable a NonEmptyArray NonEmptyList where
  fromFoldable = fromFoldable ∘ NEA.toNonEmpty
else instance Foldable from ⇒ FromFoldable a (NonEmpty from) NonEmptyArray where
  fromFoldable = NEA.fromFoldable1
else instance (FromFoldable a Array to) ⇒ FromFoldable a NonEmptyArray (NonEmpty to) where
  fromFoldable as = case NEA.toNonEmpty as of
    x :| xs → NonEmpty x $ fromFoldable xs
else instance (FromFoldable a List to) ⇒ FromFoldable a NonEmptyList (NonEmpty to) where
  fromFoldable (NonEmptyList (NonEmpty x xs)) = NonEmpty x ∘ fromFoldable $ xs
else instance (Foldable from, FromFoldable a from List) ⇒ FromFoldable a (NonEmpty from) NonEmptyList where
  fromFoldable (NonEmpty x xs) = NonEmptyList ∘ NonEmpty x ∘ fromFoldable $ xs

inRange ∷ ∀ a. Ord a ⇒ a → a → a → Boolean
inRange a b x
  | a > b = inRange b a x

inRange a b x = between a b x

infixl 4 inRange as ..?

infixr 9 compose as ∘

mapAfterMap ∷ ∀ g b a f. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapAfterMap = map ∘ map

infixl 4 mapAfterMap as <<$>>
