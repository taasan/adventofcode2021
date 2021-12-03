{- https://adventofcode.com/2021/day/3
-}
module Advent.Door3
  ( open
  ) where

import Prelude

import Advent.Lib (fromFoldable, (<$?>), (∘))
import Advent.Parser (Parser)
import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray, head, index, mapWithIndex, reverse, (!!))
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (foldl, length, product, sum)
import Data.Int.Bits (shl)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unfoldable (range)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (many1, many1Till)
import Text.Parsing.Parser.String (char)
import Unsafe.Coerce (unsafeCoerce)

open ∷ String → Either String String
open input =
  case runParser input $ fromFoldable ∘ map fromFoldable <$> many1 lineP of
    Right (constructors ∷ NonEmptyArray (NonEmptyArray (Int → Bit))) →
      pure ∘ show $
        product ∘ solve
          <$> [ calculatePart1, calculatePart2 ]
      where
      xs = f <$> constructors
        where
        f bits = mapWithIndex mkBit (reverse bits)
        mkBit i b = b i
      solve f = [ f ] <*> [ identity, bitFlip ]
      calculatePart1 f = fromBits $ f <$> mostCommonIndexed bitCount xs

      calculatePart2 f = fromBits $ head folded
        where
        folded = unsafeCoerce $
          foldl
            (map <$> fromMaybe <*> doFilter)
            (fromFoldable xs)
            (range 0 $ bitCount - 1 ∷ Array _)

        doFilter acc@[ _ ] _ = pure acc
        doFilter acc i = do
          ys ← fromArray acc
          mbit ← f <$> mostCommonIndexed bitCount ys !! i
          let keep bs = bs !! position mbit == Just mbit
          pure $ filter keep acc
      bitCount = length $ head constructors

    Left err →
      (Left ∘ show) err

mostCommonIndexed
  ∷ Int
  → NonEmptyArray (NonEmptyArray Bit)
  → NonEmptyArray Bit
mostCommonIndexed bitCount xs = mostCommon xs <$> range (bitCount - 1) 0

mostCommon
  ∷ NonEmptyArray (NonEmptyArray Bit)
  → Int
  → Bit
mostCommon xs pos =
  if numOnesT2 >= lines then
    One pos
  else
    Zero pos
  where
  numOnesT2 = ones pos * 2
  lines = length xs
  ones n = sum $ toBinaryDigit <$> column n
  column n = identity <$?> foldl f [] xs
    where
    f acc bs = Array.snoc acc $ index bs n
  toBinaryDigit (Zero _) = 0
  toBinaryDigit (One _) = 1

data Bit = Zero Int | One Int

instance Show Bit where
  show (Zero n) = "Zero " <> show n
  show (One n) = "One " <> show n

derive instance Eq Bit

position ∷ Bit → Int
position (Zero n) = n
position (One n) = n

bitFlip ∷ Bit → Bit
bitFlip (Zero n) = One n
bitFlip (One n) = Zero n

fromBits ∷ NonEmptyArray Bit → Int
fromBits ys = foldl setBit 0 $ ys
  where
  setBit acc (Zero _) = acc
  setBit acc (One pos) = acc + shl 1 pos

lineP ∷ Parser (NonEmptyList (Int → Bit))
lineP = many1Till bitP (char '\n')
  where
  bitP = Zero <$ char '0' <|> One <$ char '1'
