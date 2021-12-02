{- https://adventofcode.com/2021/day/2
-}
module Advent.Door2
  ( open
  ) where

import Prelude

import Advent.Lib ((∘))
import Advent.Parser (Parser, integer)
import Control.Alternative ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (sepEndBy1)
import Text.Parsing.Parser.String (char, string)

open ∷ String → Either String String
open input = case runParser input $ commandP `sepEndBy1` char '\n' of
  Right xs →
    pure ∘ show $
      map positionProduct ∘ solve
        <$> [ movePart1, movePart2 ]
        <*> [ xs ]

  Left err →
    (Left ∘ show) err
  where
  solve f = foldl f { depth: 0, horizontal: 0, aim: 0 }
  positionProduct { depth, horizontal } = depth * horizontal

data Command
  = Forward
  | Up
  | Down

type Position =
  { depth ∷ Int
  , horizontal ∷ Int
  , aim ∷ Int
  }

movePart1 ∷ Position → (Tuple Command Int) → Position
movePart1 pos (Tuple Forward x) = pos { horizontal = pos.horizontal + x }
movePart1 pos (Tuple Up x) = pos { depth = pos.depth - x }
movePart1 pos (Tuple Down x) = pos { depth = pos.depth + x }

movePart2 ∷ Position → (Tuple Command Int) → Position
movePart2 pos (Tuple Forward x) = pos { horizontal = pos.horizontal + x, depth = pos.depth + pos.aim * x }
movePart2 pos (Tuple Up x) = pos { aim = pos.aim - x }
movePart2 pos (Tuple Down x) = pos { aim = pos.aim + x }

commandP ∷ Parser (Tuple Command Int)
commandP = Tuple <$> command <* char ' ' <*> integer
  where
  command = Forward <$ string "forward"
    <|> Up <$ string "up"
    <|> Down <$ string "down"
