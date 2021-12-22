module Advent.Door21
  ( open
  ) where

import Prelude

import Advent.Lib (fromFoldable, (∘))
import Advent.Parser (Parser, integer)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List.Lazy (List, cycle, drop, index, range, take)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (sepEndBy)
import Text.Parsing.Parser.String (char, string)

open ∷ String → Either String String
open input = case runParser input $ startingPosition `sepEndBy` (char '\n') of
  Right xs →
    case fromFoldable xs of
      [ player1, player2 ] →
        (pure ∘ show) [ part1 ]
        where
        part1 ∷ Int
        part1 = game.rolls * loser.total
          where
          game = play
            { player1
            , player2
            , dice: cycle (range 1 100)
            , rolls: 0
            , won: false
            , rollsPerPlayer: 3
            , winningSum: 1000
            , trackSize: 10
            }

          loser ∷ Player
          loser =
            if game.player1.total < game.player2.total then
              game.player1
            else
              game.player2

      _ → (Left ∘ show) xs

  Left err → (Left ∘ show) err

type Player =
  { position ∷ Int
  , total ∷ Int
  }

startingPosition ∷ Parser Player
startingPosition = do
  _ ← string "Player "
  _ ← integer
  _ ← string " starting position: "
  position ← integer
  pure { total: 0, position }

type Game =
  { dice ∷ List Int
  , rolls ∷ Int
  , player1 ∷ Player
  , player2 ∷ Player
  , won ∷ Boolean
  , rollsPerPlayer ∷ Int
  , winningSum ∷ Int
  , trackSize ∷ Int
  }

play ∷ Game → Game
play game@{ won: true } = game
play game =
  go true game
  where
  go :: Boolean -> Game -> Game
  go _ g@{ won: true } = g
  go isPlayer1 g =
    go (not isPlayer1) (roll isPlayer1 g)

roll ∷ Boolean → Game → Game
roll isPlayer1 (game) =
  if isPlayer1 then
    newGame { player1 = newPlayer }
  else
    newGame { player2 = newPlayer }
  where
  newGame = game
    { dice = drop game.rollsPerPlayer game.dice
    , won = newPlayer.total >= game.winningSum
    , rolls = game.rolls + game.rollsPerPlayer
    }
  player =
    if isPlayer1 then
      game.player1
    else
      game.player2
  newPlayer =
    { position: newPlayerPosition
    , total: player.total + newPlayerPosition
    }

  track = drop player.position ∘ cycle $ range 0 (game.trackSize - 1)
  newPlayerPoints = sum ∘ take game.rollsPerPlayer $ game.dice
  newPlayerPosition = unsafeIndex track newPlayerPoints

unsafeIndex ∷ ∀ a. List a → Int → a
unsafeIndex xs = unsafePartial $ fromJust ∘ index xs
