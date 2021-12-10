module Advent.Door where

import Prelude (class Bounded, class Eq, class Ord, class Show, pure)
import Advent.Door1 as D1
import Advent.Door2 as D2
import Advent.Door3 as D3
import Advent.Door6 as D6
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))

data Door
  = Door1
  | Door2
  | Door3
  | Door4
  | Door5
  | Door6
  | Door7
  | Door8
  | Door9
  | Door10
  | Door11
  | Door12
  | Door13
  | Door14
  | Door15
  | Door16
  | Door17
  | Door18
  | Door19
  | Door20
  | Door21
  | Door22
  | Door23
  | Door24
  | Door25

answer ∷ Door → (Either String String)
answer Door1 = Right "[1711,1743]"
answer Door2 = Right "[1815044,1739283308]"
answer Door3 = Right "[2640986,6822109]"
answer Door6 = Right "[388419.0,1740449478328.0]"

answer _ = Left "Nothing yet"

derive instance Generic Door _

derive instance Eq Door

derive instance Ord Door

instance Show Door where
  show = genericShow

instance Enum Door where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance Bounded Door where
  bottom = Door1
  top = Door25

instance BoundedEnum Door where
  cardinality = Cardinality 25
  toEnum 1 = pure Door1
  toEnum 2 = pure Door2
  toEnum 3 = pure Door3
  toEnum 4 = pure Door4
  toEnum 5 = pure Door5
  toEnum 6 = pure Door6
  toEnum 7 = pure Door7
  toEnum 8 = pure Door8
  toEnum 9 = pure Door9
  toEnum 10 = pure Door10
  toEnum 11 = pure Door11
  toEnum 12 = pure Door12
  toEnum 13 = pure Door13
  toEnum 14 = pure Door14
  toEnum 15 = pure Door15
  toEnum 16 = pure Door16
  toEnum 17 = pure Door17
  toEnum 18 = pure Door18
  toEnum 19 = pure Door19
  toEnum 20 = pure Door20
  toEnum 21 = pure Door21
  toEnum 22 = pure Door22
  toEnum 23 = pure Door23
  toEnum 24 = pure Door24
  toEnum 25 = pure Door25
  toEnum _ = Nothing
  fromEnum Door1 = 1
  fromEnum Door2 = 2
  fromEnum Door3 = 3
  fromEnum Door4 = 4
  fromEnum Door5 = 5
  fromEnum Door6 = 6
  fromEnum Door7 = 7
  fromEnum Door8 = 8
  fromEnum Door9 = 9
  fromEnum Door10 = 10
  fromEnum Door11 = 11
  fromEnum Door12 = 12
  fromEnum Door13 = 13
  fromEnum Door14 = 14
  fromEnum Door15 = 15
  fromEnum Door16 = 16
  fromEnum Door17 = 17
  fromEnum Door18 = 18
  fromEnum Door19 = 19
  fromEnum Door20 = 20
  fromEnum Door21 = 21
  fromEnum Door22 = 22
  fromEnum Door23 = 23
  fromEnum Door24 = 24
  fromEnum Door25 = 25

open ∷ Door → String → Either String String
open Door1 = D1.open
open Door2 = D2.open
open Door3 = D3.open
open Door6 = D6.open

open _ = \_ → Left "Nothing yet"
