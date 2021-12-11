module Advent.Door where

import Prelude (class Bounded, class Eq, class Ord, class Show, (+), (-))
import Advent.Door1 as D1
import Advent.Door2 as D2
import Advent.Door3 as D3
import Advent.Door6 as D6
import Advent.Lib ((∘))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, defaultPred, defaultSucc, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
  cardinality = genericCardinality
  toEnum = genericToEnum ∘ (_ - 1)
  fromEnum = (_ + 1) ∘ genericFromEnum

open ∷ Door → String → Either String String
open Door1 = D1.open
open Door2 = D2.open
open Door3 = D3.open
open Door6 = D6.open

open _ = \_ → Left "Nothing yet"
