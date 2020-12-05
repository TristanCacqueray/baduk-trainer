module Baduk.Types where

import Data.Eq (class Eq)
import Data.List (List(Nil))
import Prelude (class Show, show, (<>))
import SGF.Types (Color(..))

data Coord
  = Coord Int Int

data Position
  = Empty
  | Occupied Color

data Stone
  = Stone Color Coord

instance showPoint :: Show Coord where
  show (Coord x y) = "(Coord " <> show x <> " " <> show y <> ")"

instance showPos :: Show Position where
  show Empty = "."
  show (Occupied Black) = "b"
  show (Occupied White) = "w"

derive instance eqCoord :: Eq Coord

type Player
  = { stones :: List Coord, moves :: List Coord }

type Game
  = { size :: Int
    , startingPlayer :: Color
    , black :: Player
    , white :: Player
    , move :: Int
    , stonesAlive :: List Stone
    }

showGame :: Game -> String
showGame game = "<Baduk size=" <> show game.size <> " />"

initPlayer :: Player
initPlayer = { stones: Nil, moves: Nil }

getPlayer :: Game -> Color -> Player
getPlayer game = case _ of
  Black -> game.black
  White -> game.white

initGame :: Game
initGame =
  { size: 19
  , startingPlayer: Black
  , black: initPlayer
  , white: initPlayer
  , move: 0
  , stonesAlive: Nil
  }
