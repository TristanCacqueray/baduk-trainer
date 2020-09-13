module Baduk.Types where

import SGF.Types (Color(..))
import Data.Array
import Data.Eq (class Eq)
import Data.List (List(Nil))
import Prelude (class Show, show, (<>))

data Coord
  = Coord Int Int

data Position
  = Empty
  | Occupied Color

instance showPoint :: Show Coord where
  show (Coord x y) = "(Coord " <> show x <> " " <> show y <> ")"

instance showPos :: Show Position where
  show Empty = "."
  show (Occupied Black) = "b"
  show (Occupied White) = "w"

derive instance eqCoord :: Eq Coord

type Player
  = { stones :: List Coord }

type Game
  = { size :: Int
    , black :: Player
    , white :: Player
    , board :: Array Position
    }

showGame :: Game -> String
showGame { size: size, board: board } = go board
  where
  go [] = ""

  go xs = show (take size xs) <> "\n" <> go (drop size xs)

initPlayer :: Player
initPlayer = { stones: Nil }

initGame :: Game
initGame = { size: 0, black: initPlayer, white: initPlayer, board: [] }
