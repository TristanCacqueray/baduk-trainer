module Baduk.Types where

import Data.List (List(Nil))
import Prelude
import SGF.Types (Color(..))

data Coord
  = Coord Int Int

data Position
  = Empty
  | Occupied Color

data Stone
  = Stone Color Coord

data Capture
  = Capture Int (List Coord)

data PlayerMove
  = PlaceStone Coord
  | Pass

data Move
  = Move Color PlayerMove

data Result
  = Win
  | Loss

instance showResult :: Show Result where
  show Win = "win"
  show Loss = "loss"

instance showPoint :: Show Coord where
  show (Coord x y) = "(Coord " <> show x <> " " <> show y <> ")"

instance showPlayerMove :: Show PlayerMove where
  show (PlaceStone coord) = "(PlaceStone " <> show coord <> ")"
  show Pass = "Pass"

instance showMove :: Show Move where
  show (Move color pm) = "(Move " <> show color <> " " <> show pm <> ")"

instance showCapture :: Show Capture where
  show (Capture p xs) = "(Capture " <> show p <> " " <> show xs <> ")"

instance showPos :: Show Position where
  show Empty = "."
  show (Occupied Black) = "b"
  show (Occupied White) = "w"

showC :: Color -> String
showC = case _ of
  Black -> "Black"
  White -> "White"

instance showStone :: Show Stone where
  show (Stone color coord) = "(Stone " <> showC color <> " " <> show coord <> ")"

derive instance eqCoord :: Eq Coord

derive instance ordCoord :: Ord Coord

derive instance eqCapture :: Eq Capture

derive instance ordCapture :: Ord Capture

derive instance eqMove :: Eq PlayerMove

derive instance ordMove :: Ord PlayerMove

derive instance eqStone :: Eq Stone

derive instance ordStone :: Ord Stone

type Player
  = { stones :: List Coord, moves :: List PlayerMove, captures :: List Capture }

initPlayer :: Player
initPlayer = { stones: Nil, moves: Nil, captures: Nil }

type Game
  = { name :: String
    , komi :: Number
    , size :: Int
    , startingPlayer :: Color
    , black :: Player
    , white :: Player
    , move :: Int
    , stonesAlive :: List Stone
    }

initGame :: Game
initGame =
  { name: ""
  , komi: 6.5
  , size: 19
  , startingPlayer: Black
  , black: initPlayer
  , white: initPlayer
  , move: 0
  , stonesAlive: Nil
  }

showGame :: Game -> String
showGame game = "<Baduk size=" <> show game.size <> " stones=" <> show game.stonesAlive <> " />"

getPlayer :: Game -> Color -> Player
getPlayer game = case _ of
  Black -> game.black
  White -> game.white
