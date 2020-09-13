module Baduk.Types where

import Data.List (List(Nil))
import Prelude (class Show)

data Coord
  = Coord Int Int

instance showPoint :: Show Coord where
  show _ = "A coord"

type Player
  = { stones :: List Coord }

type Game
  = { size :: Int
    , black :: Player
    , white :: Player
    }

initPlayer :: Player
initPlayer = { stones: Nil }

initGame :: Game
initGame = { size: 0, black: initPlayer, white: initPlayer }
