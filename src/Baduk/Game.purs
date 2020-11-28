module Baduk.Game where

import Baduk.Types
import Prelude
import SGF.Types
import Data.List (List(..), elem, filter)
import Data.Maybe (Maybe(..))

-- TODO: validate move is valid?
addStone ∷ Coord → Game → Maybe Game
addStone move@(Coord x y) game = Just (game { move = Just move })

setPlayerStone :: Player -> Coord -> Player
setPlayerStone p c = { stones: Cons c p.stones }

removePlayerStone :: Player -> Coord -> Player
removePlayerStone p c = { stones: filter (not <<< eq $ c) p.stones }

setStone :: Game -> Coord -> Color -> Game
setStone g coord color
  | not (coord `elem` g.black.stones || coord `elem` g.white.stones) = case color of
    Black -> g { black = setPlayerStone g.black coord }
    White -> g { white = setPlayerStone g.white coord }
  | otherwise = g

removeStone :: Game -> Coord -> Game
removeStone g coord
  | coord `elem` g.black.stones = g { black = removePlayerStone g.black coord }
  | coord `elem` g.white.stones = g { white = removePlayerStone g.white coord }
  | otherwise = g
