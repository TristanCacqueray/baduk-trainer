module Baduk.Game where

import Baduk.Types (Coord(..), Game)
import Data.Maybe (Maybe(..))

addStone ∷ Coord → Game → Maybe Game
addStone move@(Coord x y) game = -- TODO: validate move is valid?
  Just (game { move = Just move })
