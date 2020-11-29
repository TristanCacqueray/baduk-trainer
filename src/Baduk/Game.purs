module Baduk.Game where

import Baduk.Types
import Prelude
import SGF.Types
import Data.Char (fromCharCode, toCharCode)
import Data.List (List(..), concatMap, elem, filter, foldMap, intercalate, reverse, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton)

-- TODO: validate move is valid?
addStone ∷ Coord → Game → Maybe Game
addStone move@(Coord x y) game = Just (game { move = Just move })

setPlayerStone :: Player -> Coord -> Player
setPlayerStone p c = { stones: snoc p.stones c }

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

saveCoord :: Coord -> String
saveCoord (Coord x y) = "[" <> savePos x <> savePos y <> "]"
  where
  savePos :: Int -> String
  savePos p = fromMaybe "" $ singleton <$> fromCharCode (toCharCode 'a' + p)

save :: Game -> Maybe Color -> String
save g c =
  intercalate "\n"
    [ "(;SZ[" <> show g.size <> "]" <> startPlayer
    , ";B" <> foldMap saveCoord g.black.stones
    , ";W" <> foldMap saveCoord g.white.stones
    , startPlayer <> ")"
    ]
  where
  startPlayer = case c of
    Just c -> ";PL[" <> show c <> "]"
    Nothing -> ""
