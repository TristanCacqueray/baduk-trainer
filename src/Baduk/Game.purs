module Baduk.Game where

import Baduk.Types
import Baduk.Types
import Prelude
import SGF.Types
import Data.Char (fromCharCode, toCharCode)
import Data.List (List(..), concatMap, elem, filter, foldMap, intercalate, reverse, snoc, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import SGF (inverse)

-- TODO: validate move is valid?
addStone ∷ Color -> Coord -> Game → Game
addStone color coord game = case color of
  Black -> game { black = game.black { moves = snoc game.black.moves coord } }
  White -> game { white = game.white { moves = snoc game.white.moves coord } }

setPlayerStone :: Player -> Coord -> Player
setPlayerStone p c = p { stones = snoc p.stones c }

removePlayerStone :: Player -> Coord -> Player
removePlayerStone p c = p { stones = filter (not <<< eq $ c) p.stones }

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

save :: Game -> String
save g =
  intercalate "\n"
    ( [ "(;SZ[" <> show g.size <> "]" ]
        <> addStones "B" g.black.stones
        <> addStones "W" g.white.stones
        <> [ ";PL[" <> show g.startingPlayer <> "]" <> stonePlayed <> ")" ]
    )
  where
  addStones _ Nil = []

  addStones c xs = [ ";A" <> c <> foldMap saveCoord xs ]

  stonePlayed :: String
  stonePlayed =
    intercalate ";"
      (map showPlayedPos allStonePlayed)

  showPlayedPos :: Tuple Color Coord -> String
  showPlayedPos (Tuple color coord) = show color <> saveCoord coord

  allStonePlayed :: List (Tuple Color Coord)
  allStonePlayed = interleave starting opponent

  interleave :: forall a. List a -> List a -> List a
  interleave (Cons x xs) o = Cons x (interleave o xs)

  interleave _ _ = Nil

  starting :: List (Tuple Color Coord)
  starting = map (Tuple g.startingPlayer) (getPlayer g g.startingPlayer).moves

  opponent :: List (Tuple Color Coord)
  opponent = map (Tuple (inverse g.startingPlayer)) (getPlayer g (inverse g.startingPlayer)).moves
