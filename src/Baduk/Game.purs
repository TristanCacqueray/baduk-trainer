-- | Baduk game logic
module Baduk.Game
  ( addMove
  , addStone
  , initAliveStones
  , isCompleted
  , getLastMove
  , setStone
  , removeStone
  -- Export for debugging purpose
  , removeDeadStones
  , getGroup
  ) where

import Baduk.Types (Capture(..), Coord(..), Game, Player, PlayerMove(..), Stone(..), Move(..), getPlayer)
import Data.Foldable (find)
import Data.List (List(..), elem, filter, head, length, nub, snoc, (:))
import Data.List as Data.List
import Data.Maybe (Maybe(..))
import Prelude
import SGF (Color(..), inverseColor)

-- | Initial board when the game start
initAliveStones :: Game -> Game
initAliveStones game = game { stonesAlive = blackStones <> whiteStones }
  where
  blackStones = map (Stone Black) game.black.stones

  whiteStones = map (Stone White) game.white.stones

isCompleted :: Game -> Boolean
isCompleted g = hasPassed g.black && hasPassed g.white
  where
  hasPassed player = case player.moves of
    Pass : _ -> true
    _ -> false

type Size
  = Int

inBoard :: Coord -> Size -> Boolean
inBoard (Coord x y) sz = x >= 0 && x < sz && y >= 0 && y < sz

data Dir
  = Up
  | Down
  | Left
  | Right

move :: Coord -> Dir -> Coord
move (Coord x y) = case _ of
  Up -> Coord x (y + 1)
  Down -> Coord x (y - 1)
  Left -> Coord (x - 1) y
  Right -> Coord (x + 1) y

moveStone :: Stone -> Dir -> Stone
moveStone (Stone color coord) dir = Stone color (move coord dir)

type Group
  = { color :: Color, members :: List Coord, liberties :: List Coord }

initGroup :: Color -> Group
initGroup color = { color, members: Nil, liberties: Nil }

getCoord :: Stone -> Coord
getCoord (Stone color coord) = coord

-- | Returns the group at Coord location
getGroup :: Size -> List Stone -> Coord -> Maybe Group
getGroup sz stonesAlive origin = case find ((==) origin <<< getCoord) stonesAlive of
  Just (Stone color _) -> Just $ go origin $ initGroup color
  Nothing -> Nothing
  where
  go :: Coord -> Group -> Group
  go coord g
    -- Coord is already visited
    | elem coord g.members || elem coord g.liberties = g
    -- Coord is part of the group, visit surrounding
    | elem (Stone g.color coord) stonesAlive =
      go (move coord Up)
        $ go (move coord Down)
        $ go (move coord Left)
        $ go (move coord Right)
        $ g { members = coord : g.members }
    -- Coord is empty
    | inBoard coord sz && not elem (Stone (inverseColor g.color) coord) stonesAlive = g { liberties = coord : g.liberties }
    -- Coord is blocked
    | otherwise = g

-- | Remove dead stones around Coord location
removeDeadStones :: Size -> List Stone -> Coord -> List Stone
removeDeadStones sz xs coord = filter isAlive xs
  where
  isAlive :: Stone -> Boolean
  isAlive = not <<< flip elem captures

  captures :: List Stone
  captures =
    nub
      $ captured (move coord Up)
      <> captured (move coord Down)
      <> captured (move coord Left)
      <> captured (move coord Right)

  captured :: Coord -> List Stone
  captured c
    | Just group <- getGroup sz xs c
    , group.liberties == Nil = (map (Stone group.color) group.members)
    | otherwise = Nil

-- | Check if a stone can be played
isValidMove :: Stone -> Game -> Boolean
isValidMove (Stone color coord) game = alive && not ko
  where
  alive = case getGroup game.size game.stonesAlive coord of
    Just group -> group.liberties /= Nil
    Nothing -> false

  -- TODO ko test
  ko = false

lastMove :: Color -> List PlayerMove -> Maybe Move
lastMove color l = case head l of
  Just move' -> Just $ Move color move'
  Nothing -> Nothing

-- | Get last move
getLastMove :: Game -> Maybe Move
getLastMove game
  | length game.black.moves > length game.white.moves = lastMove Black game.black.moves
  | length game.black.moves < length game.white.moves = lastMove White game.white.moves
  | otherwise =
    let
      oponent = inverseColor game.startingPlayer
    in
      lastMove oponent (getPlayer game oponent).moves

addStone ∷ Stone -> Game → Maybe Game
addStone stone@(Stone color coord) game =
  if isValidMove stone newGame then
    Just (doAddStone newGame)
  else
    Nothing
  where
  newGame = game { stonesAlive = removeDeadStones game.size (stone : game.stonesAlive) coord }

  captures :: List Coord
  captures = map getCoord $ filter (not <<< flip elem newGame.stonesAlive) game.stonesAlive

  updatePlayer :: Player -> Player
  updatePlayer p =
    p
      { moves = PlaceStone coord : p.moves
      , captures =
        if Data.List.null captures then
          p.captures
        else
          (Capture game.move captures) : p.captures
      }

  doAddStone g = case stone of
    Stone Black _ -> g { black = updatePlayer g.black }
    Stone White _ -> g { white = updatePlayer g.white }

addMove :: Move -> Game -> Maybe Game
addMove (Move color move') g = case move' of
  PlaceStone coord -> addStone (Stone color coord) g
  Pass ->
    Just
      $ case color of
          Black -> g { black = g.black { moves = Pass : g.black.moves } }
          White -> g { white = g.white { moves = Pass : g.white.moves } }

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
