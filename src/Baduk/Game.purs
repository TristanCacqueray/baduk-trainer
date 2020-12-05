module Baduk.Game where

import SGF.Types
import Baduk.Types (Capture(..), Coord(..), Game, Player, Stone(..), getPlayer)
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (find)
import Data.List (List(..), elem, filter, foldMap, intercalate, nub, snoc, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Prelude (eq, flip, map, not, otherwise, show, ($), (&&), (+), (-), (<), (<$>), (<<<), (<>), (/=), (==), (>=), (||))
import SGF (inverse)

-- | Initial board when the game start
setAliveStones :: Game -> Game
setAliveStones game = game { stonesAlive = blackStones <> whiteStones }
  where
  blackStones = map (Stone Black) game.black.stones

  whiteStones = map (Stone White) game.white.stones

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
getGroup sz stonesAlive coord = case find ((==) coord <<< getCoord) stonesAlive of
  Just (Stone color _) -> Just $ go coord $ initGroup color
  Nothing -> Nothing
  where
  go :: Coord -> Group -> Group
  go c g
    -- Coord is already visited
    | elem c g.members || elem c g.liberties = g
    -- Coord is part of the group, visit surrounding
    | elem (Stone g.color c) stonesAlive =
      go (move coord Up)
        $ go (move coord Down)
        $ go (move coord Left)
        $ go (move coord Right)
        $ g { members = c : g.members }
    -- Coord is empty
    | inBoard c sz && not elem (Stone (inverse g.color) c) stonesAlive = g { liberties = c : g.liberties }
    -- Coord is blocked
    | otherwise = g

-- | Remove dead stones at and around Coord location
removeDeadStones :: Size -> List Stone -> Coord -> List Stone
removeDeadStones sz xs coord = filter isAlive xs
  where
  isAlive :: Stone -> Boolean
  isAlive = not <<< flip elem captures

  captures :: List Stone
  captures =
    nub $ captured coord
      <> captured (move coord Up)
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
  updatePlayer p = p { moves = snoc p.moves coord, captures = (Capture game.move captures) : p.captures }

  doAddStone g = case stone of
    Stone Black _ -> g { black = updatePlayer g.black }
    Stone White _ -> g { white = updatePlayer g.white }

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
