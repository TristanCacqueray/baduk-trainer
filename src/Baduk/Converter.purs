-- | Convertion functions for the Baduk Game type
module Baduk.Converter (load, save, showBoard, readBoard) where

import Baduk.Types (Coord(..), Game, Move(..), PlayerMove(..), Position(..), Stone(..), getPlayer, initGame)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, execStateT, get, modify)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (cons, drop, mapWithIndex, reverse, take)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either)
import Data.Foldable (elem, foldMap, traverse_)
import Data.Identity (Identity)
import Data.Int (round)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.Lazy (replicate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (CodePoint, codePointFromChar, length, null, toCodePointArray)
import Data.String.CodeUnits (singleton)
import Data.Traversable (for)
import Data.Tuple (Tuple)
import Prelude
import SGF (Color(..), FlatSGF, Property(..), SGF, Value(..), flatten, inverseColor)

type Error
  = String

type Log
  = Array String

type Loader
  = StateT Game (WriterT Log (ExceptT Error Identity))

loader ∷ FlatSGF → Loader Game
loader fsgf = case onlyHead fsgf of
  Just props → traverse_ go props *> modify identity
  Nothing → throwError "Only single tree game are currently supported"
  where
  onlyHead ∷ ∀ a. List a → Maybe a
  onlyHead xs = case L.uncons xs of
    Just { head: a, tail: Nil } → Just a
    _ → Nothing

  go ∷ Property → Loader Game
  go (Prop "GN" (Text s : Nil)) = modify (\game -> game { name = s })

  go (Prop "SZ" (Num n : Nil)) = modify (\game → game { size = round n })

  go (Prop "PL" (Color col : Nil)) = modify (\game → game { startingPlayer = col })

  go (Prop "AB" vals) = setStones Black vals

  go (Prop "B" vals) = addStones Black vals

  go (Prop "AW" vals) = setStones White vals

  go (Prop "W" vals) = addStones White vals

  go (Prop name _) = tell [ "Unknown property " <> name ] *> get

  addMoves :: Color -> List PlayerMove -> Loader Game
  addMoves color moves =
    modify
      ( \game → case color of
          Black → game { black = game.black { moves = moves <> game.black.moves } }
          White → game { white = game.white { moves = moves <> game.white.moves } }
      )

  addStones ∷ Color → List Value → Loader Game
  addStones color vals = case for vals valPoint of
    Just points → addMoves color (map PlaceStone points)
    Nothing → addMoves color (Pass : Nil)

  setStones ∷ Color → List Value → Loader Game
  setStones color vals = case for vals valPoint of
    Just points →
      modify
        ( \game → case color of
            Black → game { black = game.black { stones = points <> game.black.stones } }
            White → game { white = game.white { stones = points <> game.white.stones } }
        )
    Nothing → throwError ("Invalid stones value for " <> show color)

  valPoint ∷ Value → Maybe Coord
  valPoint (Point x y) = Just (Coord x y)

  valPoint _ = Nothing

load ∷ SGF → Either Error (Tuple Game Log)
load sgf = unwrap $ runExceptT $ runWriterT $ execStateT (loader (flatten sgf)) initGame

saveCoord :: Coord -> String
saveCoord (Coord x y) = "[" <> savePos x <> savePos y <> "]"
  where
  savePos :: Int -> String
  savePos p = fromMaybe "" $ singleton <$> fromCharCode (toCharCode 'a' + p)

save :: Game -> String
save g =
  L.intercalate "\n"
    ( [ "(;" <> gameName <> "SZ[" <> show g.size <> "]" <> "PL[" <> show g.startingPlayer <> "]" ]
        <> addStones "B" g.black.stones
        <> addStones "W" g.white.stones
        <> [ ";" <> stonePlayed <> ")" ]
    )
  where
  gameName = if null g.name && g.name /= "Unknown" then "" else ("GN[" <> g.name <> "]")

  addStones _ Nil = []

  addStones c xs = [ ";A" <> c <> foldMap saveCoord xs ]

  stonePlayed :: String
  stonePlayed =
    L.intercalate "(;"
      (map showPlayedPos allStonePlayed)
      <> L.intercalate "" (replicate (L.length allStonePlayed - 1) ")")

  showPlayedPos :: Move -> String
  showPlayedPos (Move color move) =
    show color
      <> ( case move of
            PlaceStone coord -> saveCoord coord
            Pass -> "[]"
        )

  allStonePlayed :: List Move
  allStonePlayed = L.reverse $ interleave starting opponent

  interleave :: forall a. List a -> List a -> List a
  interleave (Cons x xs) o = Cons x (interleave o xs)

  interleave _ (Cons x xs) = Cons x (interleave xs Nil)

  interleave _ _ = Nil

  starting :: List Move
  starting = map (Move g.startingPlayer) (getPlayer g g.startingPlayer).moves

  opponent :: List Move
  opponent = map (Move (inverseColor g.startingPlayer)) (getPlayer g (inverseColor g.startingPlayer)).moves

coordToIdx ∷ Int → Coord → Int
coordToIdx sz (Coord cx cy) = cx + cy * sz

idxToCoord ∷ Int → Int → Coord
idxToCoord sz idx = Coord x y
  where
  x = mod idx sz

  y = idx / sz

readBoard :: List String -> Maybe Game
readBoard xs = getSize >>= \s -> pure (initGame { size = s, stonesAlive = go 0 xs })
  where
  getSize = L.head xs >>= \x -> pure $ length x

  go :: Int -> List String -> List Stone
  go pos = case _ of
    Nil -> Nil
    (Cons x rest) ->
      let
        row = (L.catMaybes $ L.fromFoldable $ mapWithIndex (goRow pos) (toCodePointArray x))
      in
        row <> (go (pos + 1) rest)

  goRow :: Int -> Int -> CodePoint -> Maybe Stone
  goRow x y cp
    | cp == codePointFromChar 'b' = pure $ Stone Black (Coord x y)
    | cp == codePointFromChar 'w' = pure $ Stone White (Coord x y)
    | otherwise = Nothing

-- TODO: use initAliveStones before rendering the board, this is only used in tests.
getBoard ∷ Game → Array Position
getBoard game@{ size: size, black: bplayer, white: wplayer } = reverse (create (game.size * game.size))
  where
  create ∷ Int → Array Position
  create 0 = []

  create n = createCell (idxToCoord size n) `cons` create (n - 1)

  createCell coord
    | coord `elem` bplayer.stones || (PlaceStone coord) `elem` bplayer.moves = Occupied Black
    | coord `elem` wplayer.stones || (PlaceStone coord) `elem` wplayer.moves = Occupied White
    | otherwise = Empty

showBoard :: Game -> String
showBoard game = go (getBoard game)
  where
  go [] = ""

  go xs = show (take game.size xs) <> "\n" <> go (drop game.size xs)
