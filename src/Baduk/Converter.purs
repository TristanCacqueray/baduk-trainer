module Baduk.Converter (load, showBoard, readBoard) where

import SGF.Types
import Baduk.Types (Coord(..), Game, Position(..), Stone(..), initGame)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, execStateT, get, modify)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (cons, drop, mapWithIndex, reverse, take)
import Data.Either (Either)
import Data.Foldable (elem, traverse_)
import Data.Identity (Identity)
import Data.Int (round)
import Data.List (List(..), catMaybes, fromFoldable, head, uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (CodePoint, codePointFromChar, length, toCodePointArray)
import Data.Traversable (for)
import Data.Tuple (Tuple)
import Prelude (identity, mod, otherwise, pure, show, ($), (*), (*>), (+), (-), (/), (<>), (>>=), (==))

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
  onlyHead xs = case uncons xs of
    Just { head: a, tail: Nil } → Just a
    _ → Nothing

  go ∷ Property → Loader Game
  go (Prop "SZ" (Num n : Nil)) = modify (\game → game { size = round n })

  go (Prop "PL" (Color col : Nil)) = modify (\game → game { startingPlayer = col })

  go (Prop "AB" vals) = addStones Black vals

  go (Prop "B" vals) = addStones Black vals

  go (Prop "AW" vals) = addStones White vals

  go (Prop "W" vals) = addStones White vals

  go (Prop name _) = tell [ "Unknown property " <> name ] *> get

  addStones ∷ Color → List Value → Loader Game
  addStones color vals = case for vals valPoint of
    Just points →
      modify
        ( \game → case color of
            Black → game { black = game.black { stones = game.black.stones <> points } }
            White → game { white = game.white { stones = game.white.stones <> points } }
        )
    Nothing → throwError ("Invalid stones value for " <> show color)

  valPoint ∷ Value → Maybe Coord
  valPoint (Point x y) = Just (Coord x y)

  valPoint _ = Nothing

load ∷ SGF → Either Error (Tuple Game Log)
load sgf = unwrap $ runExceptT $ runWriterT $ execStateT (loader (flatten sgf)) initGame

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
  getSize = head xs >>= \x -> pure $ length x

  go :: Int -> List String -> List Stone
  go _ Nil = Nil

  go pos (Cons x rest) = (catMaybes $ fromFoldable $ mapWithIndex (goRow pos) (toCodePointArray x)) <> (go (pos + 1) rest)

  goRow :: Int -> Int -> CodePoint -> Maybe Stone
  goRow x y cp
    | cp == codePointFromChar 'b' = pure $ Stone Black (Coord x y)
    | cp == codePointFromChar 'w' = pure $ Stone White (Coord x y)
    | otherwise = Nothing

getBoard ∷ Game → Array Position
getBoard game@{ size: size, black: bplayer, white: wplayer } = reverse (create (game.size * game.size))
  where
  create ∷ Int → Array Position
  create 0 = []

  create n = createCell (idxToCoord size n) `cons` create (n - 1)

  createCell coord
    | coord `elem` bplayer.stones = Occupied Black
    | coord `elem` wplayer.stones = Occupied White
    | otherwise = Empty

showBoard :: Game -> String
showBoard game = go (getBoard game)
  where
  go [] = ""

  go xs = show (take game.size xs) <> "\n" <> go (drop game.size xs)
