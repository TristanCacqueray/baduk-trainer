module Baduk.Converter (load) where

import Baduk.Types (Coord(..), Game, Position(..), initGame)
import SGF.Types
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, execStateT, get, modify)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array (cons, reverse)
import Data.Either (Either)
import Data.Foldable (elem, traverse_)
import Data.Identity (Identity)
import Data.Int (round)
import Data.List (List(..), uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple)
import Prelude (mod, otherwise, show, ($), (*), (*>), (+), (-), (/), (<>))

type Error
  = String

type Log
  = Array String

type Loader
  = StateT Game (WriterT Log (ExceptT Error Identity))

loader ∷ FlatSGF → Loader Game
loader fsgf = case onlyHead fsgf of
  Just props → traverse_ go props *> modify addBoard
  Nothing → throwError "Only single tree game are currently supported"
  where
  onlyHead ∷ ∀ a. List a → Maybe a
  onlyHead xs = case uncons xs of
    Just { head: a, tail: Nil } → Just a
    _ → Nothing

  go ∷ Property → Loader Game
  go (Prop "SZ" (Num n : Nil)) = modify (\game → game { size = round n })

  go (Prop "PL" (Color col : Nil)) = modify (\game → game { next = Just col })

  go (Prop "AB" vals) = addStones Black vals

  go (Prop "AW" vals) = addStones White vals

  go (Prop name _) = tell [ "Unknown property " <> name ] *> get

  addStones ∷ Color → List Value → Loader Game
  addStones color vals = case for vals valPoint of
    Just points →
      modify
        ( \game → case color of
            Black → game { black = game.black { stones = points } }
            White → game { white = game.white { stones = points } }
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

addBoard ∷ Game → Game
addBoard game@{ size: size, black: bplayer, white: wplayer } = game { board = reverse (create (game.size * game.size)) }
  where
  create ∷ Int → Array Position
  create 0 = []

  create n = createCell (idxToCoord size n) `cons` create (n - 1)

  createCell coord
    | coord `elem` bplayer.stones = Occupied Black
    | coord `elem` wplayer.stones = Occupied White
    | otherwise = Empty
