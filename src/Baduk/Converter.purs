module Baduk.Converter where

import Baduk.Types (Coord(..), Game, initGame)
import Control.Monad.State (StateT, execStateT, get, modify)
import SGF.Types
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Identity (Identity)
import Data.Int (round)
import Data.List (List(..), uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple)
import Prelude (show, ($), (*>), (<>))

type Error
  = String

type Log
  = Array String

type Loader
  = StateT Game (WriterT Log (ExceptT Error Identity))

loader ∷ FlatSGF → Loader Game
loader fsgf = case onlyHead fsgf of
  Just props → traverse_ go props *> get
  Nothing → throwError "Only single tree game are currently supported"
  where
  onlyHead ∷ ∀ a. List a → Maybe a
  onlyHead xs = case uncons xs of
    Just { head: a, tail: Nil } → Just a
    _ → Nothing

  go ∷ Property → Loader Game
  go (Prop "SZ" (Num n : Nil)) = modify (\game → game { size = round n })

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
