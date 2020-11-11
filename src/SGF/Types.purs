module SGF.Types where

import Control.Semigroupoid ((<<<))
import Data.List (List(..), concat, concatMap, null, singleton, (:))
import Data.Monoid ((<>))
import Prelude (class Show, map, otherwise, show)
import Data.Eq

-- https://www.red-bean.com/sgf/sgf4.html
type SGF
  = Collection

type Collection
  = List GameTree

data GameTree
  = GameTree Sequence (List GameTree)

type Sequence
  = (List Node)

type Node
  = (List Property)

data Property
  = Prop String (List Value)

data Color
  = Black
  | White

data Value
  = Num Number
  | Point Int Int
  | Date Number Number Number
  | Color Color
  | None

instance showSGF :: Show GameTree where
  show (GameTree seq gs) = "A GameTree [" <> show seq <> "]; " <> show gs

instance showValue :: Show Value where
  show _ = "A Value"

instance showProp ∷ Show Property where
  show (Prop name vals) = name <> " = " <> show vals

instance showColor ∷ Show Color where
  show Black = "B"
  show White = "W"

derive instance eqColor :: Eq Color

showHexColor :: Color -> String
showHexColor Black = "#000"

showHexColor White = "#FFF"

type FlatSGF
  = List (List Property)

-- A simpler representation of the SGF trees
flatten ∷ SGF → FlatSGF
flatten = flattenSequence <<< concatMap flattenGameTree
  where
  flattenSequence ∷ List Sequence → List (List Property)
  flattenSequence = map concat

  flattenGameTree ∷ GameTree → List Sequence
  flattenGameTree (GameTree seq gametrees)
    | null gametrees = singleton seq
    | otherwise = seq : concatMap flattenGameTree gametrees

-- A SGF instance for testing purpose
demo ∷ SGF
demo = game : Nil
  where
  game ∷ GameTree
  game = GameTree seq Nil

  seq ∷ Sequence
  seq = (size : Nil) : (pos Black : Nil) : (player Black : Nil) : Nil

  size ∷ Property
  size = Prop "SZ" (Num 5.0 : Nil)

  pos ∷ Color → Property
  pos col = Prop (colorName col) (Point 1 1 : Point 2 1 : Nil)
    where
    colorName Black = "AB"

    colorName White = "AW"

  player ∷ Color → Property
  player col = Prop "PL" (Color col : Nil)
