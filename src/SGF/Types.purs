module SGF.Types where

import Data.List (List)
import Prelude (class Show)

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
  | Color Color
  | None

instance showSGF :: Show GameTree where
  show _ = "A GameTree"

instance showValue :: Show Value where
  show _ = "A Value"
