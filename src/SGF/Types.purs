module SGF.Types where

import Data.Eq (class Eq)
import Data.List (List)
import Prelude (class Ord, class Show, show, (<>))

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
  | Text String
  | None

instance showSGF :: Show GameTree where
  show (GameTree seq gs) = "A GameTree [" <> show seq <> "]; " <> show gs

instance showValue :: Show Value where
  show (Num n) = "Num " <> show n
  show (Point x y) = "Point " <> show x <> ":" <> show y
  show (Date _ _ _) = "Date"
  show (Color c) = "Color " <> show c
  show (Text t) = "Text " <> show t
  show None = "None"

instance showProp ∷ Show Property where
  show (Prop name vals) = name <> " = " <> show vals

instance showColor ∷ Show Color where
  show Black = "B"
  show White = "W"

derive instance eqColor :: Eq Color

derive instance ordColor :: Ord Color
