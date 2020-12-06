-- | Smart Game Format module entry point
module SGF
  ( module SGF.Types
  , module SGF.Parser
  -- | utility to simplify sgf representation
  , FlatSGF(..)
  , flatten
  -- | utilities
  , inverseColor
  , showHexColor
  ) where

-- import Baduk.Converter (load)
import Data.List (List, concat, concatMap, null, singleton, (:))
import Prelude (map, otherwise, (<<<))
import SGF.Parser (parse)
import SGF.Types (Color(..), GameTree(..), Property(..), SGF, Sequence, Value(..))

-- utility
inverseColor :: Color -> Color
inverseColor = case _ of
  White -> Black
  Black -> White

showHexColor :: Color -> String
showHexColor = case _ of
  Black -> "#000"
  White -> "#FFF"

-- A simpler representation of the SGF trees without branching
type FlatSGF
  = List (List Property)

flatten ∷ SGF → FlatSGF
flatten = flattenSequence <<< concatMap flattenGameTree
  where
  flattenSequence ∷ List Sequence → List (List Property)
  flattenSequence = map concat

  flattenGameTree ∷ GameTree → List Sequence
  flattenGameTree (GameTree seq gametrees)
    | null gametrees = singleton seq
    | otherwise = seq : concatMap flattenGameTree gametrees
