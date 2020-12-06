-- | Baduk game module re-exported entry point
module Baduk
  ( module Baduk.Types
  , module Baduk.Converter
  , module Baduk.Game
  , loadBaduk
  ) where

import Baduk.Converter (load, save)
import Baduk.Types (Game, Coord(..), Stone(..), Result(..), Move(..), initGame, initPlayer)
import Baduk.Game (initAliveStones, getLastMove, addMove, addStone, setStone, removeStone)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (const, (>>=))
import SGF (parse)

mapError :: forall a b ok. (a -> b) -> Either a ok -> Either b ok
mapError f e = case e of
  Right x -> Right x
  Left x -> Left (f x)

loadBaduk :: String -> Maybe Game
loadBaduk s = case mapError (const "error") (parse s) >>= load of
  Right (Tuple game log) -> Just game
  _ -> Nothing
