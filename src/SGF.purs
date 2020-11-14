module SGF where

import Prelude (const, (>>=))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Baduk.Types (Game)
import Baduk.Converter (load)
import SGF.Parser

mapError :: forall a b ok. (a -> b) -> Either a ok -> Either b ok
mapError f e = case e of
  Right x -> Right x
  Left x -> Left (f x)

loadBaduk :: String -> Maybe Game
loadBaduk s = case mapError (const "error") (parse s) >>= load of
  Right (Tuple game log) -> Just game
  _ -> Nothing
