-- | GnuGO wasm binding, need: https://github.com/TristanCacqueray/wasm-gnugo
module GnuGO
  ( WASM
  , get
  , play
  , score
  ) where

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data WASM :: Type

foreign import getP ::
  Fn3
    (String -> Either String WASM)
    (WASM -> Either String WASM)
    String
    (Effect (Promise (Either String WASM)))

foreign import playU :: Fn3 WASM Int String (Effect (Promise String))

foreign import scoreU :: Fn3 WASM Int String (Effect (Promise Number))

get :: String -> Aff (Either String WASM)
get url = toAffE (runFn3 getP Left Right url)

play :: WASM -> Int -> String -> Aff String
play wasm seed sgf = toAffE (runFn3 playU wasm seed sgf)

score :: WASM -> Int -> String -> Aff Number
score wasm seed sgf = toAffE (runFn3 scoreU wasm seed sgf)
