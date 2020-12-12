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
import Prelude ((<<<))

foreign import data WASM :: Type

foreign import getP ::
  Fn3
    (String -> Either String WASM)
    (WASM -> Either String WASM)
    String
    ( Effect
        (Promise (Either String WASM))
    )

foreign import playU :: Fn3 WASM Int String String

foreign import scoreU :: Fn3 WASM Int String Number

get :: String -> Aff (Either String WASM)
get = toAffE <<< getC Left Right
  where
  getC = runFn3 getP

play :: WASM -> Int -> String -> String
play = runFn3 playU

score :: WASM -> Int -> String -> Number
score = runFn3 scoreU
