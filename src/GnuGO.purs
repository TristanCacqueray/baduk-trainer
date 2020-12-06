-- | GnuGO wasm binding, need: https://github.com/TristanCacqueray/wasm-gnugo
module GnuGO
  ( WASM
  , withWasm
  , play
  , score
  ) where

import Prelude (Unit)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Effect (Effect)

foreign import data WASM :: Type

foreign import withWasmU :: Fn2 String (WASM -> Effect Unit) (Effect Unit)

foreign import playU :: Fn3 WASM Int String String

foreign import scoreU :: Fn3 WASM Int String Number

withWasm :: String -> (WASM -> Effect Unit) -> Effect Unit
withWasm = runFn2 withWasmU

play :: WASM -> Int -> String -> String
play = runFn3 playU

score :: WASM -> Int -> String -> Number
score = runFn3 scoreU
