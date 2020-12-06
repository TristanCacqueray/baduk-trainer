module Main (main) where

import Data.Array (intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import GnuGO as GnuGO
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude
import Trainer.MainHome (component)

defaultGames :: List String
defaultGames =
  ( intercalate "\n"
      [ "(;GN[Basic]SZ[6]"
      , ";AB[bb][db][bd][cc]"
      , ";AW[bc][ee]"
      , ";PL[B])"
      ]
  )
    : "(;GN[Medium]SZ[6]PL[B];AB[bb][eb][be];)"
    : Nil

-- Halogen entrypoint
mainWasm :: Maybe GnuGO.WASM -> Effect Unit
mainWasm wasm = do
  runHalogenAff do
    body <- awaitBody
    runUI component { trainingGames: defaultGames, gnugo: wasm } body

main :: Effect Unit
main = mainL
  where
  mainL = GnuGO.withWasm "/wasm-gnugo/gnugo.wasm" (\m -> mainWasm (Just m))

  mainQ = mainWasm Nothing
