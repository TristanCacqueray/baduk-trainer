module Main (main) where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import GnuGO as GnuGO
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude
import Trainer.MainHome (component)

-- Check https://senseis.xmp.net/?HandicapForSmallerBoardSizes
defaultGames :: List String
defaultGames =
  "(;GN[Capture]SZ[5]PL[B];AB[bb][db][bd][ac][dd][ec];AW[bc][dc];)"
    : "(;GN[Intro]SZ[6]PL[B];AB[bb][db][bd][cc];AW[bc][ee];)"
    : "(;GN[Five]SZ[5]PL[B];AB[cc];)"
    : "(;GN[Six]SZ[6]PL[B];AB[eb][be];))"
    : "(;GN[Test]SZ[6]PL[B]KM[2.5];AB[bb][eb][be][bd][bc][bf][cb][db][fb][aa];AW[cd][dc][ed][de][ce][cf][ec][fc][cc];)"
    : "(;GN[Eight]SZ[8]PL[B];AB[cc][ff];)"
    : "(;GN[Medium]SZ[6]PL[B];AB[bb][eb][b;)"
    : "(;GN[K25-9]SZ[9]PL[B];AB[cc][gc][gg][cg])"
    : "(;GN[K23-9]SZ[9]PL[B];AB[cc][gc][gg])"
    : "(;GN[K19-9]SZ[9]PL[B];AB[cc][gg])"
    : "(;GN[K25-13]SZ[13]PL[B];AB[cc][kc][kk][ck][gc][gk][gg])"
    : "(;GN[K23-13]SZ[13]PL[B];AB[dd][jd][jj][dj][dg][jg])"
    : "(;GN[K21-13]SZ[13]PL[B]AB[dd][jd][jj][dj][gg])"
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
