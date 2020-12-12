module Main (main) where

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude
import Trainer.MainHome (component)

-- Halogen entrypoint
main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    runUI component unit body
