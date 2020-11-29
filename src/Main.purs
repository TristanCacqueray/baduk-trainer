module Main where

import Prelude
import Baduk.Types (Game)
import Data.Array (intercalate)
import Data.List (List(..), toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Editor as Editor
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import SGF (loadBaduk)

type Slots
  = ( editor :: forall query. H.Slot query Void Int )

editor = SProxy :: SProxy "editor"

main :: Effect Unit
main = do
  case loadBaduk sgfStr of
    Just g -> do
      log $ show g
      main' g
    Nothing -> log "oops"
  where
  sgfStr =
    intercalate "\n"
      [ "(;GM[1]FF[4]"
      , "SZ[5]"
      , "DT[2020-11-08]"
      , "AP[GNU Go:3.9.1]"
      , ";B[bb]C[load and analyze mode];W[bc]C[load and analyze mode];B[db]"
      , ";B[bd]W[ee]"
      , "C[load and analyze mode])"
      ]

-- Halogen entrypoint
main' :: Game -> Effect Unit
main' game =
  runHalogenAff do
    body <- awaitBody
    runUI badukTrainer { games: (Cons game Nil) } body

type Input
  = { games :: List Game }

type State
  = { games :: List Game }

badukTrainer :: forall query output m. MonadEffect m => H.Component HH.HTML query Input output m
badukTrainer =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

initialState :: Input -> State
initialState games = games

-- Render
render :: forall action m. MonadEffect m => State -> H.ComponentHTML action Slots m
render state =
  HH.div
    [ HP.class_ (ClassName "container") ]
    (toUnfoldable $ map (\game -> HH.slot editor 0 Editor.component { game } absurd) state.games)
