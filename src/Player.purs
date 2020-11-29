module Player where

import Prelude
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML as HH
import Effect.Class (class MonadEffect)

type Input
  = String

type Output
  = Maybe Result

type Slot id
  = forall query. H.Slot query Output id

data Result
  = Win
  | Loss

instance showResult :: Show Result where
  show Win = "win"
  show Loss = "loss"

component :: forall query m. MonadEffect m => H.Component HH.HTML query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

type State
  = { initialGame :: String }

initialState :: Input -> State
initialState gameStr = { initialGame: gameStr }

data Action
  = Resign

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
render state = HH.text "player"

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Output m Unit
handleAction _ = pure unit
