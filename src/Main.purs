module Main (main) where

import Prelude
import Data.Array (intercalate)
import Data.List (List(..), mapWithIndex, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Editor as Editor
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type Slots
  = ( editor :: forall query. H.Slot query Void Int )

editor = SProxy :: SProxy "editor"

defaultGames :: List String
defaultGames =
  Cons
    ( intercalate "\n"
        [ "(;GM[1]FF[4]"
        , "SZ[5]"
        , "DT[2020-11-08]"
        , "AP[GNU Go:3.9.1]"
        , ";B[bb]C[load and analyze mode];W[bc]C[load and analyze mode];B[db]"
        , ";B[bd]W[ee]"
        , "C[load and analyze mode])"
        ]
    )
    Nil

-- Halogen entrypoint
main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI badukTrainer { trainingGames: defaultGames } body

type Input
  = { trainingGames :: List String }

data Mode
  = ShowGames
  | EditGame String
  | PlayGame String

data Action
  = SwitchMode Mode

isHome :: Mode -> Boolean
isHome = case _ of
  ShowGames -> true
  _ -> false

homeNavClass :: Mode -> String
homeNavClass m = case isHome m of
  true -> " active"
  false -> ""

type State
  = { trainingGames :: List String, mode :: Mode }

badukTrainer :: forall query output m. MonadEffect m => H.Component HH.HTML query Input output m
badukTrainer =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState { trainingGames } = { trainingGames, mode: ShowGames }

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchMode mode -> H.modify_ \s -> s { mode = mode }

-- Render
render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (ClassName "container") ]
    (nav <> body)
  --    (toUnfoldable $ map (\game -> HH.slot editor 0 Editor.component { game } absurd) state.games)
  where
  nav =
    [ HH.nav
        [ HP.class_ (ClassName "navbar navbar-expand-lg navbar-light bg-light") ]
        [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "pure-gnugo" ]
        , HH.div
            [ HP.class_ (ClassName "navbar-nav") ]
            [ HH.a [ HP.class_ (ClassName ("nav-link" <> homeNavClass state.mode)), clk ShowGames, HP.href "#" ] [ HH.text "Home" ] ]
        ]
    ]

  body = case state.mode of
    ShowGames -> [ HH.h1_ [ HH.text "Select a training game" ] ] <> (toUnfoldable $ mapWithIndex renderGamePicker state.trainingGames)
    EditGame s -> [ HH.slot editor 0 Editor.component s absurd ]
    PlayGame n -> [ HH.text "playing game" ]

  clk mode = HE.onClick \e -> Just (SwitchMode mode)

  renderGamePicker idx gameStr =
    HH.div
      [ HP.class_ (ClassName "card") ]
      [ HH.div
          [ HP.class_ (ClassName "card-body") ]
          [ Editor.renderMignature "auto" gameStr
          , HH.a
              [ HP.class_ (ClassName "btn btn-primary")
              , clk (PlayGame gameStr)
              ]
              [ HH.text "play" ]
          , HH.a
              [ HP.class_ (ClassName "btn btn-secondary")
              , clk (EditGame gameStr)
              ]
              [ HH.text "edit" ]
          ]
      ]
