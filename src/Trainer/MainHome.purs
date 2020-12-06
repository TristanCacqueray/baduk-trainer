module Trainer.MainHome (component) where

import Prelude
import Baduk (Result, loadBaduk)
import Data.List (List(..), mapWithIndex, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import GnuGO as GnuGO
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Trainer.Editor as Editor
import Trainer.Player as Player

type Slots
  = ( editor :: Editor.Slot Unit
    , player :: Player.Slot Unit
    )

editor = SProxy :: SProxy "editor"

player = SProxy :: SProxy "player"

type Input
  = { trainingGames :: List String, gnugo :: Maybe GnuGO.WASM }

data Mode
  = ShowGames
  | EditGame Int String
  | PlayGame Int String

data Action
  = SwitchMode Mode
  | Edited Int (Maybe String)
  | Played Int (Maybe Result)

isHome :: Mode -> Boolean
isHome = case _ of
  ShowGames -> true
  _ -> false

homeNavClass :: Mode -> String
homeNavClass m = case isHome m of
  true -> " active"
  false -> ""

type State
  = { trainingGames :: List String, gnugo :: Maybe GnuGO.WASM, mode :: Mode }

component :: forall query output m. MonadAff m => MonadEffect m => H.Component HH.HTML query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState { trainingGames, gnugo } = { trainingGames, gnugo, mode: ShowGames }

replaceGame :: Int -> String -> List String -> List String
replaceGame idx gameStr = go 0
  where
  go :: Int -> List String -> List String
  go n = case _ of
    Nil -> Nil
    Cons x xs -> Cons (if n == idx then gameStr else x) (go (n + 1) xs)

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchMode mode -> H.modify_ \s -> s { mode = mode }
  Edited idx maybeGame -> do
    state <- H.get
    let
      newGames = case maybeGame of
        Just g -> replaceGame idx g state.trainingGames
        Nothing -> state.trainingGames
    H.modify_ \s -> s { trainingGames = newGames, mode = ShowGames }
  Played idx maybeResult -> do
    liftEffect $ log ("Played " <> show idx <> " : " <> show maybeResult)
    H.modify_ \s -> s { mode = ShowGames }

-- Render
render :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (ClassName "container") ]
    (nav <> body)
  --    (toUnfoldable $ map (\game -> HH.slot editor 0 Editor.component { game } absurd) state.games)
  where
  nav =
    [ HH.nav
        [ HP.class_ (ClassName "navbar navbar-expand-lg navbar-light bg-light") ]
        [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "Baduk Trainer" ]
        , HH.div
            [ HP.class_ (ClassName "navbar-nav") ]
            [ HH.a
                [ HP.class_ (ClassName ("nav-link" <> homeNavClass state.mode)), clk ShowGames, HP.href "#" ]
                [ HH.text "Home" ]
            ]
        ]
    ]

  body = case state.mode of
    ShowGames ->
      [ HH.h1_
          [ HH.text "Select a training game" ]
      ]
        <> (toUnfoldable $ mapWithIndex renderGamePicker state.trainingGames)
    EditGame n s -> [ HH.slot editor unit Editor.component s (Just <<< Edited n) ]
    PlayGame n s -> case loadBaduk s of
      Just g -> [ HH.slot player unit Player.component { game: g, gnugo: state.gnugo } (Just <<< Played n) ]
      Nothing -> [ HH.text ("Invalid game: " <> s) ]

  clk mode = HE.onClick \e -> Just (SwitchMode mode)

  renderGamePicker idx gameStr = case loadBaduk gameStr of
    Just game ->
      HH.div
        [ HP.class_ (ClassName "card") ]
        [ HH.div
            [ HP.class_ (ClassName "card-body") ]
            [ HH.h5
                [ HP.class_ (ClassName "card-title") ]
                [ HH.text game.name ]
            , Editor.renderMignature "auto" gameStr
            , HH.a
                [ HP.class_ (ClassName "btn btn-primary")
                , clk (PlayGame idx gameStr)
                ]
                [ HH.text "play" ]
            , HH.a
                [ HP.class_ (ClassName "btn btn-secondary")
                , clk (EditGame idx gameStr)
                ]
                [ HH.text "edit" ]
            ]
        ]
    Nothing -> HH.div_ [ HH.text "Unknown game.." ]