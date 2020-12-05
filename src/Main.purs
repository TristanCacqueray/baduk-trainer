module Main (main) where

import Prelude
import Data.Array (intercalate)
import Data.List (List(..), mapWithIndex, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Editor as Editor
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Player as Player
import SGF as SGF
import GnuGO as GnuGO

type Slots
  = ( editor :: Editor.Slot Unit
    , player :: Player.Slot Unit
    )

editor = SProxy :: SProxy "editor"

player = SProxy :: SProxy "player"

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
mainWasm :: Maybe GnuGO.WASM -> Effect Unit
mainWasm wasm = do
  runHalogenAff do
    body <- awaitBody
    runUI badukTrainer { trainingGames: defaultGames, gnugo: wasm } body

main :: Effect Unit
main = mainL
  where
  mainL = GnuGO.withWasm "/wasm-gnugo/gnugo.wasm" (\m -> mainWasm (Just m))

  mainQ = mainWasm Nothing

type Input
  = { trainingGames :: List String, gnugo :: Maybe GnuGO.WASM }

data Mode
  = ShowGames
  | EditGame Int String
  | PlayGame Int String

data Action
  = SwitchMode Mode
  | Edited Int (Maybe String)
  | Played Int (Maybe Player.Result)

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

badukTrainer :: forall query output m. MonadAff m => MonadEffect m => H.Component HH.HTML query Input output m
badukTrainer =
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
        [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "pure-gnugo" ]
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
    PlayGame n s -> case SGF.loadBaduk s of
      Just g -> [ HH.slot player unit Player.component { game: g, gnugo: state.gnugo } (Just <<< Played n) ]
      Nothing -> [ HH.text ("Invalid game: " <> s) ]

  clk mode = HE.onClick \e -> Just (SwitchMode mode)

  renderGamePicker idx gameStr =
    HH.div
      [ HP.class_ (ClassName "card") ]
      [ HH.div
          [ HP.class_ (ClassName "card-body") ]
          [ Editor.renderMignature "auto" gameStr
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
