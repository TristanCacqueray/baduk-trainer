module Trainer.MainHome (component) where

import Prelude
import Baduk (Result, Game, loadBaduk)
import Data.List (List(..), catMaybes, mapWithIndex, toUnfoldable, zipWith)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import GnuGO as GnuGO
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Graphics.Canvas as Canvas
import Halogen (PropName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Trainer.Board (boardSize, renderMiniBoard)
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
  | Initialize

isHome :: Mode -> Boolean
isHome = case _ of
  ShowGames -> true
  _ -> false

homeNavClass :: Mode -> String
homeNavClass m = case isHome m of
  true -> " active"
  false -> ""

type TrainingGame
  = { sgf :: String
    , game :: Game
    , id :: String
    }

type State
  = { trainingGames :: List TrainingGame, gnugo :: Maybe GnuGO.WASM, mode :: Mode }

component :: forall query output m. MonadAff m => MonadEffect m => H.Component HH.HTML query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

mkTrainingGames :: forall id. Show id => id -> String -> Maybe TrainingGame
mkTrainingGames idx sgf = case loadBaduk sgf of
  Just game -> Just { sgf, game, id: "train-" <> show idx }
  _ -> Nothing

initialState :: Input -> State
initialState { trainingGames, gnugo } = { trainingGames: games, gnugo, mode: ShowGames }
  where
  games = catMaybes (mapWithIndex mkTrainingGames trainingGames)

replaceGame :: Int -> TrainingGame -> List TrainingGame -> List TrainingGame
replaceGame idx tg = go 0
  where
  go :: Int -> List TrainingGame -> List TrainingGame
  go n = case _ of
    Nil -> Nil
    Cons x xs -> Cons (if n == idx then tg else x) (go (n + 1) xs)

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchMode mode -> (H.modify \s -> s { mode = mode }) >>= redraw
  Edited idx maybeGame -> do
    state <- H.get
    let
      newGames = case maybeGame of
        Just g -> case mkTrainingGames idx g of
          Just g' -> replaceGame idx g' state.trainingGames
          Nothing -> state.trainingGames
        Nothing -> state.trainingGames
    (H.modify \s -> s { trainingGames = newGames, mode = ShowGames }) >>= redraw
  Played idx maybeResult -> do
    liftEffect $ log ("Played " <> show idx <> " : " <> show maybeResult)
    (H.modify \s -> s { mode = ShowGames }) >>= redraw
  Initialize -> H.get >>= redraw
  where
  redraw state =
    liftEffect do
      canvases' <- getCanvases state
      case canvases' of
        Just canvases -> do
          c <- traverse Canvas.getContext2D canvases
          _ <- traverse (uncurry renderMiniBoard) (zipWith (\ctx tg -> Tuple ctx tg.game) c state.trainingGames)
          pure unit
        Nothing -> log "Where are the canvases?!"

  getCanvases :: State -> Effect (Maybe (List CanvasElement))
  getCanvases state =
    sequence
      <$> traverse getCanvasElementById (map (\tg -> tg.id) state.trainingGames)

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
      , HH.div
          [ HP.class_ (ClassName "row") ]
          (toUnfoldable $ mapWithIndex renderGamePicker state.trainingGames)
      ]
    EditGame n s -> [ HH.slot editor unit Editor.component { sgfStr: s, gnugo: state.gnugo } (Just <<< Edited n) ]
    PlayGame n s -> case loadBaduk s of
      Just g -> [ HH.slot player unit Player.component { game: g, gnugo: state.gnugo } (Just <<< Played n) ]
      Nothing -> [ HH.text ("Invalid game: " <> s) ]

  clk mode = HE.onClick \e -> Just (SwitchMode mode)

  renderGamePicker idx tg =
    HH.div
      [ HP.class_ (ClassName "card") ]
      [ HH.div
          [ HP.class_ (ClassName "card-body") ]
          [ HH.h5
              [ HP.class_ (ClassName "card-title") ]
              [ HH.text tg.game.name ]
          , HH.div
              [ HP.class_ (ClassName "row") ]
              [ HH.canvas
                  [ HP.id_ tg.id
                  , HP.width (boardSize tg.game.size `div` 2)
                  , HP.height (boardSize tg.game.size `div` 2)
                  , HP.prop (PropName "style") "border: 1px solid black"
                  ]
              ]
          , HH.div
              [ HP.class_ (ClassName "row") ]
              [ HH.a
                  [ HP.class_ (ClassName "btn btn-primary")
                  , clk (PlayGame idx tg.sgf)
                  ]
                  [ HH.text "play" ]
              , HH.a
                  [ HP.class_ (ClassName "btn btn-secondary")
                  , clk (EditGame idx tg.sgf)
                  ]
                  [ HH.text "edit" ]
              ]
          ]
      ]
