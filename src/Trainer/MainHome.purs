module Trainer.MainHome (component) where

import Prelude
import Baduk (Game, Result(..), loadBaduk)
import Data.List (List(..), catMaybes, mapWithIndex, toUnfoldable, zipWith, (:))
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

data Mode
  = ShowGames
  | EditGame Int String
  | PlayGame Int String
  | Loading

data Action
  = SwitchMode Mode
  | Edited Int (Maybe String)
  | Played Int String (Maybe Result)
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
    , completed :: Boolean
    }

type State
  = { trainingGames :: List TrainingGame, gnugo :: Maybe GnuGO.WASM, mode :: Mode }

-- Check https://senseis.xmp.net/?HandicapForSmallerBoardSizes
defaultGames :: List String
defaultGames =
  "(;GN[Capture]SZ[5]PL[B];AB[bb][db][bd][ac][dd][ec];AW[bc][dc];)"
    : "(;GN[Intro]SZ[6]PL[B];AB[bb][db][bd][cc];AW[bc][ee];)"
    -- Simple ko testing game

    -- : "(;GN[TestKo]SZ[5]KM[6.5]PL[B];AB[bc][ac][cb][dc][ec][bb][db];AW[cc][bd][ad][dd][ed][ce];)"

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

component :: forall query input output m. MonadAff m => MonadEffect m => H.Component HH.HTML query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

mkTrainingGames :: forall id. Show id => Boolean -> id -> String -> Maybe TrainingGame
mkTrainingGames completed idx sgf = case loadBaduk sgf of
  Just game -> Just { sgf, game, completed, id: "train-" <> show idx }
  _ -> Nothing

initialState :: forall input. input -> State
initialState _ = { trainingGames: games, gnugo: Nothing, mode: Loading }
  where
  games = catMaybes (mapWithIndex (mkTrainingGames false) defaultGames)

replaceGame :: Int -> Maybe TrainingGame -> List TrainingGame -> List TrainingGame
replaceGame _ Nothing l = l

replaceGame idx (Just tg) l = go 0 l
  where
  go :: Int -> List TrainingGame -> List TrainingGame
  go n = case _ of
    Nil -> Nil
    Cons x xs -> Cons (if n == idx then tg else x) (go (n + 1) xs)

handleAction :: forall output m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchMode mode -> (H.modify \s -> s { mode = mode }) >>= redraw
  Edited idx maybeGame -> do
    state <- H.get
    let
      newGames = case maybeGame of
        Just g -> case mkTrainingGames false idx g of
          Just g' -> replaceGame idx (Just g') state.trainingGames
          Nothing -> state.trainingGames
        Nothing -> state.trainingGames
    (H.modify \s -> s { trainingGames = newGames, mode = ShowGames }) >>= redraw
  Played idx gameSgf maybeResult -> do
    liftEffect $ log ("Played " <> show idx <> " : " <> show maybeResult)
    state <- H.get
    let
      trainingGames = case maybeResult of
        Just Win -> replaceGame idx (mkTrainingGames true idx gameSgf) state.trainingGames
        _ -> state.trainingGames
    (H.modify \s -> s { mode = ShowGames, trainingGames = trainingGames }) >>= redraw
  Initialize -> do
    gnugo <- H.liftAff $ GnuGO.get "/wasm-gnugo/gnugo.wasm"
    (H.modify \s -> s { gnugo = Just gnugo, mode = ShowGames }) >>= redraw
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

  info =
    HH.div
      [ HP.class_ (ClassName "alert alert-warning") ]
      [ HH.text ("This application is currently beta, some features are ")
      , HH.a [ HP.href "https://github.com/TristanCacqueray/baduk-trainer#features" ] [ HH.text "missing" ]
      , HH.text (". Checkout the ")
      , HH.a [ HP.href "https://senseis.xmp.net/?RulesOfGoIntroductory" ] [ HH.text "rules of baduk" ]
      , HH.text (" to learn the basics first.")
      ]

  body = case state.mode of
    Loading -> [ HH.text "loading..." ]
    ShowGames ->
      [ info
      , HH.h1_
          [ HH.text "Select a training game" ]
      , HH.div
          [ HP.class_ (ClassName "row") ]
          (toUnfoldable $ mapWithIndex renderGamePicker state.trainingGames)
      ]
    EditGame n s -> [ HH.slot editor unit Editor.component { sgfStr: s, gnugo: state.gnugo } (Just <<< Edited n) ]
    PlayGame n s -> case loadBaduk s of
      Just g -> [ HH.slot player unit Player.component { game: g, gnugo: state.gnugo } (Just <<< Played n s) ]
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
                  [ HP.class_ (ClassName ("btn btn-" <> if tg.completed then "success" else "primary"))
                  , clk (PlayGame idx tg.sgf)
                  ]
                  [ HH.text (if tg.completed then "replay" else "play") ]
              , HH.a
                  [ HP.class_ (ClassName "btn btn-secondary")
                  , clk (EditGame idx tg.sgf)
                  ]
                  [ HH.text "edit" ]
              ]
          ]
      ]
