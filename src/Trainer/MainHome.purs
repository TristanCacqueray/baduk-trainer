module Trainer.MainHome (component) where

import Prelude
import Baduk (Game, Result(..), loadBaduk)
import Bootstrap as Bootstrap
import Data.Either (Either(..))
import Data.List (List(..), catMaybes, index, mapWithIndex, toUnfoldable, zipWith, (:))
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
  | EditGame Int Game
  | PlayGame Int Game

data Action
  = SwitchMode Mode
  | Edited Int (Maybe String)
  | Played Int (Maybe Result)
  | Initialize

gnuGoURL :: String
gnuGoURL = "/wasm-gnugo/gnugo.wasm"

isHome :: Mode -> Boolean
isHome = case _ of
  ShowGames -> true
  _ -> false

homeNavClass :: Mode -> String
homeNavClass m = case isHome m of
  true -> " active"
  false -> ""

type TrainingGame
  = { game :: Game
    , id :: String
    , completed :: Boolean
    }

type State
  = { trainingGames :: List TrainingGame, gnugo :: Maybe (Either String GnuGO.WASM), mode :: Mode }

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
  Just game -> Just { game, completed, id: "train-" <> show idx }
  _ -> Nothing

initialState :: forall input. input -> State
initialState _ = { trainingGames: games, gnugo: Nothing, mode: ShowGames }
  where
  games = catMaybes (mapWithIndex (mkTrainingGames false) defaultGames)

replaceGame :: Int -> TrainingGame -> List TrainingGame -> List TrainingGame
replaceGame idx tg l = go 0 l
  where
  go :: Int -> List TrainingGame -> List TrainingGame
  go n = case _ of
    Nil -> Nil
    Cons x xs -> Cons (if n == idx then tg else x) (go (n + 1) xs)

setCompleted :: List TrainingGame -> Int -> List TrainingGame
setCompleted xs idx = case index xs idx of
  Just tg -> replaceGame idx (tg { completed = true }) xs
  Nothing -> xs

handleAction :: forall output m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SwitchMode mode -> (H.modify \s -> s { mode = mode }) >>= redraw
  Edited idx maybeGame -> do
    state <- H.get
    let
      newGames = case maybeGame of
        Just g -> case mkTrainingGames false idx g of
          Just g' -> replaceGame idx g' state.trainingGames
          Nothing -> state.trainingGames
        Nothing -> state.trainingGames
    (H.modify \s -> s { trainingGames = newGames, mode = ShowGames }) >>= redraw
  Played idx maybeResult -> do
    H.liftEffect $ log ("Played " <> show idx <> " : " <> show maybeResult)
    state <- H.get
    let
      trainingGames = case maybeResult of
        Just Win -> setCompleted state.trainingGames idx
        _ -> state.trainingGames
    (H.modify \s -> s { mode = ShowGames, trainingGames = trainingGames }) >>= redraw
  Initialize -> do
    gnugo <- H.liftAff $ GnuGO.get gnuGoURL
    (H.modify \s -> s { gnugo = Just gnugo }) >>= redraw
  where
  redraw state =
    H.liftEffect do
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
  where
  nav =
    [ HH.nav
        [ HP.class_ (ClassName "navbar navbar-expand-lg navbar-light bg-light") ]
        [ HH.a [ HP.class_ (ClassName "navbar-brand") ] [ HH.text "Baduk Trainer" ]
        , HH.div
            [ HP.class_ (ClassName "navbar-nav") ]
            [ HH.a
                [ HP.class_ (ClassName ("nav-link" <> homeNavClass state.mode))
                , HE.onClick $ clk ShowGames
                , HP.href "#"
                ]
                [ HH.text "Home" ]
            ]
        ]
    ]

  info =
    Bootstrap.card "Welcome to Baduk Trainer"
      [ HH.text "This application is currently beta, some features are "
      , Bootstrap.a "https://github.com/TristanCacqueray/baduk-trainer#features" "missing"
      , HH.text ". Checkout the "
      , Bootstrap.a "https://senseis.xmp.net/?RulesOfGoIntroductory" "rules of baduk"
      , HH.text " to learn the basics first."
      ]

  body = case state.gnugo of
    Nothing -> [ Bootstrap.center [ Bootstrap.spinner ] ]
    Just (Left error) ->
      [ Bootstrap.alertDanger
          [ HH.h4
              [ HP.class_ (ClassName "alert-heading") ]
              [ HH.text $ "Failed to load " <> gnuGoURL ]
          , HH.text error
          , HH.hr_
          , HH.text "Try to refresh or use another browser."
          ]
      ]
    Just (Right gnugo) -> case state.mode of
      ShowGames ->
        [ info
        , HH.h1_
            [ HH.text "Select a training game" ]
        , Bootstrap.row $ toUnfoldable $ mapWithIndex renderGamePicker state.trainingGames
        ]
      EditGame idx game -> [ HH.slot editor unit Editor.component { game, gnugo } (Just <<< Edited idx) ]
      PlayGame idx game -> [ HH.slot player unit Player.component { game, gnugo } (Just <<< Played idx) ]

  clk mode = \e -> Just (SwitchMode mode)

  renderGamePicker idx tg =
    Bootstrap.card tg.game.name
      [ Bootstrap.row [ Bootstrap.canvas tg.id $ boardSize tg.game.size `div` 2 ]
      , Bootstrap.row
          [ Bootstrap.button
              (if tg.completed then "success" else "primary")
              (if tg.completed then "replay" else "play")
              $ clk (PlayGame idx tg.game)
          , Bootstrap.button "seconday" "edit" $ clk (EditGame idx tg.game)
          ]
      ]
