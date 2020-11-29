module Player where

import Prelude
import Baduk as Baduk
import Baduk.Game as Baduk
import Baduk.Types (Coord(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Editor as Editor
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Graphics.Canvas (getCanvasElementById)
import Graphics.Canvas as Canvas
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SGF.Types (Color(..))
import Web.UIEvent.MouseEvent (MouseEvent)

type Input
  = Baduk.Game

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
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Draw }
    }

data Status
  = WaitingAI
  | WaitingHuman

type State
  = { initialGame :: Baduk.Game
    , game :: Baduk.Game
    , editCoord :: Maybe Coord
    , status :: Status
    }

initialState :: Input -> State
initialState game =
  { initialGame: game
  , game: game
  , editCoord: Nothing
  , status: WaitingHuman
  }

data Action
  = Pass
  | Resign
  | Restart
  | AddStone MouseEvent
  | MouseMove MouseEvent
  | MouseLeave
  | Draw

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
render state =
  let
    message = case state.status of
      WaitingAI -> "GnuGO is playing"
      WaitingHuman -> "Your turn to play, place a stone"

    boardSize' = Editor.boardSize state.game.size

    board =
      HH.canvas
        [ HP.id_ "trainer-board"
        , HP.width boardSize'
        , HP.height boardSize'
        , HP.prop (PropName "style") "border: 1px solid black"
        , HE.onClick \e -> Just $ AddStone e
        , HE.onMouseMove \e -> Just $ MouseMove e
        , HE.onMouseLeave \e -> Just MouseLeave
        ]
  in
    HH.div
      [ HP.class_ (ClassName "container") ]
      [ HH.h1_
          [ HH.text "Baduk Trainer" ]
      , HH.p_
          [ HH.text ("Status: " <> message) ]
      , HH.div
          [ HP.class_ (ClassName "col") ]
          [ board
          , HH.div_
              [ HH.a
                  [ HP.class_ (ClassName "btn btn-primary"), HE.onClick \s -> Just $ Pass ]
                  [ HH.text "Pass" ]
              , HH.a
                  [ HP.class_ (ClassName "btn btn-secondary"), HE.onClick \s -> Just $ Restart ]
                  [ HH.text "Restart" ]
              , HH.a
                  [ HP.class_ (ClassName "btn btn-danger"), HE.onClick \s -> Just $ Resign ]
                  [ HH.text "Resign" ]
              ]
          ]
      ]

aiPlay :: Baduk.Game -> Effect Baduk.Game
aiPlay g = pure g

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Resign -> H.raise (Just Loss)
  Restart -> do
    state <- H.get
    H.modify_ \_ -> initialState state.initialGame
    drawBoard
  Draw -> drawBoard
  MouseMove e -> do
    state <- H.get
    case state.status of
      WaitingAI -> pure unit
      WaitingHuman -> do
        mCoord <- liftEffect $ Editor.mouseCoord e
        case mCoord /= state.editCoord of
          false -> pure unit
          true -> do
            H.modify_ \s -> s { editCoord = mCoord }
            case mCoord of
              Nothing -> pure unit
              Just _ -> drawBoard
  AddStone e -> do
    state <- H.get
    case state.editCoord of
      Nothing -> pure unit
      Just coord -> do
        let
          newGame = Baduk.setStone state.game coord Black
        H.modify_ \s -> s { game = newGame, status = WaitingAI }
        game <- liftEffect $ aiPlay newGame
        H.modify_ \s -> s { game = newGame, status = WaitingHuman }
  MouseLeave -> do
    H.modify_ \s -> s { editCoord = Nothing }
    drawBoard
  _ -> pure unit
  where
  drawBoard = do
    state <- H.get
    liftEffect do
      canvas <- getCanvasElementById "trainer-board"
      case canvas of
        Just boardCanvas -> do
          let
            select = case Tuple state.status state.editCoord of
              Tuple WaitingHuman (Just coord) -> Just (Tuple coord (Just Black))
              _ -> Nothing
          boardCtx <- Canvas.getContext2D boardCanvas
          Editor.renderBoard boardCtx select state.game
        Nothing -> log "Where is the canvas?!"
