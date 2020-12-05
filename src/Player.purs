module Player where

import Prelude
import Baduk (Game) as Baduk
import Baduk.Game (save, setStone) as Baduk
import Baduk.Game as Baduk
import Baduk.Game as Badul
import Baduk.Types (Coord, Stone(..))
import Data.List (List(..), length)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Editor as Editor
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import GnuGO as GnuGO
import Graphics.Canvas (getCanvasElementById)
import Graphics.Canvas as Canvas
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SGF as SGF
import Web.UIEvent.MouseEvent (MouseEvent)

type Input
  = { game :: Baduk.Game
    , gnugo :: Maybe GnuGO.WASM
    }

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

component :: forall query m. MonadAff m => MonadEffect m => H.Component HH.HTML query Input Output m
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
    , gnugo :: Maybe GnuGO.WASM
    , game :: Baduk.Game
    , editCoord :: Maybe Coord
    , status :: Status
    }

initialState :: Input -> State
initialState input =
  { initialGame: input.game
  , gnugo: input.gnugo
  , game: Badul.setAliveStones input.game
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

    item txt =
      HH.span_
        [ HH.text txt ]

    showCapture name player = case player.captures of
      Nil -> []
      captures -> [ item (name <> " captures: " <> (show $ length captures)) ]

    infos =
      HH.div_
        ( [ item ("Move: " <> show state.game.move) ]
            <> showCapture "Black" state.game.black
            <> showCapture "White" state.game.white
        )
  in
    HH.div
      [ HP.class_ (ClassName "container") ]
      [ HH.h1_
          [ HH.text "Baduk Trainer" ]
      , HH.h4_
          [ HH.text ("Playing: " <> state.game.name) ]
      , HH.p_
          [ HH.text ("Status: " <> message) ]
      , HH.div
          [ HP.class_ (ClassName "col") ]
          [ HH.div_
              [ board, infos ]
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

aiPlay :: GnuGO.WASM -> Baduk.Game -> Effect Baduk.Game
aiPlay gnugo game = do
  let
    gameStr = Baduk.save game
  log ("sending: " <> gameStr)
  let
    newGameStr = GnuGO.play gnugo 0 gameStr
  log ("received: " <> newGameStr)
  case SGF.loadBaduk newGameStr of
    -- revert starting player inversion
    Just g -> do
      log ("loaded game: " <> show g)
      case Baduk.getLastMove g of
        Just move -> do
          log ("adding: " <> show move)
          case Baduk.addStone move game of
            Just newGame -> pure newGame
            Nothing -> do
              log ("No new-move?!")
              pure game
        Nothing -> do
          log ("No move?!")
          pure game
    Nothing -> do
      log ("Couldn't load")
      pure game

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Resign -> H.raise (Just Loss)
  Restart -> do
    state <- H.get
    H.modify_ \_ -> initialState { game: state.initialGame, gnugo: state.gnugo }
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
      Just coord -> case Badul.addStone (Stone state.game.startingPlayer coord) state.game of
        Just newGame -> do
          H.modify_ \s -> s { game = newGame, status = WaitingAI }
          drawBoard
          _ <- do
            H.fork do
              -- This delay seems to help halogen render the waiting ai message
              -- Otherwise it skip the update and only show waiting for humang again
              -- as if wasm is freezing the rendering loop
              H.liftAff (delay $ Milliseconds 10.0)
              newGame' <- case state.gnugo of
                Nothing -> do
                  H.liftAff (delay $ Milliseconds 1000.0)
                  -- Fake a move
                  pure newGame
                Just gnugo -> do
                  g <- H.liftAff $ liftEffect $ aiPlay gnugo newGame
                  pure g
              H.modify_ \s -> s { game = newGame', status = WaitingHuman }
              drawBoard
          pure unit
        Nothing -> do
          liftEffect $ log "Invalid move"
          pure unit
  MouseLeave -> do
    H.modify_ \s -> s { editCoord = Nothing }
    drawBoard
  _ -> pure unit
  where
  drawBoard = do
    state <- H.get
    liftEffect do
      -- log ("Drawing: " <> show state.game)
      canvas <- getCanvasElementById "trainer-board"
      case canvas of
        Just boardCanvas -> do
          let
            select = case Tuple state.status state.editCoord of
              Tuple WaitingHuman (Just coord) -> Just (Tuple coord (Just state.game.startingPlayer))
              _ -> Nothing
          boardCtx <- Canvas.getContext2D boardCanvas
          Editor.renderBoard boardCtx select state.game
        Nothing -> log "Where is the canvas?!"
