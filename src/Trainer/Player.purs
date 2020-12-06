module Trainer.Player (Input, Slot, Output, component) where

import Prelude
import Baduk (Coord, Game, Move(..), Result(..), addMove, getLastMove, initAliveStones, loadBaduk, save)
import Baduk.Types (PlayerMove(..))
import Data.List (List(..), length)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
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
import Trainer.Board (boardSize, mouseCoord, renderBoard)
import Web.UIEvent.MouseEvent (MouseEvent)

type Input
  = { game :: Game
    , gnugo :: Maybe GnuGO.WASM
    }

type Output
  = Maybe Result

type Slot id
  = forall query. H.Slot query Output id

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
  = { initialGame :: Game
    , gnugo :: Maybe GnuGO.WASM
    , game :: Game
    , editCoord :: Maybe Coord
    , status :: Status
    }

initialState :: Input -> State
initialState input =
  { initialGame: input.game
  , gnugo: input.gnugo
  , game: initAliveStones input.game
  , editCoord: Nothing
  , status: WaitingHuman
  }

data Action
  = DoPass
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

    boardSize' = boardSize state.game.size

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
                  [ HP.class_ (ClassName "btn btn-primary"), HE.onClick \s -> Just $ DoPass ]
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

aiPlay :: GnuGO.WASM -> Game -> Effect Game
aiPlay gnugo game = do
  let
    gameStr = save game
  log ("sending: " <> gameStr)
  let
    newGameStr = GnuGO.play gnugo 0 gameStr
  log ("received: " <> newGameStr)
  case loadBaduk newGameStr of
    -- revert starting player inversion
    Just g -> do
      log ("loaded game: " <> show g)
      case getLastMove g of
        Just move -> do
          log ("adding: " <> show move)
          case addMove move game of
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
        mCoord <- liftEffect $ mouseCoord e
        case mCoord /= state.editCoord of
          false -> pure unit
          true -> do
            H.modify_ \s -> s { editCoord = mCoord }
            case mCoord of
              Nothing -> pure unit
              Just _ -> drawBoard
  MouseLeave -> do
    H.modify_ \s -> s { editCoord = Nothing }
    drawBoard
  AddStone e -> do
    state <- H.get
    case state.editCoord of
      Nothing -> pure unit
      Just coord -> play (PlaceStone coord) state
  DoPass -> H.get >>= play Pass
  where
  play :: PlayerMove -> State -> H.HalogenM State Action () Output m Unit
  play move state = case addMove (Move state.game.startingPlayer move) state.game of
    Just newGame -> do
      playAi state.gnugo newGame
    Nothing -> do
      liftEffect $ log "Invalid move"
      pure unit

  playAi :: Maybe GnuGO.WASM -> Game -> H.HalogenM State Action () Output m Unit
  playAi gnugo' game = do
    H.modify_ \s -> s { game = game, status = WaitingAI }
    drawBoard
    _ <- do
      H.fork do
        -- This delay seems to help halogen render the waiting ai message
        -- Otherwise it skip the update and only show waiting for humang again
        -- as if wasm is freezing the rendering loop
        H.liftAff (delay $ Milliseconds 10.0)
        newGame' <- case gnugo' of
          Nothing -> do
            H.liftAff (delay $ Milliseconds 1000.0)
            -- Fake a move
            pure game
          Just gnugo -> do
            g <- H.liftAff $ liftEffect $ aiPlay gnugo game
            pure g
        H.modify_ \s -> s { game = newGame', status = WaitingHuman }
        drawBoard
    pure unit

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
          renderBoard boardCtx select state.game
        Nothing -> log "Where is the canvas?!"
