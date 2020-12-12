module Trainer.Player (Input, Slot, Output, component) where

import Prelude
import Baduk (Coord, Game, Color(..), Move(..), Result(..), addMove, getLastMove, initAliveStones, isCompleted, loadBaduk, save, getStone)
import Baduk.Types (PlayerMove(..))
import Data.List (List(..), length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
    , gnugo :: GnuGO.WASM
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

data Score
  = Score Color Number

showScore :: Score -> String
showScore = case _ of
  Score Black s -> "Black wins by " <> show s <> " points"
  Score White s -> "White wins by " <> show s <> " points"

data Status
  = WaitingAI
  | WaitingHuman
  | BadMove
  | GameOver Score

type State
  = { initialGame :: Game
    , gnugo :: GnuGO.WASM
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
  | Completed Result
  | Restart
  | AddStone MouseEvent
  | MouseMove MouseEvent
  | MouseLeave
  | Draw

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  let
    lastMove = getLastMove state.game

    placeInfo = case lastMove of
      Just (Move _ Pass) -> "click pass (or play another stone)"
      _ -> "place a stone"

    message = case state.status of
      WaitingAI -> "GnuGO is playing"
      WaitingHuman -> "Your turn to play, " <> placeInfo
      BadMove -> "Invalid move, play again"
      GameOver score' -> "Game is over: " <> showScore score'

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

    item :: forall v. Show v => String -> v -> _
    item txt v =
      HH.span_
        [ HH.text (" | " <> txt <> ": "), HH.b_ [ HH.text (show v) ] ]

    showCapture name player = case player.captures of
      Nil -> []
      captures -> [ item (name <> " captures") (length captures) ]

    lastMoveInfo = case lastMove of
      Just move -> [ HH.br_, item "Last move" move ]
      Nothing -> []

    infos =
      HH.div_
        ( [ item "Move" state.game.move
          , item "Komi" state.game.komi
          ]
            <> showCapture "Black" state.game.black
            <> showCapture "White" state.game.white
            <> lastMoveInfo
        )

    mainButton = case state.status of
      GameOver (Score color _) -> case color == state.game.startingPlayer of
        true ->
          [ HH.a
              [ HP.class_ (ClassName "btn btn-success"), HE.onClick \s -> Just $ Completed Win ]
              [ HH.text "Complete" ]
          ]
        false -> []
      _ ->
        [ HH.a
            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick \s -> Just $ DoPass ]
            [ HH.text "Pass" ]
        ]
  in
    HH.div
      [ HP.class_ (ClassName "container") ]
      [ HH.h4_
          [ HH.text ("Playing: " <> state.game.name) ]
      , HH.p_
          [ HH.text ("Status: " <> message) ]
      , HH.div
          [ HP.class_ (ClassName "col") ]
          [ HH.div_
              [ board, infos ]
          , HH.div_
              ( mainButton
                  <> [ HH.a
                        [ HP.class_ (ClassName "btn btn-secondary"), HE.onClick \s -> Just $ Restart ]
                        [ HH.text "Restart" ]
                    , HH.a
                        [ HP.class_ (ClassName "btn btn-danger"), HE.onClick \s -> Just $ Completed Loss ]
                        [ HH.text "Resign" ]
                    ]
              )
          ]
      ]

aiPlay :: forall m. MonadAff m => GnuGO.WASM -> Game -> m Game
aiPlay gnugo game = do
  let
    gameStr = save game
  liftEffect $ log ("sending: " <> gameStr)
  newGameStr <- H.liftAff $ GnuGO.play gnugo 0 gameStr
  liftEffect $ log ("received: " <> newGameStr)
  case loadBaduk newGameStr of
    -- revert starting player inversion
    Just g -> do
      liftEffect $ log ("loaded game: " <> show g)
      case getLastMove g of
        Just move -> do
          liftEffect $ log ("adding: " <> show move)
          case addMove move game of
            Just newGame -> pure newGame
            Nothing -> do
              liftEffect $ log ("No new-move?!")
              pure game
        Nothing -> do
          liftEffect $ log ("No move?!")
          pure game
    Nothing -> do
      liftEffect $ log ("Couldn't load")
      pure game

aiScore :: forall m. MonadAff m => GnuGO.WASM -> Game -> m Score
aiScore gnugo game = do
  let
    gameStr = save game
  liftEffect $ log ("scoring: " <> gameStr)
  newScore <- H.liftAff $ GnuGO.score gnugo 0 gameStr
  liftEffect $ log ("received: " <> show newScore)
  pure
    $ if newScore < 0.0 then
        Score Black (newScore * -1.0)
      else
        Score White newScore

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Completed result -> H.raise (Just result)
  Restart -> do
    state <- H.get
    H.modify_ \_ -> initialState { game: state.initialGame, gnugo: state.gnugo }
    drawBoard
  Draw -> drawBoard
  MouseMove e -> do
    state <- H.get
    case state.status of
      WaitingAI -> pure unit
      GameOver _ -> pure unit
      BadMove -> moveMouse state e
      WaitingHuman -> moveMouse state e
  MouseLeave -> do
    H.modify_ \s -> s { editCoord = Nothing }
    drawBoard
  AddStone e -> do
    state <- H.get
    case state.editCoord of
      Nothing -> pure unit
      Just coord -> case getStone coord state.game of
        Nothing -> play (PlaceStone coord) state
        Just _ -> pure unit
  DoPass -> H.get >>= play Pass
  where
  moveMouse state e = do
    mCoord <- liftEffect $ mouseCoord e
    case mCoord /= state.editCoord of
      false -> pure unit
      true -> do
        H.modify_ \s -> s { editCoord = mCoord }
        case mCoord of
          Nothing -> pure unit
          Just _ -> drawBoard

  play :: PlayerMove -> State -> H.HalogenM State Action () Output m Unit
  play move state = case addMove (Move state.game.startingPlayer move) state.game of
    Just newGame -> case isCompleted newGame of
      true -> completeGame state.gnugo newGame
      false -> playAi state.gnugo newGame
    Nothing -> do
      H.modify_ \s -> s { editCoord = Nothing, status = BadMove }
      drawBoard

  playAi :: GnuGO.WASM -> Game -> H.HalogenM State Action () Output m Unit
  playAi gnugo game = do
    H.modify_ \s -> s { game = game, status = WaitingAI }
    drawBoard
    newGame' <- aiPlay gnugo game
    case isCompleted newGame' of
      true -> completeGame gnugo newGame'
      false -> do
        H.modify_ \s -> s { game = newGame', status = WaitingHuman }
        drawBoard
    pure unit

  completeGame :: GnuGO.WASM -> Game -> H.HalogenM State Action () Output m Unit
  completeGame gnugo game = do
    score' <- aiScore gnugo game
    H.modify_ \s -> s { game = game, status = GameOver score' }
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
              Tuple WaitingAI _ -> Nothing
              Tuple (GameOver _) _ -> Nothing
              Tuple _ (Just coord) -> Just (Tuple coord (Just state.game.startingPlayer))
              Tuple _ _ -> Nothing
          boardCtx <- Canvas.getContext2D boardCanvas
          renderBoard boardCtx select state.game
        Nothing -> log "Where is the canvas?!"
