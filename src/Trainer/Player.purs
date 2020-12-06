module Trainer.Player (Input, Slot, Output, component) where

import Prelude
import Baduk (Coord, Game, Color(..), Move(..), Result(..), addMove, getLastMove, initAliveStones, isCompleted, loadBaduk, save)
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
  | Completed Result
  | Restart
  | AddStone MouseEvent
  | MouseMove MouseEvent
  | MouseLeave
  | Draw

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
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

aiScore :: GnuGO.WASM -> Game -> Effect Score
aiScore gnugo game = do
  let
    gameStr = save game
  log ("scoring: " <> gameStr)
  let
    newScore = GnuGO.score gnugo 0 gameStr
  log ("received: " <> show newScore)
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
      Just coord -> play (PlaceStone coord) state
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

  playAi :: Maybe GnuGO.WASM -> Game -> H.HalogenM State Action () Output m Unit
  playAi gnugo' game = do
    H.modify_ \s -> s { game = game, status = WaitingAI }
    drawBoard
    _ <- do
      H.fork do
        -- This delay seems to help halogen render the waiting ai message
        -- Otherwise it skip the update and only show waiting for human again
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
        case isCompleted newGame' of
          true -> completeGame gnugo' newGame'
          false -> do
            H.modify_ \s -> s { game = newGame', status = WaitingHuman }
            drawBoard
    pure unit

  completeGame gnugo' game = do
    score' <- case gnugo' of
      Nothing -> do
        H.liftAff (delay $ Milliseconds 1000.0)
        -- Fake a score
        pure (Score Black 1.0)
      Just gnugo -> do
        s <- H.liftAff $ liftEffect $ aiScore gnugo game
        pure s
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
