module Trainer.Editor
  ( Input
  , Slot
  , Output
  , component
  -- TODO: implement this in Trainer.Board
  , renderMignature
  ) where

import Baduk (Coord, Game, Result, initGame, loadBaduk)
import Baduk as Baduk
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Number (pi)
-- import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (logShow)
import GnuGO as GnuGO
import Graphics.Canvas (CanvasElement, arc, fillPath, getCanvasElementById, rect, setFillStyle, translate, withContext)
import Graphics.Canvas as Canvas
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import SGF (Color(..), showHexColor)
import Trainer.Board (boardSize, renderBoard, mouseCoord)
import Trainer.Player as Player
import Web.UIEvent.MouseEvent (MouseEvent)

type Input
  = { game :: Game
    , gnugo :: GnuGO.WASM
    }

type Output
  = Maybe String

type Slot id
  = forall query. H.Slot query Output id

type Slots
  = ( testPlayer :: Player.Slot Unit )

player = Proxy :: Proxy "testPlayer"

-- component :: forall query m. MonadAff m => MonadEffect m => H.Component HH.HTML query Input Output m
component =
  H.mkComponent
    { initialState: initialSGFEditorState
    , render: renderSGFEditor
    , eval: H.mkEval $ H.defaultEval { handleAction = handleSGFEditorAction, initialize = Just Initialize }
    }

initialSGFEditorState :: Input -> State
initialSGFEditorState { game, gnugo } = { editColor: AddBlackStone, editPos: Nothing, message: "", mode: Edit, game, gnugo }

-- Rendering primitives
newtype Selected
  = Selected Boolean

renderStoneSelection :: Color -> Selected -> CanvasElement -> Effect Unit
renderStoneSelection color (Selected selected) canvas = do
  ctx <- Canvas.getContext2D canvas
  withContext ctx do
    -- Background
    setFillStyle ctx "lightgrey"
    fillPath ctx $ rect ctx { x: 0.0, y: 0.0, width: w, height: h }
    moveToCenter ctx
    -- Selection
    when selected do
      setFillStyle ctx "blue"
      fillPath ctx $ arc ctx { x: 0.0, y: 0.0, radius: r * 0.9, start: 0.0, end: pi * 2.0, useCounterClockwise: false }
    -- Stone
    setFillStyle ctx (showHexColor color)
    fillPath ctx $ arc ctx { x: 0.0, y: 0.0, radius: r * 0.8, start: 0.0, end: pi * 2.0, useCounterClockwise: false }
  where
  moveToCenter ctx = translate ctx { translateX: r, translateY: r }

  w = 50.0

  h = 50.0

  r = w / 2.0

-- State
data StoneSelector
  = AddBlackStone
  | AddWhiteStone
  | RemoveStone

derive instance editColorEq :: Eq StoneSelector

instance showStoneSelector :: Show StoneSelector where
  show AddBlackStone = "Add black"
  show AddWhiteStone = "Add white"
  show RemoveStone = "Remove color"

stoneSelectorToColor :: StoneSelector -> Maybe Color
stoneSelectorToColor s = case s of
  AddBlackStone -> Just Black
  AddWhiteStone -> Just White
  RemoveStone -> Nothing

data Mode
  = Edit
  | Test

type State
  = { editColor :: StoneSelector
    , editPos :: Maybe (Tuple Coord (Maybe Color))
    , message :: String
    , mode :: Mode
    , game :: Game
    , gnugo :: GnuGO.WASM
    }

-- Render
renderMignature :: forall a b. String -> String -> HH.HTML a b
renderMignature width sgfStr =
  HH.pre
    [ HP.prop (PropName "style") ("width: " <> width <> "; background: black; color: white; white-space: pre-wrap;")
    ]
    [ HH.text sgfStr ]

renderSGFEditor :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action Slots m
renderSGFEditor state = do
  let
    value = show (state.editColor)

    mkCanvas name action =
      HH.div
        [ HP.class_ (ClassName "row") ]
        [ HH.canvas
            [ HP.id $ name <> "Picker"
            , HP.width 50
            , HP.height 50
            , HP.prop (PropName "style") "border: 1px solid black"
            , HE.onClick \_ -> action
            ]
        ]

    boardSize' = boardSize state.game.size

    mkBoard =
      HH.canvas
        [ HP.id "board"
        , HP.width boardSize'
        , HP.height boardSize'
        , HP.prop (PropName "style") "border: 1px solid black"
        , HE.onClick \_ ->  AddStone
        , HE.onMouseMove \e -> MouseMove e
        , HE.onMouseLeave \e -> ClearSelection
        ]

    editor =
      [ HH.p_
          [ HH.text ("Selection state: " <> value <> " " <> state.message) ]
      , HH.div
          [ HP.class_ (ClassName "row") ]
          [ HH.div
              [ HP.class_ (ClassName "col-2") ]
              [ HH.span
                  [ HP.class_ (ClassName "float-right") ]
                  [ mkCanvas "black" SelectBlackStone
                  , mkCanvas "white" SelectWhiteStone
                  , mkCanvas "clear" SelectClearStone
                  ]
              ]
          , HH.div
              [ HP.class_ (ClassName "col") ]
              [ mkBoard
              , renderMignature (show boardSize' <> "px") (Baduk.save state.game)
              , HH.div_
                  [ HH.text "Name: "
                  , HH.input
                      [ HP.value state.game.name
                      , HE.onValueInput ChangeName
                      ]
                  ]
              , HH.div_
                  [ HH.text "Size: "
                  , HH.input
                      [ HP.value (show state.game.size)
                      , HE.onValueInput (ChangeSize)
                      ]
                  ]
              , HH.div_
                  [ HH.text "Komi: "
                  , HH.input
                      [ HP.value (show state.game.komi)
                      , HE.onValueInput (ChangeKomi)
                      ]
                  ]
              , HH.div_
                  [ HH.text "Starting player: "
                  , HH.text (show state.game.startingPlayer)
                  ]
              , HH.div_
                  [ HH.a
                      [ HP.class_ (ClassName "btn btn-primary"), HE.onClick \s -> Save ]
                      [ HH.text "Save" ]
                  , HH.a
                      [ HP.class_ (ClassName "btn btn-secondary"), HE.onClick \s -> Cancel ]
                      [ HH.text "Cancel" ]
                  , HH.a
                      [ HP.class_ (ClassName "btn btn-info"), HE.onClick \s -> TestGame ]
                      [ HH.text "Test" ]
                  ]
              ]
          ]
      ]

    body = case state.mode of
      Edit -> editor
      Test -> [ HH.slot player unit Player.component { game: state.game, gnugo: state.gnugo } (Resume) ]
  HH.div
    [ HP.class_ (ClassName "container") ]
    ( [ HH.h1_
          [ HH.text "Editor" ]
      ]
        <> body
    )

-- Update
data Action
  = Initialize
  | SelectBlackStone
  | SelectWhiteStone
  | SelectClearStone
  | AddStone
  | MouseMove MouseEvent
  | ChangeName String
  | ChangeKomi String
  | ChangeSize String
  | ClearSelection
  | Resume (Maybe Result)
  | TestGame
  | Save
  | Cancel

handleSGFEditorAction :: forall m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action Slots Output m Unit
handleSGFEditorAction = case _ of
  Initialize -> drawCanvases
  TestGame -> H.modify_ \s -> s { mode = Test }
  Resume _ -> do
    H.modify_ \s -> s { mode = Edit }
    drawCanvases
  SelectBlackStone -> do
    H.modify_ \s -> s { editColor = AddBlackStone, game = s.game { startingPlayer = Black } }
    drawCanvases
  SelectWhiteStone -> do
    H.modify_ \s -> s { editColor = AddWhiteStone, game = s.game { startingPlayer = White } }
    drawCanvases
  SelectClearStone -> do
    H.modify_ \s -> s { editColor = RemoveStone }
    drawCanvases
  ClearSelection -> do
    H.modify_ \s -> s { editPos = Nothing }
    drawCanvases
  ChangeName name -> do
    H.modify_ \s -> s { game = s.game { name = name } }
  ChangeKomi name -> case fromString name of
    Just komi -> H.modify_ \s -> s { game = s.game { komi = komi } }
    Nothing -> pure unit
  ChangeSize name -> case fromString name of
    Just size -> do
      H.modify_ \s -> s { game = s.game { size = round size } }
      drawCanvases
    Nothing -> pure unit
  Save -> do
    state <- H.get
    H.raise $ Just (Baduk.save state.game)
  Cancel -> H.raise Nothing
  MouseMove e -> do
    mCoord <- liftEffect $ mouseCoord e
    case mCoord of
      Just coord -> do
        state <- H.get
        let
          editPos = Just (Tuple coord (stoneSelectorToColor state.editColor))
        case editPos /= state.editPos of
          true -> do
            H.modify_ \s -> s { editPos = editPos }
            drawCanvases
          false -> pure unit
      Nothing -> H.modify_ \s -> s { editPos = Nothing }
  AddStone -> do
    state <- H.get
    case state.editPos of
      Just (Tuple coord color') -> do
        case color' of
          Just color -> case (Baduk.getStone coord state.game) of
            Nothing -> H.modify_ \s -> s { game = Baduk.setStone state.game coord color }
            Just _ -> pure unit
          Nothing -> H.modify_ \s -> s { game = Baduk.removeStone state.game coord }
        drawCanvases
      Nothing -> pure unit
  where
  getCanvases :: Array String -> Effect (Array (Maybe CanvasElement))
  getCanvases = traverse getCanvasElementById

  drawCanvases = do
    state <- H.get
    liftEffect do
      canvases <- getCanvases [ "blackPicker", "whitePicker", "clearPicker", "board" ]
      case sequence canvases of
        Just [ blackCanvas, whiteCanvas, clearCanvas, boardCanvas ] -> do
          renderStoneSelection Black (Selected $ state.editColor == AddBlackStone) blackCanvas
          renderStoneSelection White (Selected $ state.editColor == AddWhiteStone) whiteCanvas
          boardCtx <- Canvas.getContext2D boardCanvas
          renderBoard boardCtx state.editPos state.game
          logShow "Drawing..."
        _ -> logShow "Where are the canvases?!"
