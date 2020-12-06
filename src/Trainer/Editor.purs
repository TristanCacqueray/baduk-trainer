module Trainer.Editor
  ( Input
  , Slot
  , Output
  , component
  -- TODO: implement this in Trainer.Board
  , renderMignature
  ) where

import Baduk (Coord, Game, initGame, loadBaduk)
import Baduk as Baduk
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (logShow)
import Graphics.Canvas (CanvasElement, arc, fillPath, getCanvasElementById, rect, setFillStyle, translate, withContext)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math as Math
import Prelude
import SGF (Color(..), showHexColor)
import Trainer.Board (boardSize, renderBoard, mouseCoord)
import Web.UIEvent.MouseEvent (MouseEvent)

type Input
  = String

type Output
  = Maybe String

type Slot id
  = forall query. H.Slot query Output id

component :: forall query m. MonadEffect m => H.Component HH.HTML query Input Output m
component =
  H.mkComponent
    { initialState: initialSGFEditorState
    , render: renderSGFEditor
    , eval: H.mkEval $ H.defaultEval { handleAction = handleSGFEditorAction, initialize = Just Initialize }
    }

initialSGFEditorState :: Input -> State
initialSGFEditorState sgfStr = { editColor: AddBlackStone, editPos: Nothing, message, game }
  where
  Tuple game message = case loadBaduk sgfStr of
    Just g -> Tuple g ""
    Nothing -> Tuple initGame ("Invalid sgf: " <> sgfStr)

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
      fillPath ctx $ arc ctx { x: 0.0, y: 0.0, radius: r * 0.9, start: 0.0, end: Math.pi * 2.0 }
    -- Stone
    setFillStyle ctx (showHexColor color)
    fillPath ctx $ arc ctx { x: 0.0, y: 0.0, radius: r * 0.8, start: 0.0, end: Math.pi * 2.0 }
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

type State
  = { editColor :: StoneSelector
    , editPos :: Maybe (Tuple Coord (Maybe Color))
    , message :: String
    , game :: Game
    }

-- Render
renderMignature :: forall a b. String -> String -> HH.HTML a b
renderMignature width sgfStr =
  HH.pre
    [ HP.prop (PropName "style") ("width: " <> width <> "; background: black; color: white; white-space: pre-wrap;")
    ]
    [ HH.text sgfStr ]

renderSGFEditor :: forall m. MonadEffect m => State -> H.ComponentHTML Action () m
renderSGFEditor state = do
  let
    value = show (state.editColor)

    mkCanvas name action =
      HH.div
        [ HP.class_ (ClassName "row") ]
        [ HH.canvas
            [ HP.id_ $ name <> "Picker"
            , HP.width 50
            , HP.height 50
            , HP.prop (PropName "style") "border: 1px solid black"
            , HE.onClick \_ -> Just action
            ]
        ]

    boardSize' = boardSize state.game.size

    mkBoard =
      HH.canvas
        [ HP.id_ "board"
        , HP.width boardSize'
        , HP.height boardSize'
        , HP.prop (PropName "style") "border: 1px solid black"
        , HE.onClick \_ -> Just AddStone
        , HE.onMouseMove \e -> Just $ MouseMove e
        , HE.onMouseLeave \e -> Just ClearSelection
        ]
  HH.div
    [ HP.class_ (ClassName "container") ]
    [ HH.h1_
        [ HH.text "Game board editor" ]
    , HH.p_
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
                    , HE.onValueInput (Just <<< ChangeName)
                    ]
                ]
            , HH.div_
                [ HH.a
                    [ HP.class_ (ClassName "btn btn-primary"), HE.onClick \s -> Just $ Save ]
                    [ HH.text "Save" ]
                , HH.a
                    [ HP.class_ (ClassName "btn btn-secondary"), HE.onClick \s -> Just $ Cancel ]
                    [ HH.text "Cancel" ]
                ]
            ]
        ]
    ]

-- Update
data Action
  = Initialize
  | SelectBlackStone
  | SelectWhiteStone
  | SelectClearStone
  | AddStone
  | MouseMove MouseEvent
  | ChangeName String
  | ClearSelection
  | Save
  | Cancel

handleSGFEditorAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Output m Unit
handleSGFEditorAction = case _ of
  Initialize -> drawCanvases
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
          Just color -> H.modify_ \s -> s { game = Baduk.setStone state.game coord color }
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
