module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Int (round)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..), ClassName(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, toEvent)
import Effect.Console (logShow)
import Graphics.Canvas (CanvasElement, arc, fillPath, getCanvasElementById, rect, setFillStyle, translate, withContext)
import Math as Math
import SGF.Types (Color(..), showHexColor)
import Web.HTML.HTMLElement as HTMLElement
import Web.Event.Event as WE
import Graphics.Canvas as Canvas

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

-- Get the element relative poition of a click event
relativePosition :: MouseEvent -> Effect (Maybe { x :: Int, y :: Int })
relativePosition ev = case WE.target (toEvent ev) >>= HTMLElement.fromEventTarget of
  Just elem ->
    liftEffect do
      offsetLeft <- HTMLElement.offsetLeft elem
      offsetTop <- HTMLElement.offsetTop elem
      pure
        $ Just
            { x: clientX ev - round offsetLeft
            , y: clientY ev - round offsetTop
            }
  Nothing -> pure Nothing

-- Halogen entrypoint
main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI sgfEditor unit body

sgfEditor :: forall query input output m. MonadEffect m => H.Component HH.HTML query input output m
sgfEditor =
  H.mkComponent
    { initialState: initialSGFEditorState
    , render: renderSGFEditor
    , eval: H.mkEval $ H.defaultEval { handleAction = handleSGFEditorAction, initialize = Just Initialize }
    }

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

type State
  = { editColor :: StoneSelector, message :: String }

initialSGFEditorState :: forall input. input -> State
initialSGFEditorState _ = { editColor: AddBlackStone, message: "" }

-- Render
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

    mkBoard =
      HH.canvas
        [ HP.id_ "board"
        , HP.width 500
        , HP.height 500
        , HP.prop (PropName "style") "border: 1px solid black"
        , HE.onClick \e -> Just $ AddStone e
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
            , HH.pre
                [ HP.prop (PropName "style") "width: 500px; background: black; color: white"
                ]
                [ HH.text "(SGF;)" ]
            ]
        ]
    ]

-- Update
data Action
  = Initialize
  | SelectBlackStone
  | SelectWhiteStone
  | SelectClearStone
  | AddStone MouseEvent

handleSGFEditorAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleSGFEditorAction = case _ of
  Initialize -> drawCanvases
  SelectBlackStone -> do
    H.modify_ \s -> { editColor: AddBlackStone, message: s.message }
    drawCanvases
  SelectWhiteStone -> do
    H.modify_ \s -> { editColor: AddWhiteStone, message: s.message }
    drawCanvases
  SelectClearStone -> do
    H.modify_ \s -> { editColor: RemoveStone, message: s.message }
    drawCanvases
  AddStone e -> do
    pos' <- liftEffect $ relativePosition e
    case pos' of
      Just pos -> do
        liftEffect $ logShow $ "Clicked: " <> show pos.x <> " " <> show pos.y
        H.modify_ \s -> { editColor: s.editColor, message: "  | clicked: " <> show pos.x <> " " <> show pos.y }
      Nothing -> liftEffect $ logShow "Unknown event source?!"
  where
  getCanvases :: Array String -> Effect (Array (Maybe CanvasElement))
  getCanvases = traverse getCanvasElementById

  drawCanvases = do
    state <- H.get
    liftEffect do
      canvases <- getCanvases [ "blackPicker", "whitePicker", "clearPicker" ]
      case sequence canvases of
        Just [ blackCanvas, whiteCanvas, clearCanvas ] -> do
          renderStoneSelection Black (Selected $ state.editColor == AddBlackStone) blackCanvas
          renderStoneSelection White (Selected $ state.editColor == AddWhiteStone) whiteCanvas
          logShow "Drawing..."
        _ -> logShow "Where are the canvases?!"
