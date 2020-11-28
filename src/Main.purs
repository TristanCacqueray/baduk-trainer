module Main where

import Baduk.Types
import Prelude
import Baduk.Converter (load)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.List (range)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Console (logShow)
import Graphics.Canvas (CanvasElement, arc, fillPath, getCanvasElementById, rect, setFillStyle, translate, withContext)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Core (PropName(..), ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Math as Math
import SGF (loadBaduk)
import SGF.Parser (parse)
import SGF.Types (Color(..), showHexColor)
import Web.Event.Event as WE
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, toEvent)

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

renderBoard :: Canvas.Context2D -> Game -> Effect Unit
renderBoard ctx game =
  withContext ctx do
    -- Background
    setFillStyle ctx "#966F33"
    fillPath ctx $ rect ctx { x: 0.0, y: 0.0, width: boardSize', height: boardSize' }
    -- Grid
    Canvas.beginPath ctx
    for_ (range 0 game.size) \n -> do
      Canvas.strokePath ctx
        $ do
            let
              startPos = boardPadding + stoneSize * (toNumber n)

              endPos = boardSize' - boardPadding
            Canvas.moveTo ctx startPos boardPadding
            Canvas.lineTo ctx startPos endPos
            Canvas.moveTo ctx boardPadding startPos
            Canvas.lineTo ctx endPos startPos
            Canvas.closePath ctx
    -- Stone
    for_ (game.black.stones) (renderStone Black)
    for_ (game.white.stones) (renderStone White)
  where
  boardSize' = toNumber $ boardSize game.size

  coordPos :: Int -> Number
  coordPos x = boardPadding + toNumber x * stoneSize

  renderStone :: Color -> Coord -> Effect Unit
  renderStone color (Coord x y) = do
    setFillStyle ctx (showHexColor color)
    fillPath ctx $ arc ctx { x: (coordPos x), y: (coordPos y), radius: 25.0 * 0.8, start: 0.0, end: Math.pi * 2.0 }

boardPadding :: Number
boardPadding = 30.0

stoneSize :: Number
stoneSize = 50.0

boardSize :: Int -> Int
boardSize size = round boardPadding * 2 + round stoneSize * size

-- Get the element relative poition of a click event
relativePosition :: MouseEvent -> Effect (Maybe { x :: Int, y :: Int })
relativePosition ev = case WE.target (toEvent ev) >>= HTMLElement.fromEventTarget of
  Just elem ->
    liftEffect do
      boundingRect <- HTMLElement.getBoundingClientRect elem
      pure
        $ Just
            { x: clientX ev - round boundingRect.left
            , y: clientY ev - round boundingRect.top
            }
  Nothing -> pure Nothing

main :: Effect Unit
main = do
  case loadBaduk sgfStr of
    Just g -> do
      log $ show g
      main' g
    Nothing -> log "oops"
  where
  sgfStr =
    intercalate "\n"
      [ "(;GM[1]FF[4]"
      , "SZ[5]"
      , "DT[2020-11-08]"
      , "AP[GNU Go:3.9.1]"
      , ";B[bb]C[load and analyze mode];W[bc]C[load and analyze mode];B[db]"
      , ";B[bd]W[ee]"
      , "C[load and analyze mode])"
      ]

-- Halogen entrypoint
main' :: Game -> Effect Unit
main' game =
  runHalogenAff do
    body <- awaitBody
    runUI (sgfEditor game) unit body

sgfEditor :: forall query input output m. MonadEffect m => Game -> H.Component HH.HTML query input output m
sgfEditor game =
  H.mkComponent
    { initialState: initialSGFEditorState game
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
  = { editColor :: StoneSelector, message :: String, game :: Game }

initialSGFEditorState :: forall input. Game -> input -> State
initialSGFEditorState game _ = { editColor: AddBlackStone, message: "", game }

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

    boardSize' = boardSize state.game.size

    mkBoard =
      HH.canvas
        [ HP.id_ "board"
        , HP.width boardSize'
        , HP.height boardSize'
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
                [ HP.prop (PropName "style") ("width: " <> show boardSize' <> "px; background: black; color: white")
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
    H.modify_ \s -> s { editColor = AddBlackStone }
    drawCanvases
  SelectWhiteStone -> do
    H.modify_ \s -> s { editColor = AddWhiteStone }
    drawCanvases
  SelectClearStone -> do
    H.modify_ \s -> s { editColor = RemoveStone }
    drawCanvases
  AddStone e -> do
    pos' <- liftEffect $ relativePosition e
    case pos' of
      Just pos -> do
        liftEffect $ logShow $ "Clicked: " <> show pos.x <> " " <> show pos.y
        H.modify_ \s -> s { message = "  | clicked: " <> show pos.x <> " " <> show pos.y }
      Nothing -> liftEffect $ logShow "Unknown event source?!"
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
          renderBoard boardCtx state.game
          logShow "Drawing..."
        _ -> logShow "Where are the canvases?!"
