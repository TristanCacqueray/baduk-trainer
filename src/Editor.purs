module Editor where

import Prelude
import Baduk.Game as Baduk
import Baduk.Types (Coord(..), Game, Stone(..), initGame)
import Data.Int (round, toNumber)
import Data.List (List(..), range)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (for_, sequence, traverse)
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
import SGF (loadBaduk)
import SGF.Types (Color(..), showHexColor)
import Web.Event.Event as WE
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, toEvent)

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

renderBoard :: Canvas.Context2D -> Maybe (Tuple Coord (Maybe Color)) -> Game -> Effect Unit
renderBoard ctx selection game =
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
    -- Selection
    for_ selection (\(Tuple coord color) -> for_ color (flip renderStone coord))
    -- Stone
    case game.stonesAlive of
      Nil -> do
        for_ (game.black.stones) (renderStone Black)
        for_ (game.white.stones) (renderStone White)
      xs -> for_ xs (\(Stone color coord) -> renderStone color coord)
    -- Removal
    for_ selection renderClear
  where
  boardSize' = toNumber $ boardSize game.size

  coordPos :: Int -> Number
  coordPos x = boardPadding + toNumber x * stoneSize

  renderClear :: Tuple Coord (Maybe Color) -> Effect Unit
  renderClear (Tuple (Coord x y) color) = case color of
    Just _ -> pure unit
    Nothing -> do
      Canvas.strokePath ctx
        $ do
            let
              xPos = boardPadding + stoneSize * (toNumber x)

              yPos = boardPadding + stoneSize * (toNumber y)

              l = 10.0
            Canvas.setStrokeStyle ctx "lightblue"
            Canvas.setLineWidth ctx 2.5
            Canvas.moveTo ctx (xPos - l) (yPos - l)
            Canvas.lineTo ctx (xPos + l) (yPos + l)
            Canvas.moveTo ctx (xPos - l) (yPos + l)
            Canvas.lineTo ctx (xPos + l) (yPos - l)
            Canvas.closePath ctx

  renderStone :: Color -> Coord -> Effect Unit
  renderStone color (Coord x y) = do
    setFillStyle ctx (showHexColor color)
    fillPath ctx $ arc ctx { x: (coordPos x), y: (coordPos y), radius: 25.0 * 0.8, start: 0.0, end: Math.pi * 2.0 }

snapPos :: { x :: Int, y :: Int } -> Maybe Coord
snapPos { x, y } = case (x > 6 && y > 6 && modX > 10.0 && modY > 10.0) of
  true -> Just (Coord (round $ (abs xN) / stoneSize) (round $ (abs yN) / stoneSize))
  false -> Nothing
  where
  xN :: Number
  xN = (toNumber x - boardPadding)

  yN :: Number
  yN = (toNumber y - boardPadding)

  modX = abs $ (xN `mod` stoneSize) - 25.0

  modY = abs $ (yN `mod` stoneSize) - 25.0

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
        , HE.onClick \e -> Just $ AddStone e
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
  | AddStone MouseEvent
  | MouseMove MouseEvent
  | ClearSelection
  | Save
  | Cancel

mouseCoord :: MouseEvent -> Effect (Maybe Coord)
mouseCoord e = do
  pos <- relativePosition e
  pure (join $ snapPos <$> pos)

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
  AddStone e -> do
    pos' <- liftEffect $ relativePosition e
    state <- H.get
    case pos' of
      Just pos -> do
        liftEffect $ logShow $ "Clicked: " <> show pos.x <> " " <> show pos.y
        H.modify_ \s -> s { message = "  | clicked: " <> show pos.x <> " " <> show pos.y }
        case state.editPos of
          Just (Tuple coord color') -> do
            case color' of
              Just color -> H.modify_ \s -> s { game = Baduk.setStone state.game coord color }
              Nothing -> H.modify_ \s -> s { game = Baduk.removeStone state.game coord }
            drawCanvases
          Nothing -> pure unit
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
          renderBoard boardCtx state.editPos state.game
          logShow "Drawing..."
        _ -> logShow "Where are the canvases?!"
