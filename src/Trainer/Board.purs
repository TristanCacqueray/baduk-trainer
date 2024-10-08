module Trainer.Board (boardSize, renderMiniBoard, renderBoard, mouseCoord) where

import Baduk (Move(..), getLastMove)
import Baduk.Types (Coord(..), Game, PlayerMove(..), Stone(..))
import Data.Int (round, toNumber)
import Data.List (List(..), range)
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Ord (abs)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (arc, fillPath, rect, setFillStyle, withContext)
import Graphics.Canvas as Canvas
import Halogen (liftEffect)
import Prelude
import SGF (Color(..), inverseColor, showHexColor)
import Web.Event.Event as WE
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Element as DomElement
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, toEvent)

boardPadding :: Number
boardPadding = 30.0

stoneSize :: Number
stoneSize = 50.0

boardSize :: Int -> Int
boardSize size = round boardPadding * 2 + round stoneSize * (size - 1)

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

-- Get the element relative poition of a click event
relativePosition :: MouseEvent -> Effect (Maybe { x :: Int, y :: Int })
relativePosition ev = case WE.target (toEvent ev) >>= HTMLElement.fromEventTarget of
  Just elem ->
    liftEffect do
      boundingRect <- DomElement.getBoundingClientRect (HTMLElement.toElement elem)
      pure
        $ Just
            { x: clientX ev - round boundingRect.left
            , y: clientY ev - round boundingRect.top
            }
  Nothing -> pure Nothing

mouseCoord :: MouseEvent -> Effect (Maybe Coord)
mouseCoord e = do
  pos <- relativePosition e
  pure (join $ snapPos <$> pos)

renderMiniBoard :: Canvas.Context2D -> Game -> Effect Unit
renderMiniBoard ctx game =
  withContext ctx do
    -- Background
    setFillStyle ctx "#966F33"
    fillPath ctx $ rect ctx { x: 0.0, y: 0.0, width: boardSize', height: boardSize' }
    -- Grid
    Canvas.beginPath ctx
    for_ (range 0 (game.size - 1)) \n -> do
      Canvas.strokePath ctx
        $ do
            let
              startPos = (boardPadding / 2.0) + (stoneSize / 2.0) * (toNumber n)

              endPos = boardSize' - (boardPadding / 2.0)
            Canvas.moveTo ctx startPos (boardPadding / 2.0)
            Canvas.lineTo ctx startPos endPos
            Canvas.moveTo ctx (boardPadding / 2.0) startPos
            Canvas.lineTo ctx endPos startPos
            Canvas.closePath ctx
    -- Stone
    case game.stonesAlive of
      Nil -> do
        for_ (game.black.stones) (renderStone Black)
        for_ (game.white.stones) (renderStone White)
      xs -> for_ xs (\(Stone color coord) -> renderStone color coord)
  where
  boardSize' = toNumber $ boardSize game.size `div` 2

  coordPos :: Int -> Number
  coordPos x = boardPadding / 2.0 + toNumber x * (stoneSize / 2.0)

  renderStone :: Color -> Coord -> Effect Unit
  renderStone color (Coord x y) = do
    setFillStyle ctx (showHexColor color)
    fillPath ctx $ arc ctx { x: (coordPos x), y: (coordPos y), radius: 12.5 * 0.8, start: 0.0, end: pi * 2.0, useCounterClockwise: false }

renderBoard :: Canvas.Context2D -> Maybe (Tuple Coord (Maybe Color)) -> Game -> Effect Unit
renderBoard ctx selection game =
  withContext ctx do
    -- Background
    setFillStyle ctx "#966F33"
    fillPath ctx $ rect ctx { x: 0.0, y: 0.0, width: boardSize', height: boardSize' }
    -- Grid
    Canvas.beginPath ctx
    for_ (range 0 (game.size - 1)) \n -> do
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
    -- Last move
    case getLastMove game of
      Just (Move color (PlaceStone coord)) -> renderLastMove (inverseColor color) coord
      _ -> pure unit
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

  renderLastMove :: Color -> Coord -> Effect Unit
  renderLastMove color (Coord x y) = do
    Canvas.strokePath ctx
      $ do
          Canvas.setStrokeStyle ctx (showHexColor color)
          Canvas.setLineWidth ctx 3.0
          Canvas.arc ctx { x: (coordPos x), y: (coordPos y), radius: 25.0 * 0.4, start: 0.0, end: pi * 2.0, useCounterClockwise: false }

  renderStone :: Color -> Coord -> Effect Unit
  renderStone color (Coord x y) = do
    setFillStyle ctx (showHexColor color)
    fillPath ctx $ arc ctx { x: (coordPos x), y: (coordPos y), radius: 25.0 * 0.8, start: 0.0, end: pi * 2.0, useCounterClockwise: false }
