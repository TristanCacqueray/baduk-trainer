module Test.Main where

import Baduk.Converter (load, showBoard, readBoard)
import Baduk.Game (addStone)
import Baduk.Game as Baduk
import Baduk.Types (Coord(..), Stone(..), initGame, showGame)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (concat, intercalate)
import Data.Array as A
import Data.Either (Either(..))
import Data.Functor (map)
import Data.List (List(..), fromFoldable, sort)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Prelude (Unit, discard, pure, show, unit, ($), (<>), (==), (/=), bind)
import SGF (loadBaduk)
import SGF.Parser (parse)
import SGF.Types (Color(..), GameTree, demo)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

checkLoader :: forall m. MonadThrow Error m => List GameTree -> m Unit
checkLoader sgf = do
  case load sgf of
    Left err → throwError (error err)
    Right (Tuple _ logs) →
      if A.null logs then
        pure unit
      else
        throwError (error $ "loading logs: " <> show logs)

checkParser :: forall m. MonadThrow Error m => Boolean -> String -> m Unit
checkParser valid expr = do
  case parse expr of
    Right res ->
      if valid then
        pure unit
      else
        throwError (error $ "should have failed: " <> show res)
    Left err ->
      if valid then
        throwError (error $ show err)
      else
        pure unit

goodExprs :: Array String
goodExprs =
  [ "(;G[1]B[01])"
  , "(;GM[1]SZ[5]KM[3.5]AB[cb][bc]AW[cd][dd][ed]PL[B];B[bd])"
  , "(;G[]B[11](;N[42]))"
  ]

badExprs :: Array String
badExprs =
  [ ""
  , "()"
  ]

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "purescript-gnugo" do
          describe "Parser" $ sequence_ parserTests
          describe "Converter" do
            it "load demo sgf" (checkLoader demo)
            it "print game" checkShowGame
            it "save game" checkSaveGame
          describe "Game" $ sequence_ playTests
  where
  playTests =
    map (\t -> (it t.name $ checkPlayGame t))
      ( fromFoldable
          [ { name: "simple capture"
            , start:
                fromFoldable
                  [ "     "
                  , " bwb "
                  , "  b  "
                  ]
            , move: (Stone Black (Coord 0 2))
            , want:
                fromFoldable
                  [ "  b  "
                  , " b b "
                  , "  b  "
                  ]
            }
          , { name: "bigger capture"
            , start:
                fromFoldable
                  [ "   b "
                  , " bwwb"
                  , "  bb "
                  ]
            , move: (Stone Black (Coord 0 2))
            , want:
                fromFoldable
                  [ "  bb "
                  , " b  b"
                  , "  bb "
                  ]
            }
          , { name: "multi capture"
            , start:
                fromFoldable
                  [ " b b "
                  , "bw wb"
                  , " b b "
                  ]
            , move: (Stone Black (Coord 1 2))
            , want:
                fromFoldable
                  [ " b b "
                  , "b b b"
                  , " b b "
                  ]
            }
          ]
      )

  play start move want = do
    startingGame <- readBoard start
    newGame <- addStone move startingGame
    expectedGame <- readBoard want
    pure { got: newGame, expected: expectedGame }

  checkPlayGame ::
    forall m.
    MonadThrow Error m =>
    { name :: String, start :: List String, move :: Stone, want :: List String } -> m Unit
  checkPlayGame { start, move, want } = case play start move want of
    Just { got, expected } ->
      if sort got.stonesAlive /= sort expected.stonesAlive then
        throwError (error ("Wanted: " <> showGame expected <> "\n  Got:    " <> showGame got))
      else
        pure unit
    Nothing -> throwError (error "Can't play")

  sgfStr =
    intercalate "\n"
      [ "(;GM[1]FF[4]"
      , "SZ[5]"
      , "DT[2020-11-08]"
      , "AP[GNU Go:3.9.1]"
      , "C[load and analyze mode]"
      , ";B[bb]W[cc])"
      ]

  gameStr =
    intercalate "\n"
      [ "[.,.,.,.,.]"
      , "[b,.,.,.,.]"
      , "[.,w,.,.,.]"
      , "[.,.,.,.,.]"
      , "[.,.,.,.,.]"
      , ""
      ]

  testGame =
    initGame
      { black = initGame.black { moves = Cons (Coord 1 1) (Cons (Coord 1 2) Nil) }
      , white = initGame.white { moves = Cons (Coord 4 1) (Cons (Coord 4 2) Nil) }
      }

  testStr =
    intercalate "\n"
      [ "(;SZ[19]PL[B]"
      , ";W[ec](;B[bc](;W[eb](;B[bb]))))"
      ]

  checkSaveGame :: forall m. MonadThrow Error m => m Unit
  checkSaveGame =
    if Baduk.save testGame == testStr then
      pure unit
    else
      throwError (error $ Baduk.save testGame)

  checkShowGame :: forall m. MonadThrow Error m => m Unit
  checkShowGame = case loadBaduk sgfStr of
    Just g ->
      if showBoard g == gameStr then
        pure unit
      else
        throwError (error $ showBoard g)
    Nothing -> throwError (error "Couldn't load game")

  parserTests =
    concat
      [ map (\expr → (it ("parse " <> expr) $ checkParser true expr)) goodExprs
      , map (\expr → (it ("error " <> if null expr then "(empty)" else expr) $ checkParser false expr)) badExprs
      ]
