module Test.Main where

import Baduk (loadBaduk)
import Baduk as Baduk
import Baduk.Converter (load, showBoard, readBoard)
import Baduk.Game (addStone)
import Baduk.Types (Coord(..), PlayerMove(..), Stone(..), initGame, showGame)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (concat, intercalate)
import Data.Array as A
import Data.Either (Either(..))
import Data.Functor (map)
import Data.List (List(..), fromFoldable, sort, (:))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Prelude (Unit, discard, pure, show, unit, ($), (<>), (==), (/=), bind)
import SGF (Color(..), GameTree(..), Property(..), SGF, Sequence, Value(..), parse)
import SGF as SGF
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- A SGF instance for testing purpose
sampleSGF ∷ SGF
sampleSGF = game : Nil
  where
  game ∷ GameTree
  game = GameTree seq Nil

  seq ∷ Sequence
  seq = (size : Nil) : (pos Black : Nil) : (player Black : Nil) : Nil

  size ∷ Property
  size = Prop "SZ" (Num 5.0 : Nil)

  pos ∷ Color → Property
  pos col = Prop (colorName col) (Point 1 1 : Point 2 1 : Nil)
    where
    colorName Black = "AB"

    colorName White = "AW"

  player ∷ Color → Property
  player col = Prop "PL" (Color col : Nil)

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
  [ "(;G[1]B[01]W[])"
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
            it "load demo sgf" (checkLoader sampleSGF)
            it "load game" checkLoadGame
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
          , { name: "inner capture"
            , start:
                fromFoldable
                  [ " bw b "
                  , "  bw  "
                  , " b bw "
                  , " bbw  "
                  , "      "
                  ]
            , move: (Stone White (Coord 2 2))
            , want:
                fromFoldable
                  [ " bw b "
                  , "  bw  "
                  , " bw w "
                  , " bbw  "
                  , "      "
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
    Nothing -> case readBoard start of
      Nothing -> throwError (error ("Can't read" <> show start))
      Just startingGame -> throwError (error $ "Can't play " <> show startingGame.stonesAlive <> " move: " <> show move)

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
      { black = initGame.black { moves = map PlaceStone $ Cons (Coord 1 1) (Cons (Coord 1 2) Nil) }
      , white = initGame.white { moves = map PlaceStone $ Cons (Coord 4 1) (Cons (Coord 4 2) Nil) }
      }

  testStr =
    intercalate "\n"
      [ "(;SZ[19]PL[B]"
      , ";W[ec](;B[bc](;W[eb](;B[bb]))))"
      ]

  checkLoadGame = case SGF.parse "(;B[])" of
    Right sgf -> case Baduk.load sgf of
      Right _ -> pure unit
      Left e -> throwError (error $ show e)
    Left e -> throwError (error $ show e)

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
