module Test.Main where

import Data.Maybe (Maybe(..))
import Baduk.Converter (load)
import Data.Array (intercalate)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (concat)
import Data.Array as A
import Data.Either (Either(..))
import Data.Functor (map)
import Data.List (List)
import Data.String (null)
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Prelude (Unit, pure, show, unit, ($), (<>), (==), discard)
import SGF.Parser (parse)
import SGF.Types (GameTree, demo)
import SGF (loadBaduk)
import Baduk.Converter (showBoard)
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
  where
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
