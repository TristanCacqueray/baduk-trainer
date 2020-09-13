module Test.Main where

import Baduk.Converter (load)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.List (List)
import Data.String (null)
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Prelude (Unit, pure, show, unit, ($), (<>), discard)
import SGF.Parser (parse)
import SGF.Types (GameTree, demo)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

checkLoader :: forall m. MonadThrow Error m => List GameTree -> m Unit
checkLoader sgf = do
  case load sgf of
    Left err → throwError (error err)
    Right (Tuple _ _) → pure unit

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
  where
  parserTests =
    concat
      [ map (\expr → (it ("parse " <> expr) $ checkParser true expr)) goodExprs
      , map (\expr → (it ("error " <> if null expr then "(empty)" else expr) $ checkParser false expr)) badExprs
      ]
