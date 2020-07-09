module Test.Main where

import Prelude (Unit, pure, show, unit, ($), (<>))
import Effect (Effect)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Array (concat)
import Data.Functor (map)
import Data.Either (Either(..))
import Data.String (null)
import Data.Traversable (sequence_)
import SGF.Parser (parse)

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
  where
  parserTests =
    concat
      [ map (\expr → (it ("parse " <> expr) $ checkParser true expr)) goodExprs
      , map (\expr → (it ("error " <> if null expr then "(empty)" else expr) $ checkParser false expr)) badExprs
      ]
