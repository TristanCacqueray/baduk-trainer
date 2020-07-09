module SGF.Parser where

import Prelude (bind, pure, ($))
import Data.Either (Either)
import Data.Maybe
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.List (List, some, many)
import Data.Number (fromString)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParseError, Parser, runParser, fail)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Token (digit, letter)
import Text.Parsing.Parser.String (char, string)

type P a
  = Parser String a

-- https://www.red-bean.com/sgf/sgf4.html
type SGF
  = Collection

type Collection
  = List GameTree

data GameTree
  = GameTree Sequence (List GameTree)

type Sequence
  = (List Node)

type Node
  = (List Property)

data Property
  = Prop String (List Value)

data Value
  = PNum Number
  | None

instance showSGF :: Show GameTree where
  show _ = "A GameTree"

instance showValue :: Show Value where
  show _ = "A Value"

propertyValue :: P Value
propertyValue = pNone <|> (between (string "[") (string "]") $ pNum)
  where
  pNum :: P Value
  pNum = do
    numS <- A.some digit
    case fromString (fromCharArray numS) of
      Just a -> pure $ PNum a
      Nothing -> fail ("Invalid pNum: " <> fromCharArray numS)

  pNone :: P Value
  pNone = do
    _ <- string "[]"
    pure None

propertyName :: P String
propertyName = do
  lets <- some letter
  pure $ show lets

property :: P Property
property = do
  name <- propertyName
  values <- some propertyValue
  pure $ Prop name values

node :: P Node
node = do
  _ <- char ';'
  props <- many property
  pure props

sequence :: P Sequence
sequence = do
  n <- some node
  pure $ n

gameTree :: P GameTree -> P GameTree
gameTree p = do
  gt <- between (string "(") (string ")") gameTree'
  pure $ gt
  where
  gameTree' :: P GameTree
  gameTree' = do
    s <- sequence
    ts <- many (gameTree p)
    pure $ GameTree s ts

parse :: String -> Either ParseError SGF
parse expr = runParser expr (some (fix gameTree))
