module SGF.Parser where

import Prelude (bind, pure, ($))
import Data.Either (Either)
-- import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.List (List, some, many)
import Data.Show
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (between)
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

data Node
  = None

instance showMyRecord :: Show GameTree where
  show _ = "A GameTree"

node :: P Node
node = do
  _ <- char 'N'
  pure None

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
