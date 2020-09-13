module SGF.Parser where

import Data.Maybe
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.List (many, some)
import Data.Number (fromString)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodeUnits (fromCharArray)
import Prelude (bind, pure, ($), (*>), (-))
import Text.Parsing.Parser (ParseError, Parser, runParser, fail)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.String (char, string)
import Text.Parsing.Parser.Token (digit, letter)
import SGF.Types (Color(..), GameTree(..), Node, Property(..), SGF, Sequence, Value(..))

type P a
  = Parser String a

propertyValue :: P Value
propertyValue = pNone <|> (between (string "[") (string "]") $ (pNum <|> pBlack <|> pWhite <|> pPoint))
  where
  pNum :: P Value
  pNum = do
    numS <- A.some (digit <|> char '.')
    case fromString (fromCharArray numS) of
      Just a -> pure $ Num a
      Nothing -> fail ("Invalid Num: " <> fromCharArray numS)

  pBlack = char 'B' *> pure (Color Black)

  pWhite = char 'W' *> pure (Color White)

  pPoint ∷ P Value
  pPoint = do
    col ← letter
    row ← letter
    pure (Point (pointPos col) (pointPos row))
    where
    pointPos ∷ Char → Int
    pointPos c = toCharCode c - pos0

    pos0 ∷ Int
    pos0 = toCharCode 'a'

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
