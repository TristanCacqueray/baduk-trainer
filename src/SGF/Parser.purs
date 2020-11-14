module SGF.Parser where

import Data.Maybe (Maybe(..), optional)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.List (many, some)
import Data.Number (fromString)
import Data.String.CodeUnits (fromCharArray)
import Prelude (bind, pure, (>>=), ($), (<>), (-), (/=), show)
import Text.Parsing.Parser (ParseError, Parser, runParser, fail)
import Text.Parsing.Parser.Combinators (between, try)
import Text.Parsing.Parser.String (char, string, satisfy)
import Text.Parsing.Parser.Token (digit, letter)
import SGF.Types (Color(..), GameTree(..), Node, Property(..), SGF, Sequence, Value(..))

type P a
  = Parser String a

propertyValue :: P Value
propertyValue =
  pNone <|> (between (string "[") (string "]") $ try pDate <|> pNum <|> pText)
    >>= \v -> optional (string "\n") >>= \_ -> pure v
  where
  num s = fromString (fromCharArray s)

  pNum :: P Value
  pNum = do
    numS <- A.some (digit <|> char '.')
    case num numS of
      Just a -> pure $ Num a
      Nothing -> fail ("Invalid Num: " <> fromCharArray numS)

  pDate :: P Value
  pDate = do
    numY <- A.some digit
    _ <- char '-'
    numM <- A.some digit
    _ <- char '-'
    numD <- A.some digit
    case [ num numY, num numM, num numD ] of
      [ Just y, Just m, Just d ] -> pure (Date y m d)
      _ -> fail ("Invalid Date")

  pText :: P Value
  pText = do
    txt <- (A.some $ satisfy ((/=) ']'))
    pure
      ( case txt of
          [ 'B' ] -> Color Black
          [ 'W' ] -> Color White
          [ a, b ] -> Point (pointPos a) (pointPos b)
          _ -> Text (fromCharArray txt)
      )
    where
    pointPos ∷ Char → Int
    pointPos c = toCharCode c - toCharCode 'a'

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
