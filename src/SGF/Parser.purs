module SGF.Parser (parse) where

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some) as A
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.List (many, some)
import Data.Maybe (Maybe(..), optional)
import Data.Number (fromString)
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, runParser, fail)
import Parsing.Combinators (between, try)
import Parsing.String (char, string, satisfy)
import Parsing.Token (digit, letter)
import Prelude (bind, pure, ($), (&&), (-), (/=), (<=), (<>), (>=), (>>=))
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
          [ a, b ] ->
            if inRange a && inRange b then
              Point (pointPos a) (pointPos b)
            else
              Text (fromCharArray [ a, b ])
          _ -> Text (fromCharArray txt)
      )
    where
    inRange :: Char -> Boolean
    inRange c = c >= 'a' && c <= 'z'

    pointPos ∷ Char → Int
    pointPos c = toCharCode c - toCharCode 'a'

  pNone :: P Value
  pNone = do
    _ <- string "[]"
    pure None

propertyName :: P String
propertyName = do
  lets <- A.some letter
  pure $ fromCharArray lets

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

-- note: this is the `fix` trick to enable recursive parser
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
