module ParserCombinator where

import AOC2022 (readInt)
import           Text.Printf
import qualified Data.Char (isDigit, isSpace)

type Input = String
type Remainder = String

newtype Parser a = Parser { runParser :: Input -> (Remainder, Either ParseError a) }

type ParseErrorEncountered = String
type ParseErrorExpected = String
data ParseError = ParseError ParseErrorExpected ParseErrorEncountered 

instance Show ParseError where
  show (ParseError e f) = printf "expected %s but found %s" e f

parseError :: ParseErrorExpected -> ParseErrorEncountered -> Parser a
parseError expected encountered = Parser $ \input -> (input, Left $ ParseError expected encountered)

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \input -> case runParser p input of
        (xs, Right a)   -> (xs, Right (f a))
        (xs, Left e)    -> (xs, Left e)

instance Applicative Parser where
    pure a = Parser $ \input -> (input, Right a)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    (<*>) p q = Parser $ \input -> case runParser p input of
        (xs, Right f)   -> case runParser q input of
            (xs, Right a)   -> (xs, Right (f a))
            (xs, Left e)    -> (xs, Left e)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser $ \input -> case runParser p input of
        (xs, Right a)   -> runParser (f a) xs
        (xs, Left e)    -> (xs, Left e)
    return = pure

any :: Parser Char
any = Parser $ \input -> case input of
    (x:xs)  -> (xs, Right x)
    []      -> ("", Left $ ParseError "any character" "end of input")

eof :: Parser ()
eof = Parser $ \input -> case input of
    []      -> ("", Right ())
    x:_     -> ("", Left $ ParseError "end of input" [x])

eos :: Parser ()
eos = Parser $ \input -> case input of
  []        -> ("", Right ())
  x:_     -> ("", Left $ ParseError "end of string" [x])

-- if the first parser fails use the second parser, if both fails return error
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \input -> case runParser p input of
  (_, Left err)   -> runParser q input
  otherwise       -> otherwise

choice :: ParseErrorExpected -> [Parser a] -> Parser a
choice expected = foldr (<|>) parseErr
  where parseErr = parseError expected "no match"

-- zero to many
many :: Parser a -> Parser [a]
many  p = many1 p <|> return []

-- one to many
many1 :: Parser a -> Parser [a]
many1 p = do
  x   <- p
  xs  <- many p
  return (x:xs)

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy  p s = sepBy1 p s <|> return []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = do
  x   <- p
  xs  <- many (s >> p)
  return (x:xs)

try :: Parser a -> Parser a
try p = Parser $ \state -> case runParser p state of
  (_newState, Left err) -> (state, Left err)
  success               -> success

satisfy :: ParseErrorExpected -> (Char -> Bool) -> Parser Char
satisfy expected predicate = try $ do
  c <- ParserCombinator.any
  if predicate c
    then return c
    else parseError expected [c]

char c = satisfy ("char "++[c])     (== c)
space  = satisfy "space"            (== ' ')
digit  = satisfy "digit" Data.Char.isDigit
newline = satisfy "newline"         (== '\n')
string [] = return []
string (s:ss) = do
    _  <- char s >> string ss
    return (s:ss)

int = do
    ds <- ParserCombinator.many1 digit
    return $ readInt ds

run :: Parser a -> String -> (String, Either ParseError a)
run p s = runParser p s