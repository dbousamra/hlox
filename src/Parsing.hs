module Parsing where

import           Data.List                  (find)
import           Data.Void
-- this module contains commonly useful tools:
import           Text.Megaparsec            hiding (Token, token, tokens)
-- if you parse a stream of characters
import           Text.Megaparsec.Char
-- for lexing of character streams
import qualified Text.Megaparsec.Char.Lexer as L

import           Token

-- Since we don’t need custom data in error messages
-- and our input stream will be in the form of a String,
-- the following definition of Parser will do:
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

token :: String -> TokenType -> Parser Token
token s t = do
  pos <- getPosition
  p <- symbol s
  pure $ Token t p (unPos $ sourceLine pos)


tokenP :: Parser String -> TokenType -> Parser Token
tokenP parser t = do
  pos <- getPosition
  p <- parser
  pure $ Token t p (unPos $ sourceLine pos)

leftParen :: Parser Token
leftParen = token "(" LEFT_PAREN

rightParen :: Parser Token
rightParen = token ")" RIGHT_PAREN

leftBrace :: Parser Token
leftBrace = token "{" LEFT_BRACE

rightBrace :: Parser Token
rightBrace = token "}" RIGHT_BRACE

comma :: Parser Token
comma = token "," COMMA

dot :: Parser Token
dot = token "." DOT

minus :: Parser Token
minus = token "-" MINUS

plus :: Parser Token
plus = token "+" PLUS

semicolon :: Parser Token
semicolon = token ";" SEMICOLON

star :: Parser Token
star = token "*" STAR

bang :: Parser Token
bang = token "!" BANG

bangEqual :: Parser Token
bangEqual = token "!=" BANG_EQUAL

equal :: Parser Token
equal = token "=" EQUAL

equalEqual :: Parser Token
equalEqual = token "==" EQUAL_EQUAL

less :: Parser Token
less = token "<" LESS

lessEqual :: Parser Token
lessEqual = token "<=" LESS_EQUAL

greater :: Parser Token
greater = token ">" GREATER

greaterEqual :: Parser Token
greaterEqual = token ">=" GREATER_EQUAL

slash :: Parser Token
slash = token "/" SLASH

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

stringLiteral :: Parser Token
stringLiteral = tokenP stringP STRING where
  stringP :: Parser String
  stringP = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

numberLiteral :: Parser Token
numberLiteral = tokenP (show <$> L.scientific) NUMBER where

identifier :: Parser Token
identifier = do
  pos <- getPosition
  s <- letterChar <|> (char '_')
  xs <- many alphaNumChar
  let str = [s] ++ xs
  case find (\r -> fst r == str) reservedWords of
    (Just (_, tokenType)) -> pure $ Token tokenType str (unPos $ sourceLine pos)
    Nothing               -> pure $ Token IDENTIFIER str (unPos $ sourceLine pos)

eofP :: Parser Token
eofP = tokenP e EOF where
  e = const "end of file" <$> eof

tokens :: Parser Token
tokens = choice
  [ leftParen
  , rightParen
  , leftBrace
  , rightBrace
  , comma
  , dot
  , minus
  , plus
  , semicolon
  , star
  , choice [bangEqual, bang]
  , choice [equalEqual, equal]
  , choice [lessEqual, less]
  , choice [greaterEqual, greater]
  , slash
  , stringLiteral
  , numberLiteral
  , identifier
  , eofP
  ]

manyTokens :: Parser [Token]
manyTokens = many tokens

parseToken :: String -> Either String Token
parseToken input = case parse tokens "" input of
  Left e  -> Left $ parseErrorPretty e
  Right o -> Right o


example :: String
example = "(( )){} // grouping stuff\n!*+-/=<> <= == // operators"
