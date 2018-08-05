module ParsingSpec where

import           Parsing
import           Test.Tasty
import           Test.Tasty.HUnit
import           Token

test :: TestTree
test = testGroup "parsing" $ fmap (uncurry parseTest) ts where
  ts = [
      (LEFT_PAREN, "(")
    , (RIGHT_PAREN, ")")
    , (LEFT_BRACE, "{")
    , (RIGHT_BRACE, "}")
    , (COMMA, ",")
    , (DOT, ".")
    , (MINUS, "-")
    , (PLUS, "+")
    , (SEMICOLON, ";")
    , (SLASH, "/")
    , (STAR, "*")
    , (BANG, "!")
    , (BANG_EQUAL, "!=")
    , (EQUAL, "=")
    , (EQUAL_EQUAL, "==")
    , (GREATER, ">")
    , (GREATER_EQUAL, ">=")
    , (LESS, "<")
    , (LESS_EQUAL, "<=")
    , (AND, "and")
    , (CLASS, "class")
    , (ELSE, "else")
    , (FALSE, "false")
    , (FOR, "for")
    , (FUN, "fun")
    , (IF, "if")
    , (NIL, "nil")
    , (OR, "or")
    , (PRINT, "print")
    , (RETURN, "return")
    , (SUPER, "super")
    , (THIS, "this")
    , (TRUE, "true")
    , (VAR, "var")
    , (WHILE, "while")
    , (EOF, "")
    ]

assertRight :: (Eq a, Show a) => String -> a -> Either String a -> Assertion
assertRight message expected actual = case actual of
  Left e  -> assertFailure e
  Right o -> assertEqual message expected o

assertParser :: String -> String -> TokenType -> Assertion
assertParser message input expected = assertRight message expected (t_type <$> parseToken input)

parseTest :: TokenType -> String -> TestTree
parseTest token input = testCase name $ assertParser message input token
  where
    name = show token
    message = name ++ " is parsed"


