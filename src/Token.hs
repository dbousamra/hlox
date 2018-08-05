module Token where

data TokenType
    -- Single-character tokens.
    = LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    -- One or two character tokens.
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    -- Literals.
    | IDENTIFIER
    | STRING
    | NUMBER
    -- Keywords.
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF
    -- Throwaway token for comment/whitespace
    | WS
    deriving (Show, Eq, Ord)

reservedWords :: [(String, TokenType)]
reservedWords = [
    ("and",    AND),
    ("class",  CLASS),
    ("else",   ELSE),
    ("false",  FALSE),
    ("for",    FOR),
    ("fun",    FUN),
    ("if",     IF),
    ("nil",    NIL),
    ("or",     OR),
    ("print",  PRINT),
    ("return", RETURN),
    ("super",  SUPER),
    ("this",   THIS),
    ("true",   TRUE),
    ("var",    VAR),
    ("while",  WHILE)
  ]

data Token = Token
  { t_type   :: TokenType
  , t_lexeme :: String
  , t_line   :: Int
  } deriving (Show, Eq, Ord)

data Expr
  = Assign Token Expr
  | Binary Expr Token Expr
  | Call Expr Token [Expr] -- The opening args paren is saved for errors
  | Get Expr Token
  | Grouping Expr
  | Literal
  | Logical Expr Token Expr
  | Set Expr Token Expr
  | Super Token Token
  | This Token
  | Unary Token Expr
  | Variable Token
  deriving (Show, Eq, Ord)
