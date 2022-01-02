module DSL.Lexer
  ( tokens
  ) where

import safe Control.Applicative (Alternative(empty), Applicative(liftA2))
import safe Data.Functor (void)
import safe Data.Void (Void)
import safe Text.Megaparsec
  ( MonadParsec(eof, notFollowedBy, try)
  , Parsec
  , choice
  , many
  , manyTill
  )
import safe Text.Megaparsec.Char
  ( alphaNumChar
  , char
  , letterChar
  , space1
  , string
  )
import safe qualified Text.Megaparsec.Char.Lexer as L

import safe qualified DSL.Parser.Token as Tok
import safe DSL.Parser.Token (Token)

type Lexer = Parsec Void String

sc :: Lexer ()
sc = L.space space1 empty empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: String -> Lexer String
symbol = L.symbol sc

keyword :: String -> Lexer ()
keyword k = void . lexeme $ try (string k <* notFollowedBy alphaNumChar)

stringLiteral :: Lexer String
stringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

identifier :: Lexer String
identifier = lexeme $ liftA2 (:) letterChar identifierRest

identifierRest :: Lexer String
identifierRest = many alphaNumChar

number :: Lexer Int
number = lexeme L.decimal

tokens :: Lexer [Token]
tokens = sc *> manyTill token eof

token :: Lexer Token
token =
  choice
    [ Tok.Song <$ keyword "song"
    , Tok.Phrase <$ keyword "phrase"
    , Tok.LBrace <$ symbol "{"
    , Tok.RBrace <$ symbol "}"
    , Tok.Plus <$ symbol "+"
    , Tok.Minus <$ symbol "-"
    , Tok.BackSlash <$ symbol "\\"
    , Tok.String <$> stringLiteral
    , Tok.Number <$> number
    , Tok.Ident <$> identifier
    ]
