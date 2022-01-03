{-# LANGUAGE Safe #-}

module DSL.Parser
  ( file
  ) where

import safe Control.Applicative (Applicative(liftA2), optional)
import safe Data.Void (Void)
import safe Text.Megaparsec
  ( MonadParsec(token)
  , Parsec
  , between
  , choice
  , many
  , single
  )

import safe DSL.AST
  ( Body
  , Duration(..)
  , File(..)
  , Note(..)
  , Octave(..)
  , Phrase(..)
  , PhraseName(..)
  , Song(..)
  , Syllable(..)
  )
import safe qualified DSL.Parser.Token as Tok
import safe DSL.Parser.Token (Token)
import safe NoteName (NoteName(..))

type Parser = Parsec Void [Token]

file :: Parser File
file = do
  ps <- many phrase
  s <- song
  ps' <- many phrase
  return . File s $ ps ++ ps'

note :: Parser Note
note = Note <$> noteName <*> optional octave <*> duration <*> optional syllable

noteName :: Parser NoteName
noteName =
  (`token` mempty) $ \case
    Tok.Ident "a" -> Just A
    Tok.Ident "b" -> Just B
    Tok.Ident "c" -> Just C
    Tok.Ident "d" -> Just D
    Tok.Ident "e" -> Just E
    Tok.Ident "f" -> Just F
    Tok.Ident "g" -> Just G
    _ -> Nothing

octave :: Parser Octave
octave =
  Octave <$>
  choice [single Tok.Plus *> number, single Tok.Minus *> (negate <$> number)]

duration :: Parser Duration
duration = Duration <$> number

syllable :: Parser Syllable
syllable = Syllable <$> string

string :: Parser String
string =
  (`token` mempty) $ \case
    Tok.String str -> Just str
    _ -> Nothing

number :: Parser Int
number =
  (`token` mempty) $ \case
    Tok.Number int -> Just int
    _ -> Nothing

song :: Parser Song
song = single Tok.Song *> liftA2 Song string body

phrase :: Parser Phrase
phrase = single Tok.Phrase *> liftA2 Phrase phraseName body

phraseName :: Parser PhraseName
phraseName =
  (`token` mempty) $ \case
    Tok.Ident name -> Just $ PhraseName name
    _ -> Nothing

body :: Parser Body
body = between (single Tok.LBrace) (single Tok.RBrace) $ many bodyItem

bodyItem :: Parser (Either PhraseName Note)
bodyItem =
  choice [Left <$> (single Tok.BackSlash *> phraseName), Right <$> note]
