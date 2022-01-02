module DSL.AST where

import safe NoteName (NoteName)

data Note =
  Note NoteName (Maybe Octave) Duration (Maybe Syllable)

newtype Octave =
  Octave Int

newtype Duration =
  Duration Int

newtype Syllable =
  Syllable String

data File =
  File Song [Phrase]

data Song =
  Song String Body

data Phrase =
  Phrase PhraseName Body

newtype PhraseName =
  PhraseName String

type Body = [Either PhraseName Note]
