{-# LANGUAGE Safe #-}

module DSL.AST where

import safe Data.Data (Data)

import safe NoteName (NoteName)

data Note =
  Note NoteName (Maybe Octave) Duration (Maybe Syllable)
  deriving (Show, Data)

newtype Octave =
  Octave Int
  deriving (Show, Data)

newtype Duration =
  Duration Int
  deriving (Show, Data)

newtype Syllable =
  Syllable String
  deriving (Show, Data)

data File =
  File Song [Phrase]
  deriving (Show, Data)

data Song =
  Song String Body
  deriving (Show, Data)

data Phrase =
  Phrase PhraseName Body
  deriving (Show, Data)

newtype PhraseName =
  PhraseName String
  deriving (Show, Data)

type Body = [Either PhraseName Note]
