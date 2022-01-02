module LilyPond.AST where

import safe NoteName (NoteName)

data File =
  File
    { version :: Version
    , header :: Header
    , score :: Score
    }
  deriving (Show)

newtype Version =
  Version String
  deriving (Show)

newtype Header =
  Header String
  deriving (Show)

data Score =
  Score
    { staff :: Staff
    , lyrics :: Lyrics
    }
  deriving (Show)

data Staff =
  Staff
    { key :: Key
    , time :: Time
    , absolute :: Absolute
    }
  deriving (Show)

newtype Lyrics =
  Lyrics [String]
  deriving (Show)

data Key =
  Key NoteName Scale
  deriving (Show)

data Scale
  = Major
  | Minor
  deriving (Show)

data Time =
  Time Int Int
  deriving (Show)

newtype Absolute =
  Absolute [Note]
  deriving (Show)

data Note =
  Note NoteName Int Int
  deriving (Show)
