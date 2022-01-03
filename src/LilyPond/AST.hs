{-# LANGUAGE Safe #-}

module LilyPond.AST where

import safe Data.Data (Data)

import safe NoteName (NoteName)

data File =
  File
    { version :: Version
    , header :: Header
    , score :: Score
    }
  deriving (Show, Data)

newtype Version =
  Version String
  deriving (Show, Data)

newtype Header =
  Header String
  deriving (Show, Data)

data Score =
  Score
    { staff :: Staff
    , lyrics :: Lyrics
    }
  deriving (Show, Data)

data Staff =
  Staff
    { key :: Key
    , time :: Time
    , absolute :: Absolute
    }
  deriving (Show, Data)

newtype Lyrics =
  Lyrics [String]
  deriving (Show, Data)

data Key =
  Key NoteName Scale
  deriving (Show, Data)

data Scale
  = Major
  | Minor
  deriving (Show, Data)

data Time =
  Time Int Int
  deriving (Show, Data)

newtype Absolute =
  Absolute [Note]
  deriving (Show, Data)

data Note =
  Note NoteName Int Int
  deriving (Show, Data)
