{-# LANGUAGE Safe #-}

module NoteName where

import safe Data.Data (Data)

data NoteName
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Show, Data)
