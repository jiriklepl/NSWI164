{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DSL.Parser.Token where

import safe qualified Prelude as Base

data Token
  = String Base.String
  | BackSlash
  | Song
  | Phrase
  | LBrace
  | RBrace
  | Plus
  | Minus
  | Ident Base.String
  | Number Base.Int
  deriving (Base.Eq, Base.Ord)
