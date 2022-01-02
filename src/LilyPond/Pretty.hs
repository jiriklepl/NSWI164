{-# OPTIONS_GHC -Wno-orphans #-}

module LilyPond.Pretty
  (
  ) where

import safe Prettyprinter
  ( Doc
  , Pretty(pretty)
  , (<+>)
  , dquotes
  , enclose
  , equals
  , hang
  , hsep
  , indent
  , lbrace
  , line
  , rbrace
  , slash
  )

import safe LilyPond.AST
  ( Absolute(..)
  , File(..)
  , Header(..)
  , Key(..)
  , Lyrics(..)
  , Note(..)
  , Scale(..)
  , Score(..)
  , Staff(Staff)
  , Time(..)
  , Version(..)
  )
import safe NoteName.Pretty ()

dline :: Doc ann
dline = line <> line

ldarrow :: Doc ann
ldarrow = pretty "<<"

rdarrow :: Doc ann
rdarrow = pretty ">>"

nestedBlock :: Doc ann -> Doc ann -> Doc ann -> Doc ann
nestedBlock l r = enclose (l <> line) (line <> r) . indent 2

bracedBlock :: Doc ann -> Doc ann
bracedBlock = nestedBlock lbrace rbrace

instance Pretty File where
  pretty File {version, header, score} =
    pretty version <> dline <> pretty header <> dline <> pretty score

instance Pretty Version where
  pretty (Version version) = pretty "\\version " <> dquotes (pretty version)

instance Pretty Header where
  pretty (Header title) =
    pretty "\\header" <+>
    bracedBlock (pretty "title" <+> equals <+> dquotes (pretty title))

instance Pretty Score where
  pretty Score {staff, lyrics} =
    pretty "\\score" <+>
    bracedBlock
      (nestedBlock ldarrow rdarrow (pretty staff <> dline <> pretty lyrics))

instance Pretty Staff where
  pretty (Staff key time absolute) =
    pretty "\\new Staff" <+>
    bracedBlock (pretty key <> line <> pretty time <> dline <> pretty absolute)

instance Pretty Lyrics where
  pretty (Lyrics syllables) =
    pretty "\\addlyrics" <+> bracedBlock (hsep $ dquotes . pretty <$> syllables)

instance Pretty Key where
  pretty (Key name scale) = pretty "\\key" <+> pretty name <+> pretty scale

instance Pretty Scale where
  pretty Major = pretty "\\major"
  pretty Minor = pretty "\\minor"

instance Pretty Time where
  pretty (Time beats unit) =
    pretty "\\time" <+> pretty beats <> slash <> pretty unit

instance Pretty Absolute where
  pretty (Absolute notes) =
    pretty "\\absolute" <+> bracedBlock (hang 5 . hsep $ pretty <$> notes)

instance Pretty Note where
  pretty (Note name octave duration) =
    pretty name <> pretty (commas octave) <> pretty duration
    where
      commas int
        | int < 0 = replicate (-int) ','
        | int > 0 = replicate int '\''
        | otherwise = mempty
