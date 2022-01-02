module Main where

import safe Prettyprinter (Pretty(pretty))
import safe Text.Megaparsec (parse)

import safe qualified DSL.Lexer as Lexer
import safe qualified DSL.Parser as Parser
import safe qualified DSL2LilyPond.Translate as Translator
import safe qualified LilyPond.Pretty ()

main :: IO ()
main = do
  input <- getContents
  let ~(Right tokens) = parse Lexer.tokens "stdio" input
  let ~(Right file) = parse Parser.file "stdio" tokens
  let translation = Translator.translate file
  print $ pretty translation
