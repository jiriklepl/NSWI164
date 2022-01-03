{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Main where

import safe System.IO

import safe Language.Haskell.TH (stringE)
import safe Prettyprinter (Pretty(pretty))

import safe Visualize (getGraph)

import qualified DSL.AST
import qualified LilyPond.AST
#define store(name, code) do { print $ "generating the " <> name <> " graph"; file <- openFile (name <> ".dot") WriteMode; hPrint file (pretty $(stringE =<< getGraph name code)); hClose file }
main :: IO ()
main = do
  store ("DSL", [''DSL.AST.File])
  store ("LilyPond", [''LilyPond.AST.File])
  store ("Combined", (''DSL.AST.File : ''LilyPond.AST.File : mempty))
