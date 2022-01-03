{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NoteName.Pretty
  (
  ) where

import safe NoteName (NoteName(..))
import safe Prettyprinter (Pretty(pretty))

instance Pretty NoteName where
  pretty =
    pretty . \case
      A -> "a"
      B -> "b"
      C -> "c"
      D -> "d"
      E -> "e"
      F -> "f"
      G -> "g"
