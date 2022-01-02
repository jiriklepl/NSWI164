module DSL2LilyPond.Translate where

import safe qualified Data.Map as Map
import safe Data.Map (Map)

import safe qualified DSL.AST as DSL
import safe qualified LilyPond.AST as LP
import safe NoteName (NoteName(C))

translate :: DSL.File -> LP.File
translate (DSL.File (DSL.Song title song) phrases) =
  LP.File
    { LP.version = LP.Version "2.18.2"
    , LP.header = LP.Header title
    , LP.score =
        LP.Score
          { LP.staff =
              LP.Staff
                { LP.key = LP.Key C LP.Major
                , LP.time = LP.Time 4 4
                , LP.absolute = LP.Absolute $ fst <$> score
                }
          , LP.lyrics = LP.Lyrics $ snd <$> score
          }
    }
  where
    score = translateScore phraseDB song
    phraseDB =
      Map.fromList $
      (\(DSL.Phrase (DSL.PhraseName name) body) -> (name, body)) <$> phrases

translateScore :: Map String DSL.Body -> DSL.Body -> [(LP.Note, String)]
translateScore _ [] = []
translateScore phraseDB (first:rest) =
  case first of
    Left (DSL.PhraseName name) ->
      case name `Map.lookup` phraseDB of
        Nothing -> error $ "Bad reference: " <> name
        Just body -> translateScore phraseDB (body <> rest)
    Right (DSL.Note name mOctave (DSL.Duration d) mSyllable) ->
      (LP.Note name (octave mOctave) d, syllable mSyllable) :
      translateScore phraseDB rest
      where octave Nothing = 0
            octave (Just (DSL.Octave int)) = int
            syllable Nothing = ""
            syllable (Just (DSL.Syllable str)) = str
