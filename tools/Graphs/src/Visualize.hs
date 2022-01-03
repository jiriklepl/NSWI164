{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visualize where

import safe qualified Data.Set as Set
import safe Data.Set (Set)
import safe Language.Haskell.TH
  ( Con(InfixC, NormalC, RecC)
  , Dec(DataD, NewtypeD, TySynD)
  , Info(PrimTyConI, TyConI)
  , Name
  , Q
  , Type(AppT, ConT, ListT, ParensT, TupleT, VarT)
  , nameBase
  , nameModule
  , reify
  )

fullName :: Name -> String
fullName name =
  case nameModule name of
    Nothing -> base
    Just modul ->
      ((\case
          '.' -> '_'
          c -> c) <$>
       modul) <>
      "_" <> base
    where base = nameBase name

buildList :: Name -> Q (Set String)
buildList t =
  reify t >>= \case
    TyConI dec ->
      case dec of
        DataD _ _ _ _ cons _ -> do
          let names = snd . doCon <$> cons
          let names' = Just . fst . doCon <$> cons
          b <-
            (Set.unions <$>) . traverse buildList . concat . concat $
            ((\case
                _:t'@(_:_) -> t'
                a -> a) <$>) <$>
            names
          return $
            if all null . concat $ names
              then b
              else Set.fromList
                     (zipWith (`transform` length names') names' names) `Set.union`
                   b
        NewtypeD _ _ _ _ con _ -> do
          let name' = snd $ doCon con
          let name'' = Just . fst $ doCon con
          b <-
            (Set.unions <$>) . traverse buildList . concat $
            (\case
               _:t'@(_:_) -> t'
               a -> a) <$>
            name'
          return $
            if all null name'
              then b
              else transform name'' (1 :: Int) name' `Set.insert` b
        TySynD name _ t' -> do
          let name' = [mineNames t']
          b <-
            (Set.unions <$>) . traverse buildList . concat $
            (\case
               _:t''@(_:_) -> t''
               a -> a) <$>
            name'
          return $
            if all null name'
              then b
              else transform (Just name) (1 :: Int) name' `Set.insert` b
        e -> error $ "not implemented: " <> show e
    PrimTyConI {} -> return mempty
    e -> error $ "not implemented: " <> show e
  where
    transform mName cNumber names =
      case mName of
        Just name ->
          (fullName t <>
           " -> Con__" <>
           fullName name <>
           "[taillabel=\"1..1\", headlabel=\"" <>
           conNumber cNumber <> "\", dir=both, arrowtail=diamond]; ") <>
          concat childs
        Nothing -> concat childs
      where
        conNumber (1 :: Int) = "1..1"
        conNumber _ = "0..1"
        childs =
          ((\(n :: Int, ((min' :: Int, max' :: Maybe Int), x, t')) ->
              (if fullName t == t'
                 then maybe t' (("Con__" <>) . fullName) mName
                 else t') <>
              " -> " <>
              x <>
              "[taillabel=\"0..*\",headlabel=\"Field" <>
              show n <>
              " " <> show min' <> ".." <> maybe "*" show max' <> "\"]; ") <$>) $
          zip [0 ..] $ concat $ annotate (fullName t) (1, Just 1) <$> names'
        annotate t' count ("GHC_Types_[]":others) =
          annotate' t' count "GHC_Types_List" (0, Nothing) others
        annotate t' count (new@"GHC_Maybe_Maybe":others) =
          annotate' t' count new (0, Just 1) others
        annotate t' count (new@"Data_Either_Either":others) =
          annotate' t' count new (0, Just 1) others
        annotate _ _ [] = []
        annotate t' count (first:others) =
          (count, first, t') : annotate t' count others
        annotate' t' count new count' others =
          if count == (1, Just 1)
            then annotate t' count' others
            else (count, new', t') : annotate new' count' others
          where
            new' = new <> "__" <> t'
        names' = (fullName <$>) <$> names
    doCon =
      \case
        NormalC name bangTypes -> (name, mineNames . snd <$> bangTypes)
        InfixC bangType' name bangType'' ->
          (name, [mineNames (snd bangType') ++ mineNames (snd bangType'')])
        RecC name varBangTypes ->
          (name, mineNames . (\(_, _, a) -> a) <$> varBangTypes)
        e -> error $ "not implemented: " <> show e
    mineNames =
      \case
        ConT name -> filter (notElem '#' . fullName) [name]
        TupleT _ -> []
        VarT _ -> []
        ParensT t' -> mineNames t'
        ListT -> [''[]]
        AppT l r -> mineNames l <> mineNames r
        e -> error $ "not implemented: " <> show e

getGraph :: String -> [Name] -> Q String
getGraph name =
  (("digraph " <> name <> " { " ++) . (++ "}") . concat . Set.toList <$>
   Set.unions <$>) .
  traverse buildList
