-- | Multi-file resolution. Given a set of parsed modules and an entry module,
-- | gather everything reachable through `import`s and MERGE it into a single
-- | flat `Module` for the existing single-module pipeline. v1 model: one flat
-- | global namespace (so top-level names must be unique across files); imports
-- | are validated against each module's export list; cyclic imports are allowed
-- | (the flat merge is order-independent). Strict "may only reference imported
-- | names" enforcement is not done yet — that's a later refinement.
module Verdict.Std.Resolve (resolveProject) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Verdict.Syntax.AST (Exposing(..), Module(..), Name, ParsedModule, TypeDecl(..), declName, inputName, moduleDecls, moduleInputs, moduleName, moduleTypes)

resolveProject :: Map Name ParsedModule -> Name -> Either String Module
resolveProject mods entry = do
  _ <- requireModule entry
  reachable <- gather Set.empty [ entry ]
  let reachMods = Array.mapMaybe (\n -> Map.lookup n mods) (Set.toUnfoldable reachable)
  traverse_ checkModuleImports reachMods
  let allDecls = Array.concatMap (moduleDecls <<< _.mod) reachMods
  let allTypes = Array.concatMap (moduleTypes <<< _.mod) reachMods
  let allInputs = Array.concatMap (moduleInputs <<< _.mod) reachMods
  checkUnique "definition" (map declName allDecls)
  checkUnique "input" (map inputName allInputs)
  checkUnique "constructor" (Array.concatMap ctorNames allTypes)
  checkUnique "type" (map typeName allTypes)
  pure (Module entry allTypes allInputs allDecls)
  where
  requireModule n = case Map.lookup n mods of
    Just pm -> Right pm
    Nothing -> Left ("module not found: '" <> n <> "'")

  -- BFS over the import graph; the visited set makes cyclic imports safe.
  gather :: Set Name -> Array Name -> Either String (Set Name)
  gather seen frontier = case Array.uncons frontier of
    Nothing -> Right seen
    Just { head, tail }
      | Set.member head seen -> gather seen tail
      | otherwise -> do
          pm <- requireModule head
          gather (Set.insert head seen) (tail <> map _.mod pm.imports)

  checkModuleImports pm = traverse_ (checkImport (moduleName pm.mod)) pm.imports

  checkImport here imp = do
    target <- requireModule imp.mod
    case imp.names of
      ExposeAll -> Right unit
      ExposeNames ns -> traverse_ (checkExposed here imp.mod (exportsOf target)) ns

  checkExposed here from exports n =
    if Set.member n exports then Right unit
    else Left ("module '" <> here <> "' imports '" <> n <> "' from '" <> from
      <> "', which does not export it")

  -- A module's exported names: everything if `exposing (..)`, else the list.
  exportsOf :: ParsedModule -> Set Name
  exportsOf pm = case pm.exposing of
    ExposeAll ->
      Set.fromFoldable (map declName (moduleDecls pm.mod))
        <> Set.fromFoldable (Array.concatMap ctorNames (moduleTypes pm.mod))
        <> Set.fromFoldable (map typeName (moduleTypes pm.mod))
    ExposeNames ns -> Set.fromFoldable ns

checkUnique :: String -> Array Name -> Either String Unit
checkUnique what = go Set.empty
  where
  go seen arr = case Array.uncons arr of
    Nothing -> Right unit
    Just { head, tail }
      | Set.member head seen -> Left ("duplicate " <> what <> " across modules: '" <> head <> "'")
      | otherwise -> go (Set.insert head seen) tail

ctorNames :: TypeDecl -> Array Name
ctorNames (TypeDecl _ _ ctors) = map _.name ctors

typeName :: TypeDecl -> Name
typeName (TypeDecl n _ _) = n
