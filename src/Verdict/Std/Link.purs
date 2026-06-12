-- | Links the standard-library prelude into a user module and tree-shakes the
-- | result: only functions reachable from the entry point survive. This keeps
-- | output lean and—because unused builtins are dropped—keeps inferred
-- | `capabilities` precise. User declarations shadow prelude ones of the same
-- | name.
module Verdict.Std.Link (link) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Verdict.Syntax.AST (Decl(..), Expr(..), Module(..), Name, Pattern(..), TypeDecl(..), declName, moduleDecls, moduleName, moduleTypes)

-- | Names referenced by an expression (potential function calls / value refs),
-- | minus the given locally-bound names (params, let bindings) which shadow.
refs :: Set Name -> Set Name -> Expr -> Set Name
refs ctors bound = case _ of
  EAt _ e -> refs ctors bound e
  ELit _ -> Set.empty
  EVar n -> if Set.member n bound || Set.member n ctors then Set.empty else Set.singleton n
  EBin _ a b -> refs ctors bound a <> refs ctors bound b
  ECmp _ a b -> refs ctors bound a <> refs ctors bound b
  EIf c t e -> refs ctors bound c <> refs ctors bound t <> refs ctors bound e
  ELet n e body -> refs ctors bound e <> refs ctors (Set.insert n bound) body
  ECall f args ->
    let base = if Set.member f bound || Set.member f ctors || isIntrinsic f then Set.empty else Set.singleton f
    in base <> foldRefs bound args
  EBuiltin _ args -> foldRefs bound args
  EList xs -> foldRefs bound xs
  ERecord fields -> foldRefs bound (map snd fields)
  EField e _ -> refs ctors bound e
  ESwitch s arms -> refs ctors bound s <> foldRefs bound (map snd arms)
  EMatch s arms ->
    refs ctors bound s
      <> Array.foldl
        ( \acc (Tuple pat body) ->
            acc <> refs ctors (patternBound pat <> bound) body
        )
        Set.empty
        arms
  where
  foldRefs b = Array.foldl (\acc e -> acc <> refs ctors b e) Set.empty

  patternBound = case _ of
    PWild -> Set.empty
    PCtor _ names -> Set.fromFoldable names

isIntrinsic :: Name -> Boolean
isIntrinsic = case _ of
  "length" -> true
  "get" -> true
  "append" -> true
  "mod" -> true
  "spawn" -> true
  "send" -> true
  "recv" -> true
  "yield" -> true
  "self" -> true
  _ -> false

declRefs :: Set Name -> Decl -> Set Name
declRefs ctors (Decl d) = refs ctors (Set.fromFoldable d.params) d.body

-- | Names reachable from `entry` through the call graph.
reachable :: Set Name -> Map Name Decl -> Name -> Set Name
reachable ctors declMap entry = go Set.empty [ entry ]
  where
  go seen frontier = case Array.uncons frontier of
    Nothing -> seen
    Just { head, tail }
      | Set.member head seen -> go seen tail
      | otherwise ->
          let
            seen' = Set.insert head seen
            outs = case Map.lookup head declMap of
              Just d -> Array.fromFoldable (declRefs ctors d)
              Nothing -> []
          in
            go seen' (tail <> outs)

entryName :: Array Decl -> Name
entryName decls =
  case Array.find (\d -> declName d == "main") decls of
    Just _ -> "main"
    Nothing -> case Array.head decls of
      Just d -> declName d
      Nothing -> "main"

link :: Module -> Module -> Module
link userMod preludeMod =
  let
    userDecls = moduleDecls userMod
    userTypes = moduleTypes userMod
    userTypeNames = Set.fromFoldable (map typeName userTypes)
    typeExtra = Array.filter (\t -> not (Set.member (typeName t) userTypeNames)) (moduleTypes preludeMod)
    allTypes = userTypes <> typeExtra
    ctorSet = Set.fromFoldable (Array.concatMap ctorNames allTypes)
    userNames = Set.fromFoldable (map declName userDecls)
    preludeExtra = Array.filter (\d -> not (Set.member (declName d) userNames)) (moduleDecls preludeMod)
    allDecls = userDecls <> preludeExtra
    declMap = Map.fromFoldable (map (\d -> Tuple (declName d) d) allDecls)
    keep = reachable ctorSet declMap (entryName userDecls)
    kept = Array.filter (\d -> Set.member (declName d) keep) allDecls
  in
    Module (moduleName userMod) allTypes kept

typeName :: TypeDecl -> Name
typeName (TypeDecl n _ _) = n

ctorNames :: TypeDecl -> Array Name
ctorNames (TypeDecl _ _ ctors) = map _.name ctors
