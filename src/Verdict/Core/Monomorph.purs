module Verdict.Core.Monomorph (monomorphize) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..), snd)
import Verdict.Syntax.AST (Decl(..), Expr(..), Module(..), Name, Pattern(..), Ty(..), moduleDecls, moduleName, moduleTypes, splitArrow, stripAt)

type FnInfo =
  { name :: Name
  , paramNames :: Array Name
  , paramTypes :: Array Ty
  , resultTy :: Ty
  , body :: Expr
  }

type Request =
  { srcName :: Name
  , fnSubst :: Map Name Name
  , tySubst :: Map Name Ty
  , outName :: Name
  }

type MonoState =
  { generated :: Map Name Decl
  , queue :: Array Request
  , steps :: Int
  }

monomorphize :: Module -> Module
monomorphize mod =
  let
    decls = moduleDecls mod
    entry = entryName decls
    initial =
      { srcName: entry
      , fnSubst: Map.empty
      , tySubst: Map.empty
      , outName: entry
      }
    done = processAll { generated: Map.empty, queue: [ initial ], steps: 0 }
  in
    Module (moduleName mod) (moduleTypes mod) (map snd (Map.toUnfoldable done.generated :: Array (Tuple Name Decl)))
  where
  infos :: Map Name FnInfo
  infos = Map.fromFoldable
    (map (\info -> Tuple info.name info) (Array.mapMaybe infoOf (moduleDecls mod)))

  processAll :: MonoState -> MonoState
  processAll st
    | st.steps > 10_000 = st
    | otherwise = case Array.uncons st.queue of
        Nothing -> st
        Just { head: req, tail } ->
          if Map.member req.outName st.generated then
            processAll st { queue = tail, steps = st.steps + 1 }
          else case Map.lookup req.srcName infos of
            Nothing -> processAll st { queue = tail, steps = st.steps + 1 }
            Just info ->
              let
                body0 = renameFns req.fnSubst info.body
                rewritten = rewriteCalls body0
                body1 = rewritten.expr
                valueParams = valueParamEntries info
                sig = foldr TArrow (applySubst req.tySubst info.resultTy)
                  (map (applySubst req.tySubst <<< _.ty) valueParams)
                decl = Decl
                  { name: req.outName
                  , params: map _.name valueParams
                  , sig: Just sig
                  , body: body1
                  }
                generated = Map.insert req.outName decl st.generated
              in
                processAll
                  { generated
                  , queue: tail <> rewritten.requests
                  , steps: st.steps + 1
                  }

  rewriteCalls :: Expr -> { expr :: Expr, requests :: Array Request }
  rewriteCalls = case _ of
    EAt _ e -> rewriteCalls e
    ELit lit -> { expr: ELit lit, requests: [] }
    EVar n ->
      { expr: EVar n
      , requests:
          if Map.member n infos && not (isHOF n) then [ baseRequest n ]
          else []
      }
    EBin op a b ->
      let ra = rewriteCalls a
          rb = rewriteCalls b
      in { expr: EBin op ra.expr rb.expr, requests: ra.requests <> rb.requests }
    ECmp op a b ->
      let ra = rewriteCalls a
          rb = rewriteCalls b
      in { expr: ECmp op ra.expr rb.expr, requests: ra.requests <> rb.requests }
    EIf c t e ->
      let rc = rewriteCalls c
          rt = rewriteCalls t
          re = rewriteCalls e
      in { expr: EIf rc.expr rt.expr re.expr, requests: rc.requests <> rt.requests <> re.requests }
    ELet n e body ->
      let re = rewriteCalls e
          rb = rewriteCalls body
      in { expr: ELet n re.expr rb.expr, requests: re.requests <> rb.requests }
    ECall "spawn" args -> case Array.uncons args of
      Just { head: fnRef, tail: rest } -> case stripAt fnRef of
        EVar worker ->
          let
            rargs = map rewriteCalls rest
            req =
              if Map.member worker infos then [ baseRequest worker ]
              else []
          in
            { expr: ECall "spawn" (EVar worker `Array.cons` map _.expr rargs)
            , requests: Array.concatMap _.requests rargs <> req
            }
        _ ->
          let rargs = map rewriteCalls args
          in { expr: ECall "spawn" (map _.expr rargs), requests: Array.concatMap _.requests rargs }
      Nothing -> { expr: ECall "spawn" [], requests: [] }
    ECall f args ->
      if isHOF f then
        specializeCall f args
      else
        let rargs = map rewriteCalls args
            requests = Array.concatMap _.requests rargs
            req =
              if Map.member f infos then [ baseRequest f ]
              else []
        in { expr: ECall f (map _.expr rargs), requests: requests <> req }
    EBuiltin name args ->
      let rargs = map rewriteCalls args
      in { expr: EBuiltin name (map _.expr rargs), requests: Array.concatMap _.requests rargs }
    EList xs ->
      let rxs = map rewriteCalls xs
      in { expr: EList (map _.expr rxs), requests: Array.concatMap _.requests rxs }
    ERecord fields ->
      let rfields = map (\(Tuple n e) -> let r = rewriteCalls e in { field: Tuple n r.expr, requests: r.requests }) fields
      in { expr: ERecord (map _.field rfields), requests: Array.concatMap _.requests rfields }
    EField e name ->
      let r = rewriteCalls e
      in { expr: EField r.expr name, requests: r.requests }
    ESwitch s arms ->
      let rs = rewriteCalls s
          rarms = map (\(Tuple pat body) -> let r = rewriteCalls body in { arm: Tuple pat r.expr, requests: r.requests }) arms
      in { expr: ESwitch rs.expr (map _.arm rarms), requests: rs.requests <> Array.concatMap _.requests rarms }
    EMatch s arms ->
      let rs = rewriteCalls s
          rarms = map (\(Tuple pat body) -> let r = rewriteCalls body in { arm: Tuple pat r.expr, requests: r.requests }) arms
      in { expr: EMatch rs.expr (map _.arm rarms), requests: rs.requests <> Array.concatMap _.requests rarms }

  specializeCall :: Name -> Array Expr -> { expr :: Expr, requests :: Array Request }
  specializeCall f args = case Map.lookup f infos of
    Nothing ->
      let rargs = map rewriteCalls args
      in { expr: ECall f (map _.expr rargs), requests: Array.concatMap _.requests rargs }
    Just info ->
      let
        pairs = Array.zip info.paramTypes args
        fnPairs = Array.mapMaybe functionArg pairs
        fnArgs = map _.name fnPairs
        valueArgs = Array.mapMaybe valueArg pairs
        rargs = map rewriteCalls valueArgs
        outF = f <> "$" <> joinWith "$" fnArgs
        fnSubst = Map.fromFoldable (Array.zip (arrowParamNames info) fnArgs)
        tySubst = Array.foldl addFnTypeSubst Map.empty fnPairs
        req =
          { srcName: f
          , fnSubst
          , tySubst
          , outName: outF
          }
      in
        { expr: ECall outF (map _.expr rargs)
        , requests: Array.concatMap _.requests rargs <> [ req ]
        }

  functionArg :: Tuple Ty Expr -> Maybe { name :: Name, ty :: Ty }
  functionArg (Tuple ty expr)
    | isArrow ty = case stripAt expr of
        EVar name -> Just { name, ty }
        _ -> Nothing
    | otherwise = Nothing

  valueArg :: Tuple Ty Expr -> Maybe Expr
  valueArg (Tuple ty expr) =
    if isArrow ty then Nothing else Just expr

  addFnTypeSubst subst { name, ty } = case fullArrowType name of
    Just actual -> matchTypes subst ty actual
    Nothing -> subst

  baseRequest name =
    { srcName: name
    , fnSubst: Map.empty
    , tySubst: Map.empty
    , outName: name
    }

  valueParamEntries info =
    Array.mapMaybe
      ( \(Tuple name ty) ->
          if isArrow ty then Nothing else Just { name, ty }
      )
      (Array.zip info.paramNames info.paramTypes)

  isHOF name = case Map.lookup name infos of
    Just info -> Array.any isArrow info.paramTypes
    Nothing -> false

  arrowParamNames info =
    Array.mapMaybe
      ( \(Tuple name ty) ->
          if isArrow ty then Just name else Nothing
      )
      (Array.zip info.paramNames info.paramTypes)

  fullArrowType name = case Map.lookup name infos of
    Just info | not (Array.null info.paramTypes) -> Just (foldr TArrow info.resultTy info.paramTypes)
    _ -> Nothing

  infoOf (Decl d) = case d.sig of
    Just sig ->
      let sp = splitArrow (Array.length d.params) sig
      in Just
        { name: d.name
        , paramNames: d.params
        , paramTypes: sp.params
        , resultTy: sp.result
        , body: d.body
        }
    Nothing -> Nothing

renameFns :: Map Name Name -> Expr -> Expr
renameFns subst = go subst
  where
  go s = case _ of
    EAt _ e -> go s e
    ELit lit -> ELit lit
    EVar n -> EVar (fromMaybe n (Map.lookup n s))
    EBin op a b -> EBin op (go s a) (go s b)
    ECmp op a b -> ECmp op (go s a) (go s b)
    EIf c t e -> EIf (go s c) (go s t) (go s e)
    ELet n e body -> ELet n (go s e) (go (Map.delete n s) body)
    ECall f args -> ECall (fromMaybe f (Map.lookup f s)) (map (go s) args)
    EBuiltin name args -> EBuiltin name (map (go s) args)
    EList xs -> EList (map (go s) xs)
    ERecord fields -> ERecord (map (\(Tuple n e) -> Tuple n (go s e)) fields)
    EField e name -> EField (go s e) name
    ESwitch scrut arms -> ESwitch (go s scrut) (map (\(Tuple pat body) -> Tuple pat (go s body)) arms)
    EMatch scrut arms -> EMatch (go s scrut) (map renameArm arms)
      where
      renameArm (Tuple pat body) = Tuple pat (go (removePattern pat s) body)

  removePattern = case _ of
    PWild -> identity
    PCtor _ names -> \s -> Array.foldl (flip Map.delete) s names

isArrow :: Ty -> Boolean
isArrow = case _ of
  TArrow _ _ -> true
  _ -> false

applySubst :: Map Name Ty -> Ty -> Ty
applySubst subst = case _ of
  TVar name -> fromMaybe (TVar name) (Map.lookup name subst)
  TData name args -> TData name (map (applySubst subst) args)
  TList t -> TList (applySubst subst t)
  TRecord fields -> TRecord (map (\(Tuple n t) -> Tuple n (applySubst subst t)) fields)
  TArrow a b -> TArrow (applySubst subst a) (applySubst subst b)
  other -> other

matchTypes :: Map Name Ty -> Ty -> Ty -> Map Name Ty
matchTypes subst pat actual = case pat, actual of
  TVar name, _ -> case Map.lookup name subst of
    Just _ -> subst
    Nothing -> Map.insert name actual subst
  TArrow a b, TArrow c d -> matchTypes (matchTypes subst a c) b d
  TList a, TList b -> matchTypes subst a b
  TData n as, TData m bs | n == m -> Array.foldl (\s (Tuple a b) -> matchTypes s a b) subst (Array.zip as bs)
  TRecord as, TRecord bs ->
    Array.foldl
      ( \s (Tuple n a) -> case lookupField n bs of
          Just b -> matchTypes s a b
          Nothing -> s
      )
      subst
      as
  _, _ -> subst

lookupField :: Name -> Array (Tuple Name Ty) -> Maybe Ty
lookupField name fields = map snd (Array.find (\(Tuple n _) -> n == name) fields)

entryName :: Array Decl -> Name
entryName decls = case Array.find (\d -> declName d == "main") decls of
  Just d -> declName d
  Nothing -> case Array.head decls of
    Just d -> declName d
    Nothing -> "main"

declName :: Decl -> Name
declName (Decl d) = d.name
