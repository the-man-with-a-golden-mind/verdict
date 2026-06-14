-- | Bidirectional-ish type checker. Top-level signatures are REQUIRED; parameter
-- | and result types come from the signature, everything inside is inferred and
-- | matched with `compatible` (which treats the internal `TUnknown` as a
-- | wildcard, e.g. for empty lists and FFI results).
module Verdict.Typecheck (TypeError(..), showTypeError, checkModule) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Verdict.Syntax.AST (BinOp, CmpOp(..), Ctor, Decl(..), Expr(..), Lit(..), Module(..), Name, Pattern(..), SourcePos, Ty(..), TypeDecl(..), splitArrow, stripAt, typeArity)

data TypeError
  = Located SourcePos TypeError
  | MissingSignature Name
  | SigArityMismatch Name Int Int
  | UnknownName Name
  | FunctionAsValue Name
  | SpawnRequiresFunction
  | NotAFunction Name
  | CallArityMismatch Name Int Int
  | Mismatch String Ty Ty
  | NotNumeric String Ty
  | NoField Name Ty
  | NotARecord Ty
  | SwitchNoDefault
  | UnknownType Name
  | UnknownConstructor Name
  | ConstructorArityMismatch Name Int Int
  | MatchNonData Ty
  | MatchWrongConstructor Name Name
  | MatchNonExhaustive Name
  | DataArityMismatch Name Int Int

showTypeError :: TypeError -> String
showTypeError = case _ of
  Located p e -> show p.line <> ":" <> show p.column <> ": " <> showTypeError e
  MissingSignature n -> "missing required type signature for '" <> n <> "'"
  SigArityMismatch n want got ->
    "signature for '" <> n <> "' declares " <> show want
      <> " parameter(s) but the definition binds " <> show got
  UnknownName n -> "unknown name '" <> n <> "'"
  FunctionAsValue n -> "'" <> n <> "' is a function and cannot be used as a value (no higher-order functions)"
  SpawnRequiresFunction -> "spawn expects a bare top-level function name as its first argument"
  NotAFunction n -> "'" <> n <> "' is not a function and cannot be called"
  CallArityMismatch n want got ->
    "'" <> n <> "' expects " <> show want <> " argument(s) but got " <> show got
  Mismatch ctx want got ->
    "type mismatch in " <> ctx <> ": expected " <> show want <> ", got " <> show got
  NotNumeric ctx t -> "operator " <> ctx <> " needs a number (Int, Fixed, or Rational), got " <> show t
  NoField f t -> "record has no field '" <> f <> "' (" <> show t <> ")"
  NotARecord t -> "field access on a non-record value of type " <> show t
  SwitchNoDefault -> "switch is missing a required `_` (default) arm"
  UnknownType n -> "unknown type '" <> n <> "'"
  UnknownConstructor n -> "unknown constructor '" <> n <> "'"
  ConstructorArityMismatch n want got ->
    "constructor '" <> n <> "' expects " <> show want <> " argument(s) but got " <> show got
  MatchNonData t -> "match scrutinee must be a sum type, got " <> show t
  MatchWrongConstructor ctor ty -> "constructor '" <> ctor <> "' does not belong to type '" <> ty <> "'"
  MatchNonExhaustive n -> "match on '" <> n <> "' is not exhaustive"
  DataArityMismatch n want got ->
    "type '" <> n <> "' expects " <> show want <> " type argument(s) but got " <> show got

relocate :: SourcePos -> TypeError -> TypeError
relocate pos err = case err of
  Located _ _ -> err
  _ -> Located pos err

atExpr :: Expr -> TypeError -> TypeError
atExpr expr err = case expr of
  EAt pos _ -> relocate pos err
  _ -> err

type Scheme = { params :: Array Ty, ret :: Ty }
type Globals = Map Name Scheme
type DataInfo = { name :: Name, params :: Array Name, ctors :: Array Ctor }
type CtorInfo = { parent :: Name, params :: Array Name, fields :: Array Ty }
type TypeEnv =
  { globals :: Globals
  , dataTypes :: Map Name DataInfo
  , ctors :: Map Name CtorInfo
  }
type Locals = Map Name Ty

--------------------------------------------------------------------------------

isNumeric :: Ty -> Boolean
isNumeric = case _ of
  TInt -> true
  TFixed -> true
  TRational -> true
  TUnknown -> true
  _ -> false

-- | Structural compatibility with `TUnknown` as a wildcard. Type variables are
-- | RIGID (only equal to the same variable) — they are NOT wildcards; call-site
-- | instantiation (`matchTy`) is what resolves them to concrete types.
compatible :: Ty -> Ty -> Boolean
compatible TUnknown _ = true
compatible _ TUnknown = true
compatible (TVar a) (TVar b) = a == b
compatible (TList a) (TList b) = compatible a b
compatible (TArrow a b) (TArrow c d) = compatible a c && compatible b d
compatible (TData a as) (TData b bs) =
  a == b && Array.length as == Array.length bs
    && Array.all (\(Tuple x y) -> compatible x y) (Array.zip as bs)
compatible (TRecord fa) (TRecord fb) =
  Array.length fa == Array.length fb
    && Array.all (\(Tuple n t) -> case lookupField n fb of
          Just t' -> compatible t t'
          Nothing -> false) fa
compatible a b = a == b

--------------------------------------------------------------------------------
-- Parametric polymorphism: instantiate a callee's polymorphic signature at each
-- call site by MATCHING its parameter types (which may contain type variables)
-- against the inferred argument types, building a substitution, then applying it
-- to the result type. For monomorphic signatures this degenerates to a plain
-- compatibility check.
--------------------------------------------------------------------------------

type Subst = Map Name Ty

applySubst :: Subst -> Ty -> Ty
applySubst s = case _ of
  TVar a -> fromMaybe (TVar a) (Map.lookup a s)
  TList t -> TList (applySubst s t)
  TArrow a b -> TArrow (applySubst s a) (applySubst s b)
  TRecord fs -> TRecord (map (\(Tuple n t) -> Tuple n (applySubst s t)) fs)
  TData n as -> TData n (map (applySubst s) as)
  other -> other

-- | Match a scheme type `pat` (may contain TVars) against an actual type `act`,
-- | extending the substitution. TUnknown is a wildcard on either side; a bound
-- | variable must stay compatible with its earlier binding.
matchTy :: String -> Subst -> Ty -> Ty -> Either TypeError Subst
matchTy ctx s pat act = case pat, act of
  TUnknown, _ -> Right s
  _, TUnknown -> Right s
  TVar a, _ -> case Map.lookup a s of
    Just b -> if compatible b act then Right s else Left (Mismatch ctx b act)
    Nothing -> Right (Map.insert a act s)
  TList p, TList q -> matchTy ctx s p q
  TArrow p1 p2, TArrow q1 q2 -> do
    s1 <- matchTy ctx s p1 q1
    matchTy ctx s1 p2 q2
  TData n ps, TData m qs
    | n == m && Array.length ps == Array.length qs ->
        Array.foldM (\acc (Tuple p q) -> matchTy ctx acc p q) s (Array.zip ps qs)
  TRecord pf, TRecord qf ->
    Array.foldM
      ( \acc (Tuple n p) -> case lookupField n qf of
          Just q -> matchTy ctx acc p q
          Nothing -> Left (Mismatch ctx pat act)
      )
      s
      pf
  _, _ -> if compatible pat act then Right s else Left (Mismatch ctx pat act)

-- | Check an argument list against parameter types that may be polymorphic, and
-- | return the substitution that instantiates the type variables.
instantiate :: TypeEnv -> Locals -> String -> Array Ty -> Array Expr -> Either TypeError Subst
instantiate env locals ctx params args = do
  argTys <- traverse (infer env locals) args
  Array.foldM (\s (Tuple p a) -> matchTy ctx s p a) Map.empty (Array.zip params argTys)

globalArrowType :: Globals -> Name -> Maybe Ty
globalArrowType globals name = case Map.lookup name globals of
  Just sch | not (Array.null sch.params) -> Just (foldr TArrow sch.ret sch.params)
  _ -> Nothing

isArrow :: Ty -> Boolean
isArrow = case _ of
  TArrow _ _ -> true
  _ -> false

listElem :: Ty -> Maybe Ty
listElem = case _ of
  TList t -> Just t
  TUnknown -> Just TUnknown
  _ -> Nothing

inferIntrinsicCall :: TypeEnv -> Locals -> Name -> Array Expr -> Maybe (Either TypeError Ty)
inferIntrinsicCall env locals f args = case f of
  "actorStart" -> Just do
    case Array.uncons args of
      Nothing -> Left (CallArityMismatch "actorStart" 1 0)
      Just { head: fnRef, tail: fnArgs } -> case stripAt fnRef of
        EVar fnName -> case Map.lookup fnName env.globals of
          Nothing -> Left SpawnRequiresFunction
          Just sch -> do
            let want = Array.length sch.params
            when (want /= Array.length fnArgs) (Left (CallArityMismatch fnName want (Array.length fnArgs)))
            _ <- instantiateCall env locals fnName sch.params fnArgs
            Right (TData "ActorRef" [ TUnknown ])
        _ -> Left SpawnRequiresFunction
  "actorSelf" -> Just do
    case args of
      [ u ] -> do
        tu <- infer env locals u
        when (not (compatible TUnit tu)) (Left (Mismatch "actorSelf argument" TUnit tu))
        Right (TData "ActorRef" [ TUnknown ])
      _ -> Left (CallArityMismatch "actorSelf" 1 (Array.length args))
  "actorReceive" -> Just do
    case args of
      [ u ] -> do
        tu <- infer env locals u
        when (not (compatible TUnit tu)) (Left (Mismatch "actorReceive argument" TUnit tu))
        Right TUnknown
      _ -> Left (CallArityMismatch "actorReceive" 1 (Array.length args))
  "actorSend" -> Just do
    case args of
      [ _, _ ] -> do
        _ <- instantiateCall env locals "actorSend" [ TData "ActorRef" [ TVar "m" ], TVar "m" ] args
        Right TUnit
      _ -> Left (CallArityMismatch "actorSend" 2 (Array.length args))
  "actorReply" -> Just do
    case args of
      [ _, _ ] -> do
        _ <- instantiateCall env locals "actorReply" [ TData "ActorRef" [ TVar "m" ], TVar "m" ] args
        Right TUnit
      _ -> Left (CallArityMismatch "actorReply" 2 (Array.length args))
  "actorCall" -> Just do
    case args of
      [ _, _ ] -> do
        _ <- instantiateCall env locals "actorCall" [ TData "ActorRef" [ TVar "m" ], TArrow TPid (TVar "m") ] args
        Right TUnknown
      _ -> Left (CallArityMismatch "actorCall" 2 (Array.length args))
  "spawn" -> Just do
    case Array.uncons args of
      Nothing -> Left (CallArityMismatch "spawn" 1 0)
      Just { head: fnRef, tail: fnArgs } -> case stripAt fnRef of
        EVar fnName -> case Map.lookup fnName env.globals of
          Nothing -> Left SpawnRequiresFunction
          Just sch -> do
            let want = Array.length sch.params
            when (want /= Array.length fnArgs) (Left (CallArityMismatch fnName want (Array.length fnArgs)))
            _ <- instantiateCall env locals fnName sch.params fnArgs
            Right TPid
        _ -> Left SpawnRequiresFunction
  "send" -> Just do
    case args of
      [ pid, msg ] -> do
        tpid <- infer env locals pid
        _ <- infer env locals msg
        when (not (compatible TPid tpid)) (Left (Mismatch "send pid" TPid tpid))
        Right TUnit
      _ -> Left (CallArityMismatch "send" 2 (Array.length args))
  "recv" -> Just do
    case args of
      [] -> Right TUnknown
      _ -> Left (CallArityMismatch "recv" 0 (Array.length args))
  "yield" -> Just do
    case args of
      [] -> Right TUnit
      _ -> Left (CallArityMismatch "yield" 0 (Array.length args))
  "self" -> Just do
    case args of
      [] -> Right TPid
      _ -> Left (CallArityMismatch "self" 0 (Array.length args))
  "length" -> Just do
    case args of
      [ xs ] -> do
        txs <- infer env locals xs
        case listElem txs of
          Just _ -> Right TInt
          Nothing -> Left (Mismatch "length argument" (TList TUnknown) txs)
      _ -> Left (CallArityMismatch "length" 1 (Array.length args))
  "get" -> Just do
    case args of
      [ xs, ix ] -> do
        txs <- infer env locals xs
        tix <- infer env locals ix
        when (not (compatible TInt tix)) (Left (Mismatch "get index" TInt tix))
        case listElem txs of
          Just elemTy -> Right elemTy
          Nothing -> Left (Mismatch "get argument" (TList TUnknown) txs)
      _ -> Left (CallArityMismatch "get" 2 (Array.length args))
  "append" -> Just do
    case args of
      [ xs, x ] -> do
        txs <- infer env locals xs
        tx <- infer env locals x
        case listElem txs of
          Just elemTy -> do
            when (not (compatible elemTy tx)) (Left (Mismatch "append element" elemTy tx))
            Right (TList (joinTy elemTy tx))
          Nothing -> Left (Mismatch "append argument" (TList TUnknown) txs)
      _ -> Left (CallArityMismatch "append" 2 (Array.length args))
  "mod" -> Just do
    case args of
      [ a, b ] -> do
        ta <- infer env locals a
        tb <- infer env locals b
        when (not (compatible TInt ta)) (Left (Mismatch "mod argument" TInt ta))
        when (not (compatible TInt tb)) (Left (Mismatch "mod argument" TInt tb))
        Right TInt
      _ -> Left (CallArityMismatch "mod" 2 (Array.length args))
  _ -> Nothing

callLocalArrow :: TypeEnv -> Locals -> Name -> Ty -> Array Expr -> Either TypeError Ty
callLocalArrow env locals f ty args = do
  let want = typeArity ty
  when (want /= Array.length args) (Left (CallArityMismatch f want (Array.length args)))
  let sp = splitArrow (Array.length args) ty
  subst <- instantiate env locals ("argument to " <> f) sp.params args
  Right (applySubst subst sp.result)

instantiateCall :: TypeEnv -> Locals -> String -> Array Ty -> Array Expr -> Either TypeError Subst
instantiateCall env locals ctx params args =
  Array.foldM step Map.empty (Array.zip params args)
  where
  step subst (Tuple param arg)
    | isArrow param = case functionRefType arg of
        Just ty -> matchTy ctx subst param ty
        Nothing -> Left (FunctionAsValue ctx)
    | otherwise = do
        argTy <- infer env locals arg
        matchTy ctx subst param argTy

  functionRefType arg = case stripAt arg of
    EVar name -> case Map.lookup name locals of
      Just ty | isArrow ty -> Just ty
      _ -> globalArrowType env.globals name
    _ -> Nothing

lookupField :: Name -> Array (Tuple Name Ty) -> Maybe Ty
lookupField n fs = map snd (Array.find (\f -> fst f == n) fs)

-- | The more concrete of two compatible types (resolves a `TUnknown` branch).
joinTy :: Ty -> Ty -> Ty
joinTy TUnknown b = b
joinTy a _ = a

--------------------------------------------------------------------------------

infer :: TypeEnv -> Locals -> Expr -> Either TypeError Ty
infer env locals = case _ of
  EAt pos e -> case infer env locals e of
    Left err -> Left (relocate pos err)
    Right t -> Right t

  ELit (LInt _) -> Right TInt
  ELit (LFixed _ _) -> Right TFixed
  ELit (LRational _ _) -> Right TRational
  ELit LUnit -> Right TUnit
  ELit (LBool _) -> Right TBool
  ELit (LStr _) -> Right TString

  EVar n -> case Map.lookup n locals of
    Just t | isArrow t -> Left (FunctionAsValue n)
    Just t -> Right t
    Nothing -> case Map.lookup n env.globals of
      Just sch
        | Array.null sch.params -> Right sch.ret
        | otherwise -> Left (FunctionAsValue n)
      Nothing -> case Map.lookup n env.ctors of
        Just ctor
          | Array.null ctor.fields ->
              -- nullary constructor: type args are unconstrained here
              Right (TData ctor.parent (map (const TUnknown) ctor.params))
          | otherwise -> Left (FunctionAsValue n)
        Nothing -> Left (UnknownName n)

  EBin op a b -> do
    ta <- infer env locals a
    tb <- infer env locals b
    when (not (isNumeric ta)) (Left (atExpr a (NotNumeric (showBin op) ta)))
    when (not (isNumeric tb)) (Left (atExpr b (NotNumeric (showBin op) tb)))
    when (not (compatible ta tb)) (Left (atExpr b (Mismatch ("operator " <> showBin op) ta tb)))
    Right (joinTy ta tb)

  ECmp op a b -> do
    ta <- infer env locals a
    tb <- infer env locals b
    case op of
      CmpEq -> when (not (compatible ta tb)) (Left (atExpr b (Mismatch "==" ta tb)))
      _ -> do
        when (not (isNumeric ta)) (Left (atExpr a (NotNumeric (showCmp op) ta)))
        when (not (isNumeric tb)) (Left (atExpr b (NotNumeric (showCmp op) tb)))
        when (not (compatible ta tb)) (Left (atExpr b (Mismatch (showCmp op) ta tb)))
    Right TBool

  EIf c t e -> do
    tc <- infer env locals c
    when (not (compatible tc TBool)) (Left (atExpr c (Mismatch "if condition" TBool tc)))
    tt <- infer env locals t
    te <- infer env locals e
    when (not (compatible tt te)) (Left (atExpr e (Mismatch "if branches" tt te)))
    Right (joinTy tt te)

  ELet n e body -> do
    te <- infer env locals e
    infer env (Map.insert n te locals) body

  ECall f args -> case inferIntrinsicCall env locals f args of
    Just result -> result
    Nothing -> case Map.lookup f locals of
      Just ty | isArrow ty -> callLocalArrow env locals f ty args
      Just _ -> Left (NotAFunction f)
      Nothing -> case Map.lookup f env.globals of
        Nothing -> case Map.lookup f env.ctors of
          Just ctor -> do
            let want = Array.length ctor.fields
            when (want /= Array.length args) (Left (ConstructorArityMismatch f want (Array.length args)))
            -- instantiate the constructor's type params from the argument types
            subst <- instantiate env locals ("argument to " <> f) ctor.fields args
            Right (TData ctor.parent (map (\p -> fromMaybe TUnknown (Map.lookup p subst)) ctor.params))
          Nothing -> Left (UnknownName f)
        Just sch -> do
          let want = Array.length sch.params
          when (want /= Array.length args) (Left (CallArityMismatch f want (Array.length args)))
          subst <- instantiateCall env locals f sch.params args
          Right (applySubst subst sch.ret)

  EBuiltin _ args -> do
    traverse_ (infer env locals) args
    Right TUnknown

  EList xs -> do
    ts <- traverse (infer env locals) xs
    case Array.uncons ts of
      Nothing -> Right (TList TUnknown)
      Just { head, tail } -> do
        elemTy <- Array.foldM unifyElem head tail
        Right (TList elemTy)
    where
    unifyElem acc t =
      if compatible acc t then Right (joinTy acc t)
      else Left (Mismatch "list element" acc t)

  ERecord fields -> do
    typed <- traverse (\(Tuple n e) -> map (Tuple n) (infer env locals e)) fields
    Right (TRecord typed)

  EField e fname -> do
    te <- infer env locals e
    case te of
      TRecord fs -> case lookupField fname fs of
        Just t -> Right t
        Nothing -> Left (atExpr e (NoField fname te))
      TUnknown -> Right TUnknown
      _ -> Left (atExpr e (NotARecord te))

  ESwitch scrut arms -> do
    ts <- infer env locals scrut
    when (not (Array.any (isNothing <<< fst) arms)) (Left SwitchNoDefault)
    -- each literal arm must be comparable with the scrutinee
    traverse_
      ( \(Tuple mp _) -> case mp of
          Just lit -> let lt = litTy lit in when (not (compatible ts lt)) (Left (Mismatch "switch case" ts lt))
          Nothing -> Right unit
      )
      arms
    btys <- traverse (\(Tuple _ b) -> infer env locals b) arms
    case Array.uncons btys of
      Nothing -> Left SwitchNoDefault
      Just { head, tail } ->
        Array.foldM
          (\acc t -> if compatible acc t then Right (joinTy acc t) else Left (Mismatch "switch branches" acc t))
          head
          tail

  EMatch scrut arms -> do
    ts <- infer env locals scrut
    case ts of
      TData tyName typeArgs -> case Map.lookup tyName env.dataTypes of
        Nothing -> Left (UnknownType tyName)
        Just info -> do
          when (not (isExhaustive info.ctors arms)) (Left (MatchNonExhaustive tyName))
          -- bind the data type's params to the scrutinee's type arguments, so a
          -- pattern variable gets the instantiated payload type (Some n : Int
          -- when matching an Option Int).
          let dsub = Map.fromFoldable
                ( Array.zip info.params
                    (typeArgs <> Array.replicate (Array.length info.params) TUnknown)
                )
          btys <- traverse (inferMatchArm tyName dsub) arms
          case Array.uncons btys of
            Nothing -> Left (MatchNonExhaustive tyName)
            Just { head, tail } ->
              Array.foldM
                (\acc t -> if compatible acc t then Right (joinTy acc t) else Left (Mismatch "match branches" acc t))
                head
                tail
      TUnknown -> Right TUnknown
      _ -> Left (MatchNonData ts)
    where
    inferMatchArm tyName dsub (Tuple pat body) = case pat of
      PWild -> infer env locals body
      PCtor ctorName vars -> case Map.lookup ctorName env.ctors of
        Nothing -> Left (UnknownConstructor ctorName)
        Just ctor -> do
          when (ctor.parent /= tyName) (Left (MatchWrongConstructor ctorName tyName))
          when (Array.length ctor.fields /= Array.length vars)
            (Left (ConstructorArityMismatch ctorName (Array.length ctor.fields) (Array.length vars)))
          let fieldTys = map (applySubst dsub) ctor.fields
          let locals' = Map.union (Map.fromFoldable (Array.zip vars fieldTys)) locals
          infer env locals' body

    isExhaustive ctors arms =
      Array.any (\(Tuple p _) -> p == PWild) arms
        || Set.fromFoldable (map _.name ctors)
          == Set.fromFoldable (Array.mapMaybe ctorPatName arms)

    ctorPatName (Tuple (PCtor n _) _) = Just n
    ctorPatName _ = Nothing

litTy :: Lit -> Ty
litTy = case _ of
  LInt _ -> TInt
  LFixed _ _ -> TFixed
  LRational _ _ -> TRational
  LUnit -> TUnit
  LBool _ -> TBool
  LStr _ -> TString

showBin :: BinOp -> String
showBin = show

showCmp :: CmpOp -> String
showCmp = show

--------------------------------------------------------------------------------

schemeOf :: Decl -> Either TypeError Scheme
schemeOf (Decl d) = case d.sig of
  Nothing -> Left (MissingSignature d.name)
  Just t ->
    let nparams = Array.length d.params
    in
      if typeArity t < nparams then Left (SigArityMismatch d.name (typeArity t) nparams)
      else
        let sp = splitArrow nparams t
        in Right { params: sp.params, ret: sp.result }

validateTy :: Map Name DataInfo -> Ty -> Either TypeError Unit
validateTy dataTypes = case _ of
  TData n args -> case Map.lookup n dataTypes of
    Nothing -> Left (UnknownType n)
    Just info -> do
      when (Array.length args /= Array.length info.params)
        (Left (DataArityMismatch n (Array.length info.params) (Array.length args)))
      traverse_ (validateTy dataTypes) args
  TVar _ -> Right unit
  TList t -> validateTy dataTypes t
  TRecord fs -> traverse_ (validateTy dataTypes <<< snd) fs
  TArrow a b -> validateTy dataTypes a *> validateTy dataTypes b
  _ -> Right unit

checkDecl :: TypeEnv -> Decl -> Either TypeError Unit
checkDecl env decl@(Decl d) = do
  sch <- schemeOf decl
  traverse_ (validateTy env.dataTypes) sch.params
  validateTy env.dataTypes sch.ret
  let locals = Map.fromFoldable (Array.zip d.params sch.params)
  bodyTy <- infer env locals d.body
  when (not (compatible sch.ret bodyTy))
    (Left (Mismatch ("body of " <> d.name) sch.ret bodyTy))

checkModule :: Module -> Either TypeError Unit
checkModule (Module _ typeDecls decls) = do
  let dataTypes = buildDataTypes typeDecls
  let ctors = buildCtors typeDecls
  traverse_ (validateTypeDecl dataTypes) typeDecls
  globals <- buildGlobals
  let env = { globals, dataTypes, ctors }
  traverse_ (checkDecl env) decls
  where
  buildDataTypes tys = Map.fromFoldable
    (map (\(TypeDecl n ps cs) -> Tuple n { name: n, params: ps, ctors: cs }) tys)

  buildCtors tys = Map.fromFoldable
    (Array.concatMap ctorEntries tys)

  ctorEntries (TypeDecl parent ps ctors) =
    map (\c -> Tuple c.name { parent, params: ps, fields: c.fields }) ctors

  validateTypeDecl dataTypes (TypeDecl _ _ ctors) =
    traverse_ (traverse_ (validateTy dataTypes) <<< _.fields) ctors

  buildGlobals = map Map.fromFoldable
    (traverse (\decl@(Decl d) -> map (Tuple d.name) (schemeOf decl)) decls)
