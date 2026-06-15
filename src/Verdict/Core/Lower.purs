-- | Lowering: AST -> MIR. Each subexpression gets a fresh virtual register
-- | (the allocator coalesces them later). Calls lower to a static `MCall`;
-- | the entry point's result flows to `MHalt`, every other body to `MRet`.
module Verdict.Core.Lower (lowerModule) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (State, evalState, get, modify_)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Verdict.Core.MIR (MFunc, MInstr(..), VReg, Label)
import Verdict.Syntax.AST (BinOp(..), CmpOp(..), Ctor, Decl(..), Expr(..), Lit(..), Module(..), Name, Pattern(..), Ty(..), TypeDecl(..), splitArrow, stripAt)

type LowerState =
  { nextReg :: VReg
  , nextLabel :: Int
  , nextEffect :: Int
  , currentFunc :: Name
  , instrs :: Array MInstr
  }

type Env = Map Name VReg
type CtorMap = Map Name Ctor

type L a = State LowerState a

freshReg :: L VReg
freshReg = do
  s <- get
  modify_ _ { nextReg = s.nextReg + 1 }
  pure s.nextReg

freshLabel :: String -> L Label
freshLabel pfx = do
  s <- get
  modify_ _ { nextLabel = s.nextLabel + 1 }
  pure (pfx <> "_" <> show s.nextLabel)

emit :: MInstr -> L Unit
emit i = modify_ \s -> s { instrs = Array.snoc s.instrs i }

freshEffectId :: L Int
freshEffectId = do
  s <- get
  modify_ _ { nextEffect = s.nextEffect + 1 }
  pure s.nextEffect

--------------------------------------------------------------------------------
-- Expressions (value position): returns the vreg holding the result.
--------------------------------------------------------------------------------

lowerExpr :: CtorMap -> Env -> Expr -> L VReg
lowerExpr ctors env = case _ of
  EAt _ e -> lowerExpr ctors env e

  ELit lit -> do
    r <- freshReg
    emit (MLoad r lit)
    pure r

  EVar n -> case Map.lookup n env of
    Just r -> pure r
    Nothing -> case Map.lookup n ctors of
      Just ctor | Array.null ctor.fields -> lowerCtor n []
      _ -> do
        -- nullary reference to a top-level value
        r <- freshReg
        emit (MCall r n [])
        pure r

  EBin op a b -> do
    ra <- lowerExpr ctors env a
    rb <- lowerExpr ctors env b
    r <- freshReg
    emit (MBin op r ra rb)
    pure r

  ECmp op a b -> do
    ra <- lowerExpr ctors env a
    rb <- lowerExpr ctors env b
    r <- freshReg
    emit (MCmp op r ra rb)
    pure r

  EIf c t e -> do
    rc <- lowerExpr ctors env c
    dst <- freshReg
    elseL <- freshLabel "else"
    endL <- freshLabel "end"
    emit (MJumpIfFalse rc elseL)
    rt <- lowerExpr ctors env t
    emit (MMove dst rt)
    emit (MJump endL)
    emit (MLabel elseL)
    re <- lowerExpr ctors env e
    emit (MMove dst re)
    emit (MLabel endL)
    pure dst

  ELet n e body -> do
    re <- lowerExpr ctors env e
    lowerExpr ctors (Map.insert n re env) body

  ECall "spawn" args -> case Array.uncons args of
    Just { head: fnRef, tail: rest } -> case stripAt fnRef of
      EVar fnName -> do
        rs <- traverse (lowerExpr ctors env) rest
        r <- freshReg
        emit (MSpawn r fnName rs)
        pure r
      _ -> do
        r <- freshReg
        emit (MSpawn r "__invalid_spawn__" [])
        pure r
    Nothing -> do
      r <- freshReg
      emit (MSpawn r "__invalid_spawn__" [])
      pure r

  ECall "actorStart" args -> case Array.uncons args of
    Just { head: fnRef, tail: rest } -> case stripAt fnRef of
      EVar fnName -> do
        rs <- traverse (lowerExpr ctors env) rest
        pid <- freshReg
        emit (MSpawn pid fnName rs)
        lowerCtor "MkActorRef" [ pid ]
      _ -> do
        pid <- freshReg
        emit (MSpawn pid "__invalid_spawn__" [])
        lowerCtor "MkActorRef" [ pid ]
    Nothing -> do
      pid <- freshReg
      emit (MSpawn pid "__invalid_spawn__" [])
      lowerCtor "MkActorRef" [ pid ]

  ECall "send" [ pid, msg ] -> do
    rp <- lowerExpr ctors env pid
    rm <- lowerExpr ctors env msg
    emit (MSend rp rm)
    r <- freshReg
    emit (MLoad r LUnit)
    pure r

  ECall "recv" [] -> do
    r <- freshReg
    emit (MRecv r)
    pure r

  ECall "yield" [] -> do
    emit MYield
    r <- freshReg
    emit (MLoad r LUnit)
    pure r

  ECall "self" [] -> do
    r <- freshReg
    emit (MSelf r)
    pure r

  ECall "length" [ xs ] -> do
    rxs <- lowerExpr ctors env xs
    r <- freshReg
    emit (MListLength r rxs)
    pure r

  ECall "get" [ xs, ix ] -> do
    rxs <- lowerExpr ctors env xs
    rix <- lowerExpr ctors env ix
    r <- freshReg
    emit (MListGet r rxs rix)
    pure r

  ECall "append" [ xs, x ] -> do
    rxs <- lowerExpr ctors env xs
    rx <- lowerExpr ctors env x
    r <- freshReg
    emit (MListAppend r rxs rx)
    pure r

  ECall "mod" [ a, b ] -> do
    ra <- lowerExpr ctors env a
    rb <- lowerExpr ctors env b
    r <- freshReg
    emit (MBin OpMod r ra rb)
    pure r

  ECall f args -> do
    rs <- traverse (lowerExpr ctors env) args
    case Map.lookup f ctors of
      Just _ -> lowerCtor f rs
      Nothing -> do
        r <- freshReg
        emit (MCall r f rs)
        pure r

  EBuiltin name args -> do
    rs <- traverse (lowerExpr ctors env) args
    if isEffectfulBuiltin name then lowerEffectBuiltin name rs
    else do
      r <- freshReg
      emit (MBuiltin r name rs)
      pure r

  -- `effect(...)` always lowers to the async effect protocol, for ANY namespace.
  EEffect name args -> do
    rs <- traverse (lowerExpr ctors env) args
    lowerEffectBuiltin name rs

  EList xs -> do
    rs <- traverse (lowerExpr ctors env) xs
    r <- freshReg
    emit (MListNew r)
    traverse_ (\v -> emit (MListAppend r r v)) rs
    pure r

  ERecord fields -> do
    r <- freshReg
    emit (MRecordNew r)
    traverse_
      ( \(Tuple fname fe) -> do
          rv <- lowerExpr ctors env fe
          emit (MRecordSet r r fname rv)
      )
      fields
    pure r

  EField e fname -> do
    re <- lowerExpr ctors env e
    r <- freshReg
    emit (MRecordGet r re fname)
    pure r

  ESwitch scrut arms -> do
    rs <- lowerExpr ctors env scrut
    dst <- freshReg
    endL <- freshLabel "swend"
    traverse_ (lowerSwitchArm ctors env rs dst endL) arms
    emit (MLabel endL)
    pure dst

  EMatch scrut arms -> do
    rs <- lowerExpr ctors env scrut
    dst <- freshReg
    tag <- freshReg
    endL <- freshLabel "matchend"
    emit (MRecordGet tag rs "$tag")
    traverse_ (lowerMatchArm ctors env rs tag dst endL) arms
    emit (MLabel endL)
    pure dst
  where
  lowerCtor name args = do
    r <- freshReg
    tag <- freshReg
    emit (MRecordNew r)
    emit (MLoad tag (LStr name))
    emit (MRecordSet r r "$tag" tag)
    traverse_ (\(Tuple ix arg) -> emit (MRecordSet r r ("$" <> show ix) arg)) (Array.mapWithIndex Tuple args)
    pure r

-- | Lower an effectful builtin to FinVM 1.1.0's ASYNC effect protocol. The
-- | requesting process suspends on the intent's correlation `key` (EFFECT_AWAIT)
-- | while OTHER processes keep running; the host driver fulfils the effect and
-- | delivers the result to the mailbox as `VVariant "EffectReply" { key, value }`,
-- | which we read with PROC_RECEIVE, unwrap (VARIANT_PAYLOAD), and project
-- | (`value`). This composes with the actor framework: an actor awaiting I/O
-- | yields to its siblings instead of blocking the whole VM.
-- |
-- | The key is unique per effect call SITE (`__effect.result.<fn>.<n>`). Effects
-- | inside loops/recursion reuse the site key across iterations (a known limit);
-- | and because PROC_RECEIVE is FIFO, an actor that has other messages queued
-- | when it awaits will dequeue those first — fine for linear request/reply,
-- | selective receive is a future refinement.
lowerEffectBuiltin :: String -> Array VReg -> L VReg
lowerEffectBuiltin bid args = do
  effectId <- freshEffectId
  s <- get
  let key = effectResultKey s.currentFunc effectId
  payload <- effectPayload key bid args
  intent <- freshReg
  emit (MEffectNew intent (effectType bid) payload)
  emit (MEffectAwait intent)
  reply <- freshReg
  emit (MRecv reply)
  replyRec <- freshReg
  emit (MVariantPayload replyRec reply)
  r <- freshReg
  emit (MRecordGet r replyRec "value")
  pure r

effectResultKey :: Name -> Int -> String
effectResultKey fn ix = "__effect.result." <> fn <> "." <> show ix

effectType :: String -> String
effectType = SCU.takeWhile (_ /= '@')

isEffectfulBuiltin :: String -> Boolean
isEffectfulBuiltin bid =
  case namespace bid of
    "http" -> true
    "db" -> true
    "cache" -> true
    "sys" -> true
    "ws" -> true
    "time" -> true
    "random" -> true
    _ -> false

namespace :: String -> String
namespace = SCU.takeWhile (_ /= '.')

effectPayload :: String -> String -> Array VReg -> L VReg
effectPayload key bid args = do
  keyReg <- freshReg
  emit (MLoad keyReg (LStr key))
  case payloadFields bid args of
    Just fields -> recordPayload (Tuple "key" keyReg `Array.cons` fields)
    Nothing -> do
      argsReg <- argsPayload args
      recordPayload [ Tuple "key" keyReg, Tuple "args" argsReg ]

payloadFields :: String -> Array VReg -> Maybe (Array (Tuple String VReg))
payloadFields bid args = case bid, args of
  "http.get@1", [ url ] -> Just [ Tuple "url" url ]
  "http.post@1", [ url, body ] -> Just [ Tuple "url" url, Tuple "body" body ]
  "db.insert@1", [ table, record ] -> Just [ Tuple "table" table, Tuple "record" record ]
  "db.get@1", [ table, id ] -> Just [ Tuple "table" table, Tuple "id" id ]
  "db.update@1", [ table, id, record ] -> Just [ Tuple "table" table, Tuple "id" id, Tuple "record" record ]
  "db.delete@1", [ table, id ] -> Just [ Tuple "table" table, Tuple "id" id ]
  "db.query@1", [ table, query, options ] -> Just [ Tuple "table" table, Tuple "query" query, Tuple "options" options ]
  "db.createIndex@1", [ table, field ] -> Just [ Tuple "table" table, Tuple "field" field ]
  "db.hash@1", [ table ] -> Just [ Tuple "table" table ]
  "cache.set@1", [ ns, key, value ] -> Just [ Tuple "ns" ns, Tuple "cacheKey" key, Tuple "value" value ]
  "cache.get@1", [ ns, key ] -> Just [ Tuple "ns" ns, Tuple "cacheKey" key ]
  "cache.delete@1", [ ns, key ] -> Just [ Tuple "ns" ns, Tuple "cacheKey" key ]
  "sys.log@1", [ message ] -> Just [ Tuple "message" message ]
  "sys.cwd@1", [] -> Just []
  "sys.readText@1", [ path ] -> Just [ Tuple "path" path ]
  "sys.writeText@1", [ path, contents ] -> Just [ Tuple "path" path, Tuple "contents" contents ]
  "sys.env@1", [ name ] -> Just [ Tuple "name" name ]
  _, _ -> Nothing

argsPayload :: Array VReg -> L VReg
argsPayload args = case args of
  [] -> do
    r <- freshReg
    emit (MLoad r LUnit)
    pure r
  [ one ] -> pure one
  _ -> do
    r <- freshReg
    emit (MListNew r)
    traverse_ (\arg -> emit (MListAppend r r arg)) args
    pure r

recordPayload :: Array (Tuple String VReg) -> L VReg
recordPayload fields = do
  r <- freshReg
  emit (MRecordNew r)
  traverse_ (\(Tuple name value) -> emit (MRecordSet r r name value)) fields
  pure r

-- | One switch arm: a literal arm compares the scrutinee and skips on miss; the
-- | default arm always runs. Every arm writes the shared result register.
lowerSwitchArm :: CtorMap -> Env -> VReg -> VReg -> Label -> Tuple (Maybe Lit) Expr -> L Unit
lowerSwitchArm ctors env rs dst endL (Tuple pat body) = case pat of
  Just lit -> do
    rl <- freshReg
    emit (MLoad rl lit)
    rc <- freshReg
    emit (MCmp CmpEq rc rs rl)
    nextL <- freshLabel "swnext"
    emit (MJumpIfFalse rc nextL)
    rb <- lowerExpr ctors env body
    emit (MMove dst rb)
    emit (MJump endL)
    emit (MLabel nextL)
  Nothing -> do
    rb <- lowerExpr ctors env body
    emit (MMove dst rb)
    emit (MJump endL)

lowerMatchArm :: CtorMap -> Env -> VReg -> VReg -> VReg -> Label -> Tuple Pattern Expr -> L Unit
lowerMatchArm ctors env scrut tag dst endL (Tuple pat body) = case pat of
  PWild -> do
    rb <- lowerExpr ctors env body
    emit (MMove dst rb)
    emit (MJump endL)
  PCtor ctor vars -> do
    expected <- freshReg
    ok <- freshReg
    nextL <- freshLabel "matchnext"
    emit (MLoad expected (LStr ctor))
    emit (MCmp CmpEq ok tag expected)
    emit (MJumpIfFalse ok nextL)
    payloads <- traverse
      ( \(Tuple ix name) -> do
          r <- freshReg
          emit (MRecordGet r scrut ("$" <> show ix))
          pure (Tuple name r)
      )
      (Array.mapWithIndex Tuple vars)
    rb <- lowerExpr ctors (Map.union (Map.fromFoldable payloads) env) body
    emit (MMove dst rb)
    emit (MJump endL)
    emit (MLabel nextL)

--------------------------------------------------------------------------------
-- Declarations & module
--------------------------------------------------------------------------------

lowerDecl :: CtorMap -> Boolean -> Decl -> MFunc
lowerDecl ctors isEntry (Decl d) =
  let
    scheme = case d.sig of
      Just t -> splitArrow (Array.length d.params) t
      Nothing -> { params: map (const TUnknown) d.params, result: TUnknown }

    -- The entry point halts the VM; every other function returns to its caller.
    ret = if isEntry then MHalt else MRet

    build :: L MFunc
    build = do
      paramRegs <- traverse (const freshReg) d.params
      let env = Map.fromFoldable (Array.zip d.params paramRegs)
      r <- lowerExpr ctors env d.body
      emit (ret r)
      s <- get
      pure
        { name: d.name
        , params: paramRegs
        , paramTys: scheme.params
        , retTy: scheme.result
        , body: s.instrs
        , isEntry
        }
  in
    evalState build { nextReg: 0, nextLabel: 0, nextEffect: 0, currentFunc: d.name, instrs: [] }

lowerModule :: Module -> { funcs :: Array MFunc, entry :: Name }
lowerModule (Module _ typeDecls decls) =
  let
    ctors = ctorMap typeDecls
    entry = fromMaybe "main"
      ( map declNameOf (Array.find (\(Decl d) -> d.name == "main") decls)
          <|> map declNameOf (Array.head decls)
      )
    declNameOf (Decl d) = d.name
  in
    { funcs: map (\d -> lowerDecl ctors (declNameOf d == entry) d) decls
    , entry
    }

ctorMap :: Array TypeDecl -> CtorMap
ctorMap typeDecls =
  Map.fromFoldable (Array.concatMap entries typeDecls)
  where
  entries (TypeDecl _ _ ctors) = map (\c -> Tuple c.name c) ctors
