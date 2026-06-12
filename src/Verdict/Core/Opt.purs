-- | Optimization passes over MIR. The language being pure + strict makes these
-- | sound. Order: whole-program nullary inlining, then per-function constant
-- | fold/propagate + strength-reduce, CSE, and dead-code elimination to
-- | fixpoint. (Redundant MOVEs are dropped after register allocation, once we
-- | know which coalesce.)
module Verdict.Core.Opt (cse, inlineNullaries, optimize) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Verdict.Core.MIR (MFunc, MInstr(..), VReg, defOf, isPure, mapVRegs, regsOf, usesOf)
import Verdict.Eval.BigInt (addStr, cmpStr, modStr, mulStr, normalizeStr, scale10, subStr)
import Verdict.Eval.Rational as Rational
import Verdict.Syntax.AST (BinOp(..), CmpOp(..), Lit(..), Name)

optimize :: MFunc -> MFunc
optimize f =
  f { body = dropUnreachable (tailCalls (dce (cse (dropUnreachable (foldPass f.body))))) }

--------------------------------------------------------------------------------
-- Tail-call peephole. A call whose result becomes the function's return value
-- with no further computation is in tail position, so it can reuse the current
-- frame (FinVM `TAIL_CALL`). Recursion flows through `if`/`switch`, so the call
-- is followed not by an immediate `MRet` but by a move into the result register
-- which then reaches the function's return label by fall-through or a jump. We
-- recognize:
--   MCall d f as          where control from the next instruction returns d
--   MCall t f as ; MMove d t   where control from there returns d
-- "Control returns r" means: following only labels and unconditional jumps (no
-- computation), the next real instruction is `MRet r`. Entry functions end in
-- `MHalt`, never `MRet`, so their calls never match — correct, since a tail call
-- from the entry has no frame to return to. We leave the now-dead move/return in
-- place; the trailing dropUnreachable prunes them (the function then ends in the
-- `MTailCall` terminator). Runs after DCE so sequences are adjacent.
--------------------------------------------------------------------------------

tailCalls :: Array MInstr -> Array MInstr
tailCalls instrs = go 0
  where
  labelIx = labelIndexMap instrs

  -- Following only labels and unconditional jumps from index k (no intervening
  -- computation), which register does control return? Fuel-bounded vs jump cycles.
  returnsAt :: Int -> Int -> Maybe VReg
  returnsAt fuel k
    | fuel <= 0 = Nothing
    | otherwise = case Array.index instrs k of
        Just (MRet d) -> Just d
        Just (MLabel _) -> returnsAt (fuel - 1) (k + 1)
        Just (MJump l) -> case Map.lookup l labelIx of
          Just j -> returnsAt (fuel - 1) j
          Nothing -> Nothing
        _ -> Nothing

  returns k = returnsAt (Array.length instrs) k

  go :: Int -> Array MInstr
  go i = case Array.index instrs i of
    Nothing -> []
    Just instr -> case instr of
      MCall d fid args
        | returns (i + 1) == Just d ->
            Array.cons (MTailCall fid args) (go (i + 1))
      MCall d fid args ->
        case Array.index instrs (i + 1) of
          Just (MMove d2 t)
            | t == d, returns (i + 2) == Just d2 ->
                Array.cons (MTailCall fid args) (go (i + 1))
          _ -> Array.cons instr (go (i + 1))
      _ -> Array.cons instr (go (i + 1))

-- | Index of each label's `MLabel` instruction.
labelIndexMap :: Array MInstr -> Map String Int
labelIndexMap instrs = build 0 Map.empty
  where
  build i m = case Array.index instrs i of
    Nothing -> m
    Just (MLabel l) -> build (i + 1) (Map.insert l i m)
    Just _ -> build (i + 1) m

--------------------------------------------------------------------------------
-- Nullary function inlining. This is whole-program because a call can be in a
-- different function from the callee. Iteration lets nullaries that call other
-- nullaries collapse without trying to solve recursive cycles.
--------------------------------------------------------------------------------

inlineNullaries :: Array MFunc -> String -> Array MFunc
inlineNullaries funcs entry = go 5 funcs
  where
  go 0 fs = dropDead entry fs
  go n fs =
    let
      table = inlinableMap entry fs
      fs' = map (inlineFunc table) fs
    in
      go (n - 1) (dropDead entry fs')

inlinableMap :: String -> Array MFunc -> Map Name MFunc
inlinableMap entry funcs =
  Map.fromFoldable
    ( map (\f -> Tuple f.name f)
        (Array.filter (isInlinable entry) funcs)
    )

isInlinable :: String -> MFunc -> Boolean
isInlinable entry f =
  Array.null f.params
    && f.name /= entry
    && not (Array.any (callsName f.name) f.body)
    && hasTrailingRet f.body

callsName :: Name -> MInstr -> Boolean
callsName name = case _ of
  MCall _ callee _ -> callee == name
  _ -> false

hasTrailingRet :: Array MInstr -> Boolean
hasTrailingRet body = case trailingRet body of
  Just _ -> true
  Nothing -> false

trailingRet :: Array MInstr -> Maybe { init :: Array MInstr, ret :: VReg }
trailingRet body = case Array.unsnoc body of
  Just { init, last: MRet r } -> Just { init, ret: r }
  _ -> Nothing

type InlineState =
  { nextReg :: VReg
  , callIx :: Int
  , instrs :: Array MInstr
  }

inlineFunc :: Map Name MFunc -> MFunc -> MFunc
inlineFunc table f =
  f { body = final.instrs }
  where
  final = Array.foldl step
    { nextReg: maxVRegInBody f.body + 1, callIx: 0, instrs: [] }
    f.body

  step :: InlineState -> MInstr -> InlineState
  step st instr = case instr of
    MCall d name args | Array.null args ->
      case Map.lookup name table of
        Just callee -> case trailingRet callee.body of
          Just { init, ret } ->
            let
              base = st.nextReg
              suffix = "__inl" <> show st.callIx
              body' = map (freshenLabels suffix <<< mapVRegs (\r -> r + base)) init
              ret' = ret + base
              width = max 0 (maxVRegInBody callee.body + 1)
            in
              st
                { nextReg = base + width
                , callIx = st.callIx + 1
                , instrs = st.instrs <> body' <> [ MMove d ret' ]
                }
          Nothing -> keep st instr
        Nothing -> keep st instr
    _ -> keep st instr

  keep st instr = st { instrs = Array.snoc st.instrs instr }

freshenLabels :: String -> MInstr -> MInstr
freshenLabels suffix = case _ of
  MLabel lbl -> MLabel (rename lbl)
  MJump lbl -> MJump (rename lbl)
  MJumpIfFalse c lbl -> MJumpIfFalse c (rename lbl)
  other -> other
  where
  rename lbl = lbl <> suffix

maxVRegInBody :: Array MInstr -> VReg
maxVRegInBody body =
  Array.foldl max (-1) (Array.concatMap regsOf body)

dropDead :: String -> Array MFunc -> Array MFunc
dropDead entry funcs =
  let keep = reachableFrom entry funcs
  in Array.filter (\f -> Set.member f.name keep) funcs

reachableFrom :: String -> Array MFunc -> Set.Set Name
reachableFrom entry funcs = go Set.empty [ entry ]
  where
  funcMap = Map.fromFoldable (map (\f -> Tuple f.name f) funcs)

  go seen frontier = case Array.uncons frontier of
    Nothing -> seen
    Just { head, tail }
      | Set.member head seen -> go seen tail
      | otherwise ->
          let
            seen' = Set.insert head seen
            outs = case Map.lookup head funcMap of
              Just f -> callRefs f.body
              Nothing -> []
          in
            go seen' (tail <> outs)

callRefs :: Array MInstr -> Array Name
callRefs = Array.concatMap case _ of
  MCall _ name _ -> [ name ]
  MSpawn _ name _ -> [ name ]
  MTailCall name _ -> [ name ]
  _ -> []

--------------------------------------------------------------------------------
-- Constant folding / propagation / strength reduction (one forward pass).
-- The known-constant map is cleared at labels (join points) to stay sound
-- without a full dataflow analysis.
--------------------------------------------------------------------------------

foldPass :: Array MInstr -> Array MInstr
foldPass = go Map.empty
  where
  go :: Map VReg Lit -> Array MInstr -> Array MInstr
  go known instrs = case Array.uncons instrs of
    Nothing -> []
    Just { head, tail } ->
      case head of
        MLabel _ -> Array.cons head (go Map.empty tail)
        MJumpIfFalse c lbl -> case Map.lookup c known of
          Just (LBool true) -> go known tail
          Just (LBool false) -> Array.cons (MJump lbl) (go known tail)
          _ -> Array.cons head (go known tail)
        _ ->
          let r = rewrite known head
          in Array.cons r.instr (go (record known r) tail)

  record known { instr } = case defOf instr, instr of
    Just d, MLoad _ lit -> Map.insert d lit known
    Just d, _ -> Map.delete d known
    _, _ -> known

  rewrite :: Map VReg Lit -> MInstr -> { instr :: MInstr }
  rewrite known = case _ of
    MBin op d a b -> { instr: foldBin known op d a b }
    MCmp op d a b -> { instr: foldCmp known op d a b }
    other -> { instr: other }

intOf :: Map VReg Lit -> VReg -> Maybe String
intOf known r = case Map.lookup r known of
  Just (LInt s) -> Just s
  _ -> Nothing

fixedOf :: Map VReg Lit -> VReg -> Maybe { value :: String, scale :: Int }
fixedOf known r = case Map.lookup r known of
  Just (LFixed value scale) -> Just { value, scale }
  _ -> Nothing

ratOf :: Map VReg Lit -> VReg -> Maybe Rational.Rat
ratOf known r = case Map.lookup r known of
  Just (LRational n d) -> Just (Rational.reduce n d)
  _ -> Nothing

foldBin :: Map VReg Lit -> BinOp -> VReg -> VReg -> VReg -> MInstr
foldBin known op d a b =
  case intOf known a, intOf known b of
    -- Both constant: evaluate at compile time, removing the BigInt op entirely.
    Just x, Just y -> case op of
      OpAdd -> MLoad d (LInt (normalizeStr (addStr x y)))
      OpSub -> MLoad d (LInt (normalizeStr (subStr x y)))
      OpMul -> MLoad d (LInt (normalizeStr (mulStr x y)))
      OpDiv -> MBin op d a b -- division rounding left to the VM
      OpMod -> MLoad d (LInt (normalizeStr (modStr x y)))
    -- One constant: algebraic strength reduction.
    Just x, Nothing -> case op, normalizeStr x of
      OpAdd, "0" -> MMove d b
      OpMul, "1" -> MMove d b
      OpMul, "0" -> MLoad d (LInt "0")
      _, _ -> MBin op d a b
    Nothing, Just y -> case op, normalizeStr y of
      OpAdd, "0" -> MMove d a
      OpSub, "0" -> MMove d a
      OpMul, "1" -> MMove d a
      OpMul, "0" -> MLoad d (LInt "0")
      OpDiv, "1" -> MMove d a
      _, _ -> MBin op d a b
    _, _ -> case fixedOf known a, fixedOf known b of
      Just x, Just y ->
        let
          s = max x.scale y.scale
          xa = scale10 x.value (s - x.scale)
          ya = scale10 y.value (s - y.scale)
        in
          case op of
            OpAdd -> MLoad d (LFixed (addStr xa ya) s)
            OpSub -> MLoad d (LFixed (subStr xa ya) s)
            OpMul -> MLoad d (LFixed (mulStr x.value y.value) (x.scale + y.scale))
            OpDiv -> MBin op d a b
            OpMod -> MBin op d a b
      _, _ -> case ratOf known a, ratOf known b of
        Just x, Just y ->
          let
            r = case op of
              OpAdd -> Rational.add x y
              OpSub -> Rational.sub x y
              OpMul -> Rational.mul x y
              OpDiv -> Rational.divR x y
              OpMod -> x
          in
            case op of
              OpMod -> MBin op d a b
              _ -> MLoad d (LRational r.num r.den)
        _, _ -> MBin op d a b

foldCmp :: Map VReg Lit -> CmpOp -> VReg -> VReg -> VReg -> MInstr
foldCmp known op d a b =
  case intOf known a, intOf known b of
    Just x, Just y ->
      let c = cmpStr x y
      in MLoad d
        ( LBool case op of
            CmpEq -> c == 0
            CmpLt -> c < 0
            CmpGt -> c > 0
        )
    _, _ -> case fixedOf known a, fixedOf known b of
      Just x, Just y ->
        let
          s = max x.scale y.scale
          c = cmpStr (scale10 x.value (s - x.scale)) (scale10 y.value (s - y.scale))
        in
          MLoad d
            ( LBool case op of
                CmpEq -> c == 0
                CmpLt -> c < 0
                CmpGt -> c > 0
            )
      _, _ -> case ratOf known a, ratOf known b of
        Just x, Just y ->
          let c = Rational.cmp x y
          in
            MLoad d
              ( LBool case op of
                  CmpEq -> c == 0
                  CmpLt -> c < 0
                  CmpGt -> c > 0
              )
        _, _ -> MCmp op d a b

--------------------------------------------------------------------------------
-- Unreachable block pruning. Constant conditional jumps become unconditional
-- jumps or disappear in foldPass; this pass removes the dead arm that leaves
-- behind, and trims jumps that now target the immediately following label.
--------------------------------------------------------------------------------

dropUnreachable :: Array MInstr -> Array MInstr
dropUnreachable instrs =
  removeJumpsToNextLabel (go true instrs)
  where
  targets = targetLabels instrs

  go :: Boolean -> Array MInstr -> Array MInstr
  go reachable rest = case Array.uncons rest of
    Nothing -> []
    Just { head, tail } -> case head of
      MJump _ | reachable -> Array.cons head (go false tail)
      MJump _ -> go false tail
      MHalt _ | reachable -> Array.cons head (go false tail)
      MHalt _ -> go false tail
      MRet _ | reachable -> Array.cons head (go false tail)
      MRet _ -> go false tail
      MTailCall _ _ | reachable -> Array.cons head (go false tail)
      MTailCall _ _ -> go false tail
      MLabel lbl ->
        let reachable' = reachable || Set.member lbl targets
        in if reachable' then Array.cons head (go true tail)
           else go false tail
      _ ->
        if reachable then Array.cons head (go true tail)
        else go false tail

targetLabels :: Array MInstr -> Set.Set String
targetLabels = Set.fromFoldable <<< Array.concatMap case _ of
  MJump lbl -> [ lbl ]
  MJumpIfFalse _ lbl -> [ lbl ]
  _ -> []

removeJumpsToNextLabel :: Array MInstr -> Array MInstr
removeJumpsToNextLabel instrs = case Array.uncons instrs of
  Nothing -> []
  Just { head, tail } -> case head, Array.uncons tail of
    MJump lbl, Just { head: MLabel next } | lbl == next ->
      removeJumpsToNextLabel tail
    _, _ -> Array.cons head (removeJumpsToNextLabel tail)

--------------------------------------------------------------------------------
-- Common-subexpression elimination. The expression map is cleared at labels
-- because a value computed before a branch may not dominate a post-join use.
--------------------------------------------------------------------------------

cse :: Array MInstr -> Array MInstr
cse = go Map.empty
  where
  go :: Map String VReg -> Array MInstr -> Array MInstr
  go seen instrs = case Array.uncons instrs of
    Nothing -> []
    Just { head, tail } -> case head of
      MLabel _ -> Array.cons head (go Map.empty tail)
      _ -> case stableKey head, defOf head of
        Just key, Just d | isPure head ->
          case Map.lookup key seen of
            Just existing -> Array.cons (MMove d existing) (go seen tail)
            Nothing -> Array.cons head (go (Map.insert key d seen) tail)
        _, _ -> Array.cons head (go seen tail)

stableKey :: MInstr -> Maybe String
stableKey = case _ of
  MBin op _ a b -> Just ("bin:" <> show op <> ":" <> show a <> ":" <> show b)
  MCmp op _ a b -> Just ("cmp:" <> show op <> ":" <> show a <> ":" <> show b)
  MRecordGet _ r fld -> Just ("recordGet:" <> show r <> ":" <> fld)
  MListGet _ l i -> Just ("listGet:" <> show l <> ":" <> show i)
  _ -> Nothing

--------------------------------------------------------------------------------
-- Dead-code elimination: drop pure instructions whose result is never used.
-- Iterated to a fixpoint (removing one def can free another).
--------------------------------------------------------------------------------

dce :: Array MInstr -> Array MInstr
dce instrs =
  let
    used = Set.fromFoldable (Array.concatMap usesOf instrs)
    kept = Array.filter (keep used) instrs
  in
    if Array.length kept == Array.length instrs then instrs
    else dce kept
  where
  keep used i = case defOf i of
    Just d -> not (isPure i) || Set.member d used
    Nothing -> true
