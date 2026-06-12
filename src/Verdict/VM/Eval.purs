-- | A pure reference interpreter for the deterministic FinVM subset Verdict
-- | emits. It executes the *actual* ProgramVM (constant pool + bytecode), so it
-- | validates real output, not an idealized model. Builtins (db/cache/logic/
-- | bigint) run against a deterministic in-memory world.
-- |
-- | Scope: enough to test compiled programs end to end and to assert that
-- | optimization preserves semantics, including deterministic process
-- | scheduling for Verdict's process primitives.
module Verdict.VM.Eval
  ( Value(..)
  , encodeValueJson
  , runProgram
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, any, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (joinWith, replaceAll, split, toLower, toUpper, trim)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..), fst)
import Foreign.Object as FO
import Verdict.Eval.BigInt (addStr, cmpStr, divFloorStr, gcdStr, modInvStr, modPowStr, modStr, mulStr, normalizeStr, powStr, scale10, sqrtFloorStr, subStr)
import Verdict.Eval.Rational as Rational
import Verdict.Eval.Regex as Regex
import Verdict.Eval.Series as Series
import Verdict.FinVM.Types (FunctionVM, InstructionVM(..), ProgramVM, ValueVM(..))

-- | Runtime values.
data Value
  = VUnit
  | VInt String
  | VFixed String Int
  | VRational String String
  | VBool Boolean
  | VString String
  | VList (Array Value)
  | VRecord (Array (Tuple String Value))

derive instance eqValue :: Eq Value
instance showValue :: Show Value where
  show = serialize

encodeValueJson :: Value -> Json
encodeValueJson = case _ of
  VUnit -> J.jsonNull
  VInt s -> "int" := s ~> J.jsonEmptyObject
  VFixed v sc ->
    "fixed" := ("value" := v ~> "scale" := sc ~> J.jsonEmptyObject)
      ~> J.jsonEmptyObject
  VRational n d ->
    "rational" := ("numerator" := n ~> "denominator" := d ~> J.jsonEmptyObject)
      ~> J.jsonEmptyObject
  VBool b -> "bool" := b ~> J.jsonEmptyObject
  VString s -> "string" := s ~> J.jsonEmptyObject
  VList xs -> "list" := map encodeValueJson xs ~> J.jsonEmptyObject
  VRecord fs ->
    "record" := J.fromObject (FO.fromFoldable (map (\(Tuple k v) -> Tuple k (encodeValueJson v)) fs))
      ~> J.jsonEmptyObject

type World =
  { db :: Map String (Map String Value)
  , cache :: Map String Value
  , files :: Map String String
  , logs :: Array String
  , nextId :: Int
  , steps :: Int
  , procs :: Map String Process
  , ready :: Array String
  , nextPid :: Int
  }

type Eval a = StateT World (Either String) a

err :: forall a. String -> Eval a
err = lift <<< Left

initWorld :: World
initWorld =
  { db: Map.empty
  , cache: Map.empty
  , files: Map.empty
  , logs: []
  , nextId: 0
  , steps: 0
  , procs: Map.empty
  , ready: []
  , nextPid: 0
  }

fuel :: Int
fuel = 5_000_000

runProgram :: ProgramVM -> Either String Value
runProgram prog = case FO.lookup prog.entrypoint prog.functions of
  Nothing -> Left ("no entrypoint: " <> prog.entrypoint)
  Just fn ->
    let
      p0 =
        { frame: initialFrame fn []
        , mailbox: []
        , blocked: false
        }
      world0 = initWorld
        { procs = Map.singleton "main" p0
        , ready = [ "main" ]
        , nextPid = 0
        }
    in
      map fst (runStateT (schedule prog) world0)

fromVM :: ValueVM -> Value
fromVM = case _ of
  VUnitVM -> VUnit
  VIntVM s -> VInt s
  VFixedVM v sc -> VFixed v sc
  VRationalVM n d ->
    let r = Rational.reduce n d
    in VRational r.num r.den
  VBoolVM b -> VBool b
  VStringVM s -> VString s

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

type Regs = Map Int Value

labelTable :: Array InstructionVM -> Map String Int
labelTable = foldlWithIndex
  (\i m instr -> case instr of
      Label l -> Map.insert l i m
      _ -> m)
  Map.empty

-- | One trampoline frame: the function being executed, its register file, the
-- | program counter, and a tiny call stack for synchronous `CALL`. `TAIL_CALL`
-- | swaps only the current frame and preserves the caller stack.
type Caller = { fn :: FunctionVM, regs :: Regs, pc :: Int, dst :: Int }
type Frame = { fn :: FunctionVM, regs :: Regs, pc :: Int, stack :: Array Caller }
type Process = { frame :: Frame, mailbox :: Array Value, blocked :: Boolean }

data ProcResult
  = SContinue Frame
  | SDone Value
  | SBlocked Frame
  | SYield Frame

argRegs :: Array Value -> Regs
argRegs as = Map.fromFoldable (Array.mapWithIndex Tuple as)

initialFrame :: FunctionVM -> Array Value -> Frame
initialFrame fn args = { fn, regs: argRegs args, pc: 0, stack: [] }

schedule :: ProgramVM -> Eval Value
schedule prog = do
  w <- get
  case Array.uncons w.ready of
    Nothing -> err "deadlock: all processes blocked"
    Just { head: pid, tail } -> case Map.lookup pid w.procs of
      Nothing -> do
        put w { ready = tail }
        schedule prog
      Just proc -> do
        put w
          { ready = tail
          , procs = Map.insert pid proc { blocked = false } w.procs
          }
        result <- runProcess prog pid proc.frame
        case result of
          SDone v | pid == "main" -> pure v
          SDone _ -> do
            w2 <- get
            put w2 { procs = Map.delete pid w2.procs }
            schedule prog
          SBlocked frame -> do
            saveProcess pid frame true false
            schedule prog
          SYield frame -> do
            saveProcess pid frame false true
            schedule prog
          SContinue _ -> err "internal scheduler error: process returned SContinue"

saveProcess :: String -> Frame -> Boolean -> Boolean -> Eval Unit
saveProcess pid frame blocked requeue = do
  w <- get
  case Map.lookup pid w.procs of
    Nothing -> pure unit
    Just proc -> do
      let ready' = if requeue then Array.snoc w.ready pid else w.ready
      put w
        { procs = Map.insert pid proc { frame = frame, blocked = blocked } w.procs
        , ready = ready'
        }

runProcess :: ProgramVM -> String -> Frame -> Eval ProcResult
runProcess prog pid frame0 = tailRecM loop frame0
  where
  getR regs r = case Map.lookup r regs of
    Just v -> pure v
    Nothing -> err ("read of unset register " <> show r)

  set regs d v = Map.insert d v regs
  traverseR regs = traverse (getR regs)

  lookupFn fid = case FO.lookup fid prog.functions of
    Just f -> pure f
    Nothing -> err ("call to unknown function " <> fid)

  loop :: Frame -> Eval (Step Frame ProcResult)
  loop frame@{ fn, pc } = do
    w <- get
    if w.steps > fuel then err "step limit exceeded (possible infinite loop)"
    else do
      put w { steps = w.steps + 1 }
      case Array.index fn.instructions pc of
        Nothing -> err "program counter ran off the end (missing RETURN/HALT?)"
        Just instr -> step frame instr

  -- | Execute one instruction, yielding either the next frame (`Loop`) or the
  -- | function's result (`Done`). `next`/`cont` build the continuation frame.
  step :: Frame -> InstructionVM -> Eval (Step Frame ProcResult)
  step frame@{ fn, regs, pc, stack } instr =
    let
      labels = labelTable fn.instructions
      jumpTo l = case Map.lookup l labels of
        Just i -> pure i
        Nothing -> err ("unknown label: " <> l)
      cont regs' pc' = pure (Loop frame { regs = regs', pc = pc' })
      next regs' = cont regs' (pc + 1)
      suspend result = pure (Done result)
      returnValue v = case Array.unsnoc stack of
        Nothing -> suspend (SDone v)
        Just { init, last: caller } ->
          pure (Loop { fn: caller.fn, regs: set caller.regs caller.dst v, pc: caller.pc, stack: init })
      arith f rf d a b = do
        va <- getR regs a
        vb <- getR regs b
        res <- numAddLike f rf va vb
        next (set regs d res)
      mul d a b = do
        va <- getR regs a
        vb <- getR regs b
        res <- numMul va vb
        next (set regs d res)
      divValue d a b = do
        va <- getR regs a
        vb <- getR regs b
        case va, vb of
          VInt x, VInt y -> next (set regs d (VInt (divFloorStr x y)))
          VRational n d1, VRational m d2 ->
            let r = Rational.divR { num: n, den: d1 } { num: m, den: d2 }
            in next (set regs d (VRational r.num r.den))
          _, _ -> err "DIV expects integers or rationals"
      modValue d a b = do
        va <- getR regs a
        vb <- getR regs b
        case va, vb of
          VInt x, VInt y -> next (set regs d (VInt (modStr x y)))
          _, _ -> err "MOD expects integers"
      cmp test d a b = do
        va <- getR regs a
        vb <- getR regs b
        c <- numCompare va vb
        next (set regs d (VBool (test c)))
    in case instr of
      Return r -> getR regs r >>= returnValue
      Halt r -> getR regs r >>= (suspend <<< SDone)
      Label _ -> next regs
      Jump l -> jumpTo l >>= cont regs
      JumpIfFalse c l -> do
        v <- getR regs c
        case v of
          VBool false -> jumpTo l >>= cont regs
          VBool true -> next regs
          _ -> err "JUMP_IF_FALSE on a non-boolean"
      LoadConst d i -> case Array.index prog.constants i of
        Just vm -> next (set regs d (fromVM vm))
        Nothing -> err ("bad constant index " <> show i)
      Move d s -> getR regs s >>= \v -> next (set regs d v)
      Add d a b -> arith addStr Rational.add d a b
      Sub d a b -> arith subStr Rational.sub d a b
      Mul d a b -> mul d a b
      Div d a b -> divValue d a b
      Mod d a b -> modValue d a b
      EqI d a b -> do
        va <- getR regs a
        vb <- getR regs b
        next (set regs d (VBool (va == vb)))
      LtI d a b -> cmp (\c -> c < 0) d a b
      GtI d a b -> cmp (\c -> c > 0) d a b
      Call d fid as -> do
        argsV <- traverseR regs as
        f2 <- lookupFn fid
        pure (Loop { fn: f2, regs: argRegs argsV, pc: 0, stack: Array.snoc stack { fn, regs, pc: pc + 1, dst: d } })
      -- Tail call: discard the current frame, continue in the callee. Its
      -- RETURN becomes this frame's result (the trampoline carries it up).
      TailCall fid as -> do
        argsV <- traverseR regs as
        f2 <- lookupFn fid
        pure (Loop { fn: f2, regs: argRegs argsV, pc: 0, stack })
      CallBuiltin d bid as -> do
        argsV <- traverseR regs as
        res <- callBuiltin bid argsV
        next (set regs d res)
      Spawn d fid as -> do
        argsV <- traverseR regs as
        f2 <- lookupFn fid
        w <- get
        let newPid = "p" <> show w.nextPid
        let child = { frame: initialFrame f2 argsV, mailbox: [], blocked: false }
        put w
          { nextPid = w.nextPid + 1
          , procs = Map.insert newPid child w.procs
          , ready = Array.snoc w.ready newPid
          }
        next (set regs d (VString newPid))
      Send p m -> do
        pidVal <- getR regs p
        msg <- getR regs m
        case pidVal of
          VString targetPid -> do
            w <- get
            case Map.lookup targetPid w.procs of
              Nothing -> next regs
              Just target -> do
                let shouldWake = targetPid /= pid && not (any (_ == targetPid) w.ready)
                put w
                  { procs = Map.insert targetPid target { mailbox = Array.snoc target.mailbox msg, blocked = false } w.procs
                  , ready = if shouldWake then Array.snoc w.ready targetPid else w.ready
                  }
                next regs
          _ -> err "PROC_SEND expects pid"
      Recv d -> do
        w <- get
        case Map.lookup pid w.procs of
          Nothing -> err ("missing process " <> show pid)
          Just proc -> case Array.uncons proc.mailbox of
            Nothing -> suspend (SBlocked frame)
            Just { head: msg, tail } -> do
              put w { procs = Map.insert pid proc { mailbox = tail } w.procs }
              next (set regs d msg)
      Yield ->
        suspend (SYield frame { pc = pc + 1 })
      Self d ->
        next (set regs d (VString pid))
      RecordNew d -> next (set regs d (VRecord []))
      RecordSet d r f v -> do
        rec <- getR regs r
        val <- getR regs v
        case rec of
          VRecord fs -> next (set regs d (VRecord (recUpsert f val fs)))
          _ -> err "RECORD_SET on non-record"
      RecordGet d r f -> do
        rec <- getR regs r
        case rec of
          VRecord fs -> case lookupField f fs of
            Just v -> next (set regs d v)
            Nothing -> err ("missing field " <> f)
          _ -> err "RECORD_GET on non-record"
      ListNew d -> next (set regs d (VList []))
      ListAppend d l v -> do
        lst <- getR regs l
        val <- getR regs v
        case lst of
          VList xs -> next (set regs d (VList (Array.snoc xs val)))
          _ -> err "LIST_APPEND on non-list"
      ListGet d l i -> do
        lst <- getR regs l
        idx <- getR regs i
        case lst, idx of
          VList xs, VInt s -> case Array.index xs (parseInt s) of
            Just v -> next (set regs d v)
            Nothing -> err "list index out of range"
          _, _ -> err "LIST_GET on non-list/index"
      ListLength d l -> do
        lst <- getR regs l
        case lst of
          VList xs -> next (set regs d (VInt (show (Array.length xs))))
          _ -> err "LIST_LENGTH on non-list"

-- traverse for Eval over Array
traverse :: forall a b. (a -> Eval b) -> Array a -> Eval (Array b)
traverse f = Array.foldM (\acc x -> map (Array.snoc acc) (f x)) []

--------------------------------------------------------------------------------
-- Numeric helpers (Int exact; Fixed with scale alignment)
--------------------------------------------------------------------------------

numAddLike :: (String -> String -> String) -> (Rational.Rat -> Rational.Rat -> Rational.Rat) -> Value -> Value -> Eval Value
numAddLike f rf a b = case a, b of
  VInt x, VInt y -> pure (VInt (f x y))
  VFixed x sx, VFixed y sy ->
    let s = max sx sy
    in pure (VFixed (f (scale10 x (s - sx)) (scale10 y (s - sy))) s)
  VRational n d, VRational m e ->
    let r = rf { num: n, den: d } { num: m, den: e }
    in pure (VRational r.num r.den)
  _, _ -> err "arithmetic on non-numbers"

numMul :: Value -> Value -> Eval Value
numMul a b = case a, b of
  VInt x, VInt y -> pure (VInt (mulStr x y))
  VFixed x sx, VFixed y sy -> pure (VFixed (mulStr x y) (sx + sy))
  VRational n d, VRational m e ->
    let r = Rational.mul { num: n, den: d } { num: m, den: e }
    in pure (VRational r.num r.den)
  _, _ -> err "multiply on non-numbers"

numCompare :: Value -> Value -> Eval Int
numCompare a b = case a, b of
  VInt x, VInt y -> pure (cmpStr x y)
  VFixed x sx, VFixed y sy ->
    let s = max sx sy
    in pure (cmpStr (scale10 x (s - sx)) (scale10 y (s - sy)))
  VRational n d, VRational m e ->
    pure (Rational.cmp { num: n, den: d } { num: m, den: e })
  _, _ -> err "comparison on non-numbers"

parseInt :: String -> Int
parseInt s = fromMaybe 0 (Int.fromString s)

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

recUpsert :: String -> Value -> Array (Tuple String Value) -> Array (Tuple String Value)
recUpsert k v fs =
  if any (\(Tuple n _) -> n == k) fs
  then map (\(Tuple n old) -> if n == k then Tuple n v else Tuple n old) fs
  else Array.snoc fs (Tuple k v)

lookupField :: String -> Array (Tuple String Value) -> Maybe Value
lookupField k fs = map snd' (Array.find (\(Tuple n _) -> n == k) fs)
  where snd' (Tuple _ v) = v

--------------------------------------------------------------------------------
-- Builtins (deterministic in-memory world)
--------------------------------------------------------------------------------

callBuiltin :: String -> Array Value -> Eval Value
callBuiltin bid args = case bid, args of
  "logic.and@1", [ VBool a, VBool b ] -> pure (VBool (a && b))
  "logic.or@1", [ VBool a, VBool b ] -> pure (VBool (a || b))
  "logic.not@1", [ VBool a ] -> pure (VBool (not a))
  "bigint.modPow@1", [ VInt b, VInt e, VInt m ] -> pure (VInt (modPowStr b e m))
  "bigint.modInv@1", [ VInt a, VInt m ] -> pure (VInt (modInvStr a m))
  "math.gcd@1", [ VInt a, VInt b ] -> pure (VInt (gcdStr a b))
  "math.lcm@1", [ VInt a, VInt b ] ->
    pure (VInt (lcmStr a b))
  "math.pow@1", [ VInt b, VInt e ]
    | cmpStr e "0" < 0 -> err "math.pow expects a non-negative exponent"
    | otherwise -> pure (VInt (powStr b e))
  "math.sqrtFloor@1", [ VInt n ]
    | cmpStr n "0" < 0 -> err "math.sqrtFloor expects a non-negative integer"
    | otherwise -> pure (VInt (sqrtFloorStr n))

  "db.insert@1", [ VString table, rec ] -> do
    w <- get
    let idStr = show w.nextId
    let tbl = fromMaybe Map.empty (Map.lookup table w.db)
    put w { db = Map.insert table (Map.insert idStr rec tbl) w.db, nextId = w.nextId + 1 }
    pure (VString idStr)
  "db.get@1", [ VString table, VString idStr ] -> do
    w <- get
    pure (fromMaybe VUnit (Map.lookup table w.db >>= Map.lookup idStr))
  "db.update@1", [ VString table, VString idStr, rec ] -> do
    w <- get
    case Map.lookup table w.db >>= Map.lookup idStr of
      Nothing -> pure (VBool false)
      Just _ -> do
        let tbl = fromMaybe Map.empty (Map.lookup table w.db)
        put w { db = Map.insert table (Map.insert idStr rec tbl) w.db }
        pure (VBool true)
  "db.delete@1", [ VString table, VString idStr ] -> do
    w <- get
    case Map.lookup table w.db of
      Just tbl | Map.member idStr tbl -> do
        put w { db = Map.insert table (Map.delete idStr tbl) w.db }
        pure (VBool true)
      _ -> pure (VBool false)
  "db.query@1", [ VString table, query, _opts ] -> do
    w <- get
    let rows = maybe [] (Array.fromFoldable <<< Map.values) (Map.lookup table w.db)
    pure (VList (Array.filter (matchesQuery query) rows))
  "db.createIndex@1", [ VString _, VString _ ] -> pure VUnit
  "db.hash@1", [ VString table ] -> do
    w <- get
    let tbl = fromMaybe Map.empty (Map.lookup table w.db)
    let entries = map (\(Tuple k v) -> k <> "=" <> serialize v) (Map.toUnfoldable tbl :: Array (Tuple String Value))
    pure (VString (joinWith ";" entries))

  "cache.set@1", [ VString ns, VString key, val ] -> do
    w <- get
    put w { cache = Map.insert (ns <> "\x00" <> key) val w.cache }
    pure (VBool true)
  "cache.get@1", [ VString ns, VString key ] -> do
    w <- get
    pure (fromMaybe VUnit (Map.lookup (ns <> "\x00" <> key) w.cache))
  "cache.delete@1", [ VString ns, VString key ] -> do
    w <- get
    let k = ns <> "\x00" <> key
    let had = Map.member k w.cache
    put w { cache = Map.delete k w.cache }
    pure (VBool had)

  -- HTTP: deterministic reference behavior. Real hosts may wire these to the
  -- FinVM HTTP capability; tests only require stable shape and capability use.
  "http.get@1", [ VString url ] ->
    pure (httpResponse 200 true ("GET " <> url))
  "http.post@1", [ VString url, VString body ] ->
    pure (httpResponse 200 true ("POST " <> url <> " " <> body))

  -- System I/O: modeled as an in-memory filesystem/log for deterministic tests.
  "sys.log@1", [ VString msg ] -> do
    w <- get
    put w { logs = Array.snoc w.logs msg }
    pure VUnit
  "sys.cwd@1", [] -> pure (VString "/")
  "sys.readText@1", [ VString path ] -> do
    w <- get
    pure (maybe VUnit VString (Map.lookup path w.files))
  "sys.writeText@1", [ VString path, VString contents ] -> do
    w <- get
    put w { files = Map.insert path contents w.files }
    pure (VBool true)
  "sys.env@1", [ VString name ] ->
    pure case name of
      "PWD" -> VString "/"
      "VERDICT" -> VString "1"
      _ -> VUnit

  -- Data processing: host-backed fast paths over common list workloads.
  "data.sortInts@1", [ VList xs ] -> do
    ints <- intList xs
    pure (VList (map VInt (Array.sortBy cmpIntString ints)))
  "data.distinctInts@1", [ VList xs ] -> do
    ints <- intList xs
    pure (VList (map VInt (distinctStrings ints)))
  "data.sumInts@1", [ VList xs ] -> do
    ints <- intList xs
    pure (VInt (sumStrings ints))
  "data.averageFloor@1", [ VList xs ] -> do
    ints <- intList xs
    pure (VInt (if Array.null ints then "0" else divFloorStr (sumStrings ints) (show (Array.length ints))))

  -- Regex: host-backed string regex helpers.
  "regex.test@1", [ VString pattern, VString input ] ->
    pure (VBool (Regex.regexTest pattern input))
  "regex.findAll@1", [ VString pattern, VString input ] ->
    pure (VList (map VString (Regex.regexFindAll pattern input)))
  "regex.replace@1", [ VString pattern, VString replacement, VString input ] ->
    pure (VString (Regex.regexReplace pattern replacement input))
  "regex.split@1", [ VString pattern, VString input ] ->
    pure (VList (map VString (Regex.regexSplit pattern input)))

  -- Stats: dataframe-style fast paths for integer columns.
  "stats.min@1", [ VList xs ] -> intList xs >>= pure <<< VInt <<< minString
  "stats.max@1", [ VList xs ] -> intList xs >>= pure <<< VInt <<< maxString
  "stats.meanFloor@1", [ VList xs ] -> intList xs >>= pure <<< VInt <<< meanFloorString
  "stats.median@1", [ VList xs ] -> intList xs >>= pure <<< VInt <<< medianString
  "stats.percentileNearest@1", [ VInt pct, VList xs ] ->
    intList xs >>= pure <<< VInt <<< percentileNearestString (parseInt pct)
  "stats.varianceFloor@1", [ VList xs ] -> intList xs >>= pure <<< VInt <<< varianceFloorString
  "stats.stddevFloor@1", [ VList xs ] -> intList xs >>= pure <<< VInt <<< stddevFloorString
  "stats.describeInts@1", [ VList xs ] -> do
    ints <- intList xs
    pure (describeIntsValue ints)
  "stats.valueCountsInts@1", [ VList xs ] -> do
    ints <- intList xs
    pure (VList (map valueCountValue (valueCountsStrings ints)))
  "stats.rollingSumInts@1", [ VInt window, VList xs ] -> do
    ints <- intList xs
    pure (VList (map VInt (rollingSumStrings (parseInt window) ints)))

  -- Time-series / technical-analysis indicators.
  "series.sma@1", [ VList xs, VInt p ] -> series1 xs p Series.sma
  "series.ema@1", [ VList xs, VInt p ] -> series1 xs p Series.ema
  "series.wma@1", [ VList xs, VInt p ] -> series1 xs p Series.wma
  "series.rollingMedian@1", [ VList xs, VInt p ] -> series1 xs p Series.rollingMedian
  "series.momentum@1", [ VList xs, VInt p ] -> series1 xs p Series.momentum
  "series.roc@1", [ VList xs, VInt p ] -> series1 xs p Series.roc
  "series.rsi@1", [ VList xs, VInt p ] -> series1 xs p Series.rsi
  "series.macd@1", [ VList xs, VInt fast, VInt slow ] -> series1_2 xs fast slow Series.macd
  "series.macdSignal@1", [ VList xs, VInt fast, VInt slow, VInt sig ] -> series1_3 xs fast slow sig Series.macdSignal
  "series.macdHistogram@1", [ VList xs, VInt fast, VInt slow, VInt sig ] -> series1_3 xs fast slow sig Series.macdHistogram
  "series.slope@1", [ VList xs, VInt p ] -> series1 xs p Series.slope
  "series.rollingStd@1", [ VList xs, VInt p ] -> series1 xs p Series.rollingStd
  "series.realizedVol@1", [ VList xs, VInt p ] -> series1 xs p Series.realizedVol
  "series.ewmStd@1", [ VList xs, VInt p ] -> series1 xs p Series.ewmStd
  "series.stdevRatio@1", [ VList xs, VInt short, VInt long ] -> series1_2 xs short long Series.stdevRatio
  "series.atrApprox@1", [ VList xs, VInt p ] -> series1 xs p Series.atrApprox
  "series.bollingerUpper@1", [ VList xs, VInt p, VInt nstd ] -> series1_2 xs p nstd Series.bollingerUpper
  "series.bollingerLower@1", [ VList xs, VInt p, VInt nstd ] -> series1_2 xs p nstd Series.bollingerLower
  "series.zscore@1", [ VList xs, VInt p ] -> series1 xs p Series.zscore
  "series.percentileRank@1", [ VList xs, VInt p ] -> series1 xs p Series.percentileRank
  "series.drawdown@1", [ VList xs ] -> series0 xs Series.drawdown
  "series.pctChange@1", [ VList xs, VInt p ] -> series1 xs p Series.pctChange
  "series.ratio@1", [ VList a, VList b ] -> series2 a b Series.ratio
  "series.spread@1", [ VList a, VList b ] -> series2 a b Series.spread
  "series.rollingCorr@1", [ VList a, VList b, VInt p ] -> series2_1 a b p Series.rollingCorr
  "series.rollingBeta@1", [ VList a, VList b, VInt p ] -> series2_1 a b p Series.rollingBeta
  "series.relativeMomentum@1", [ VList a, VList b, VInt p ] -> series2_1 a b p Series.relativeMomentum
  "series.hedgeRatio@1", [ VList a, VList b, VInt p ] -> series2_1 a b p Series.hedgeRatio
  "series.add@1", [ VList a, VList b ] -> series2 a b Series.seriesAdd
  "series.sub@1", [ VList a, VList b ] -> series2 a b Series.seriesSub
  "series.mul@1", [ VList a, VList b ] -> series2 a b Series.seriesMul
  "series.div@1", [ VList a, VList b ] -> series2 a b Series.seriesDiv
  "series.abs@1", [ VList xs ] -> series0 xs Series.seriesAbs
  "series.clip@1", [ VList xs, VInt lo, VInt hi ] -> series1_2 xs lo hi Series.clip
  "series.shift@1", [ VList xs, VInt p ] -> series1 xs p Series.shift
  "series.diff@1", [ VList xs ] -> series0 xs Series.diff
  "series.log@1", [ VList xs ] -> series0 xs Series.logSeries
  "series.rollingMax@1", [ VList xs, VInt p ] -> series1 xs p Series.rollingMax
  "series.rollingMin@1", [ VList xs, VInt p ] -> series1 xs p Series.rollingMin
  "series.cummax@1", [ VList xs ] -> series0 xs Series.cummax
  "series.cummin@1", [ VList xs ] -> series0 xs Series.cummin
  "series.crossover@1", [ VList a, VList b ] -> series2 a b Series.crossover
  "series.crossunder@1", [ VList a, VList b ] -> series2 a b Series.crossunder
  "series.atrOhlc@1", [ VList h, VList l, VList c, VInt p ] -> series3_1 h l c p Series.atrOhlc
  "series.trueRange@1", [ VList h, VList l, VList c ] -> series3 h l c Series.trueRange
  "series.vwap@1", [ VList close, VList volume, VInt p ] -> series2_1 close volume p Series.vwap
  "series.obv@1", [ VList close, VList volume ] -> series2 close volume Series.obv
  "series.volumeSma@1", [ VList volume, VInt p ] -> series1 volume p Series.volumeSma
  "series.volumeRatio@1", [ VList volume, VInt p ] -> series1 volume p Series.volumeRatio
  "series.bodySize@1", [ VList open, VList close ] -> series2 open close Series.bodySize
  "series.upperWick@1", [ VList high, VList open, VList close ] -> series3 high open close Series.upperWick
  "series.lowerWick@1", [ VList low, VList open, VList close ] -> series3 low open close Series.lowerWick
  "series.rangePct@1", [ VList high, VList low ] -> series2 high low Series.rangePct

  -- Strings: deterministic, pure. These mirror the FinVM `str.*` FFI builtins;
  -- all operate on UTF-16 code units to match JS string semantics.
  "str.length@1", [ VString s ] -> pure (VInt (show (SCU.length s)))
  "str.concat@1", [ VString a, VString b ] -> pure (VString (a <> b))
  "str.slice@1", [ VString s, VInt startS, VInt lenS ] ->
    let start = max 0 (parseInt startS)
        len = max 0 (parseInt lenS)
    in pure (VString (SCU.take len (SCU.drop start s)))
  "str.indexOf@1", [ VString s, VString needle ] ->
    pure (VInt (show (fromMaybe (-1) (SCU.indexOf (Pattern needle) s))))
  "str.split@1", [ VString s, VString sep ] ->
    pure (VList (map VString (split (Pattern sep) s)))
  "str.toUpper@1", [ VString s ] -> pure (VString (toUpper s))
  "str.toLower@1", [ VString s ] -> pure (VString (toLower s))
  "str.trim@1", [ VString s ] -> pure (VString (trim s))
  "str.fromInt@1", [ VInt n ] -> pure (VString n)
  "str.toInt@1", [ VString s ] ->
    pure (if isIntLiteral s then VInt (normalizeStr s) else VUnit)
  "str.replace@1", [ VString s, VString from, VString to ] ->
    pure (VString (replaceAll (Pattern from) (Replacement to) s))

  -- JSON: Elm-style typed decoder/encoder recipes interpreted by the host.
  "json.decodeValue@1", [ recipe, value ] ->
    pure (resultValue (decodeJsonRecipe recipe value))
  "json.decodeString@1", [ recipe, VString source ] ->
    case jsonParser source of
      Left msg -> pure (errResult ("invalid JSON: " <> msg))
      Right json -> pure (resultValue (decodeJsonRecipe recipe (jsonToValue json)))
  "json.encodeValue@1", [ recipe, value ] ->
    case encodeJsonRecipe recipe value of
      Left msg -> err msg
      Right v -> pure v
  "json.encodeString@1", [ recipe, value ] ->
    case encodeJsonRecipe recipe value of
      Left msg -> err msg
      Right v -> pure (VString (J.stringify (valueToJson v)))
  "json.null@1", [] -> pure VUnit
  "json.object@1", [ VList fields ] -> do
    pairs <- traverse jsonObjectPair fields
    pure (VRecord pairs)

  _, _ -> err ("unsupported builtin in reference VM: " <> bid)

jsonToValue :: Json -> Value
jsonToValue json =
  J.caseJson
    (\_ -> VUnit)
    VBool
    jsonNumberToValue
    VString
    (VList <<< map jsonToValue)
    (VRecord <<< map (\(Tuple k v) -> Tuple k (jsonToValue v)) <<< (FO.toUnfoldable :: FO.Object Json -> Array (Tuple String Json)))
    json

jsonNumberToValue :: Number -> Value
jsonNumberToValue n = case Int.fromNumber n of
  Just i -> VInt (show i)
  Nothing -> VString (show n)

valueToJson :: Value -> Json
valueToJson = case _ of
  VUnit -> J.jsonNull
  VInt s -> case Int.fromString s of
    Just i -> J.fromNumber (Int.toNumber i)
    Nothing -> J.fromString s
  VFixed v sc -> J.fromString ("fixed(" <> v <> "," <> show sc <> ")")
  VRational n d -> J.fromString (Rational.render { num: n, den: d })
  VBool b -> J.fromBoolean b
  VString s -> J.fromString s
  VList xs -> J.fromArray (map valueToJson xs)
  VRecord fs -> J.fromObject (FO.fromFoldable (map (\(Tuple k v) -> Tuple k (valueToJson v)) fs))

decodeJsonRecipe :: Value -> Value -> Either String Value
decodeJsonRecipe recipe value = case recipeKind recipe of
  Just "value" -> Right value
  Just "int" -> case value of
    VInt _ -> Right value
    _ -> Left "expected Int"
  Just "string" -> case value of
    VString _ -> Right value
    _ -> Left "expected String"
  Just "bool" -> case value of
    VBool _ -> Right value
    _ -> Left "expected Bool"
  Just "field" -> do
    name <- recipeString "name" recipe
    sub <- recipeField "decoder" recipe
    case value of
      VRecord fs -> case lookupField name fs of
        Just v -> decodeJsonRecipe sub v
        Nothing -> Left ("missing field '" <> name <> "'")
      _ -> Left "expected object"
  Just "list" -> do
    sub <- recipeField "decoder" recipe
    case value of
      VList xs -> map VList (decodeJsonList sub xs)
      _ -> Left "expected list"
  Just "nullable" ->
    if value == VUnit then Right noneValue
    else do
      sub <- recipeField "decoder" recipe
      map someValue (decodeJsonRecipe sub value)
  Just other -> Left ("unknown decoder kind '" <> other <> "'")
  Nothing -> Left "invalid decoder"

encodeJsonRecipe :: Value -> Value -> Either String Value
encodeJsonRecipe recipe value = case recipeKind recipe of
  Just "value" -> Right value
  Just "int" -> case value of
    VInt _ -> Right value
    _ -> Left "json int encoder expected Int"
  Just "string" -> case value of
    VString _ -> Right value
    _ -> Left "json string encoder expected String"
  Just "bool" -> case value of
    VBool _ -> Right value
    _ -> Left "json bool encoder expected Bool"
  Just "list" -> do
    sub <- recipeField "encoder" recipe
    case value of
      VList xs -> map VList (encodeJsonList sub xs)
      _ -> Left "json list encoder expected List"
  Just "nullable" -> case optionValue value of
    Just Nothing -> Right VUnit
    Just (Just v) -> case recipeField "encoder" recipe of
      Left msg -> Left msg
      Right sub -> encodeJsonRecipe sub v
    Nothing -> Left "json nullable encoder expected Option"
  Just other -> Left ("unknown encoder kind '" <> other <> "'")
  Nothing -> Left "invalid encoder"

decodeJsonList :: Value -> Array Value -> Either String (Array Value)
decodeJsonList recipe xs = case Array.uncons xs of
  Nothing -> Right []
  Just { head, tail } -> do
    h <- decodeJsonRecipe recipe head
    t <- decodeJsonList recipe tail
    Right (Array.cons h t)

encodeJsonList :: Value -> Array Value -> Either String (Array Value)
encodeJsonList recipe xs = case Array.uncons xs of
  Nothing -> Right []
  Just { head, tail } -> do
    h <- encodeJsonRecipe recipe head
    t <- encodeJsonList recipe tail
    Right (Array.cons h t)

recipeKind :: Value -> Maybe String
recipeKind recipe = case recipe of
  VRecord fs -> case lookupField "kind" fs of
    Just (VString k) -> Just k
    _ -> Nothing
  _ -> Nothing

recipeField :: String -> Value -> Either String Value
recipeField name recipe = case recipe of
  VRecord fs -> case lookupField name fs of
    Just v -> Right v
    Nothing -> Left ("missing recipe field '" <> name <> "'")
  _ -> Left "invalid recipe"

recipeString :: String -> Value -> Either String String
recipeString name recipe = do
  v <- recipeField name recipe
  case v of
    VString s -> Right s
    _ -> Left ("recipe field '" <> name <> "' must be a string")

resultValue :: Either String Value -> Value
resultValue = case _ of
  Left msg -> errResult msg
  Right v -> okResult v

okResult :: Value -> Value
okResult v =
  VRecord [ Tuple "$tag" (VString "Ok"), Tuple "$0" v ]

errResult :: String -> Value
errResult msg =
  VRecord [ Tuple "$tag" (VString "Err"), Tuple "$0" (VString msg) ]

someValue :: Value -> Value
someValue v =
  VRecord [ Tuple "$tag" (VString "Some"), Tuple "$0" v ]

noneValue :: Value
noneValue =
  VRecord [ Tuple "$tag" (VString "None") ]

optionValue :: Value -> Maybe (Maybe Value)
optionValue = case _ of
  VRecord fs -> case lookupField "$tag" fs of
    Just (VString "None") -> Just Nothing
    Just (VString "Some") -> map Just (lookupField "$0" fs)
    _ -> Nothing
  _ -> Nothing

jsonObjectPair :: Value -> Eval (Tuple String Value)
jsonObjectPair = case _ of
  VRecord fs -> case lookupField "key" fs, lookupField "value" fs of
    Just (VString k), Just v -> pure (Tuple k v)
    _, _ -> err "jsonObject expects { key, value } records"
  _ -> err "jsonObject expects { key, value } records"

httpResponse :: Int -> Boolean -> String -> Value
httpResponse status ok body =
  VRecord
    [ Tuple "status" (VInt (show status))
    , Tuple "ok" (VBool ok)
    , Tuple "body" (VString body)
    ]

intList :: Array Value -> Eval (Array String)
intList =
  traverse \v -> case v of
    VInt n -> pure n
    _ -> err "data.* integer list builtin received a non-int element"

seriesOut :: Array String -> Value
seriesOut xs =
  VList (map VInt xs)

series0 :: Array Value -> (Array String -> Array String) -> Eval Value
series0 xs f = do
  ints <- intList xs
  pure (seriesOut (f ints))

series1 :: Array Value -> String -> (Array String -> String -> Array String) -> Eval Value
series1 xs p f = do
  ints <- intList xs
  pure (seriesOut (f ints p))

series1_2 :: Array Value -> String -> String -> (Array String -> String -> String -> Array String) -> Eval Value
series1_2 xs a b f = do
  ints <- intList xs
  pure (seriesOut (f ints a b))

series1_3 :: Array Value -> String -> String -> String -> (Array String -> String -> String -> String -> Array String) -> Eval Value
series1_3 xs a b c f = do
  ints <- intList xs
  pure (seriesOut (f ints a b c))

series2 :: Array Value -> Array Value -> (Array String -> Array String -> Array String) -> Eval Value
series2 a b f = do
  xs <- intList a
  ys <- intList b
  pure (seriesOut (f xs ys))

series2_1 :: Array Value -> Array Value -> String -> (Array String -> Array String -> String -> Array String) -> Eval Value
series2_1 a b p f = do
  xs <- intList a
  ys <- intList b
  pure (seriesOut (f xs ys p))

series3 :: Array Value -> Array Value -> Array Value -> (Array String -> Array String -> Array String -> Array String) -> Eval Value
series3 a b c f = do
  xs <- intList a
  ys <- intList b
  zs <- intList c
  pure (seriesOut (f xs ys zs))

series3_1 :: Array Value -> Array Value -> Array Value -> String -> (Array String -> Array String -> Array String -> String -> Array String) -> Eval Value
series3_1 a b c p f = do
  xs <- intList a
  ys <- intList b
  zs <- intList c
  pure (seriesOut (f xs ys zs p))

cmpIntString :: String -> String -> Ordering
cmpIntString a b =
  let c = cmpStr a b
  in if c < 0 then LT else if c > 0 then GT else EQ

distinctStrings :: Array String -> Array String
distinctStrings =
  foldl (\acc x -> if Array.elem x acc then acc else Array.snoc acc x) []

sumStrings :: Array String -> String
sumStrings =
  foldl (\acc x -> addStr acc x) "0"

sortedIntStrings :: Array String -> Array String
sortedIntStrings =
  Array.sortBy cmpIntString

minString :: Array String -> String
minString xs = fromMaybe "0" (Array.head (sortedIntStrings xs))

maxString :: Array String -> String
maxString xs = fromMaybe "0" (Array.last (sortedIntStrings xs))

meanFloorString :: Array String -> String
meanFloorString xs =
  if Array.null xs then "0" else divFloorStr (sumStrings xs) (show (Array.length xs))

medianString :: Array String -> String
medianString xs =
  let
    sorted = sortedIntStrings xs
    n = Array.length sorted
    mid = n / 2
  in
    if n == 0 then "0"
    else if mod n 2 == 1 then fromMaybe "0" (Array.index sorted mid)
    else
      let
        a = fromMaybe "0" (Array.index sorted (mid - 1))
        b = fromMaybe "0" (Array.index sorted mid)
      in
        divFloorStr (addStr a b) "2"

percentileNearestString :: Int -> Array String -> String
percentileNearestString pct xs =
  let
    sorted = sortedIntStrings xs
    n = Array.length sorted
    p = max 0 (min 100 pct)
    rank = if n == 0 then 0 else ((p * n + 99) / 100) - 1
    ix = max 0 (min (n - 1) rank)
  in
    if n == 0 then "0" else fromMaybe "0" (Array.index sorted ix)

varianceFloorString :: Array String -> String
varianceFloorString xs =
  if Array.null xs then "0"
  else
    let
      mean = meanFloorString xs
      sumSq =
        foldl
          ( \acc x ->
              let d = subStr x mean
              in addStr acc (mulStr d d)
          )
          "0"
          xs
    in
      divFloorStr sumSq (show (Array.length xs))

stddevFloorString :: Array String -> String
stddevFloorString =
  sqrtFloorStr <<< varianceFloorString

describeIntsValue :: Array String -> Value
describeIntsValue xs =
  VRecord
    [ Tuple "count" (VInt (show (Array.length xs)))
    , Tuple "sum" (VInt (sumStrings xs))
    , Tuple "min" (VInt (minString xs))
    , Tuple "max" (VInt (maxString xs))
    , Tuple "mean" (VInt (meanFloorString xs))
    , Tuple "median" (VInt (medianString xs))
    , Tuple "variance" (VInt (varianceFloorString xs))
    , Tuple "stddev" (VInt (stddevFloorString xs))
    ]

valueCountsStrings :: Array String -> Array (Tuple String Int)
valueCountsStrings xs =
  foldl addCount [] (sortedIntStrings xs)
  where
  addCount acc x = case Array.unsnoc acc of
    Just { init, last: Tuple v n } | v == x -> Array.snoc init (Tuple v (n + 1))
    _ -> Array.snoc acc (Tuple x 1)

valueCountValue :: Tuple String Int -> Value
valueCountValue (Tuple v n) =
  VRecord [ Tuple "value" (VInt v), Tuple "count" (VInt (show n)) ]

rollingSumStrings :: Int -> Array String -> Array String
rollingSumStrings window xs =
  if window <= 0 then []
  else go 0 []
  where
  n = Array.length xs

  go start acc =
    if start + window > n then acc
    else go (start + 1) (Array.snoc acc (sumWindow start 0 "0"))

  sumWindow start offset acc =
    if offset == window then acc
    else
      let v = fromMaybe "0" (Array.index xs (start + offset))
      in sumWindow start (offset + 1) (addStr acc v)

lcmStr :: String -> String -> String
lcmStr a b =
  let g = gcdStr a b
  in if g == "0" then "0" else absIntString (divFloorStr (mulStr a b) g)

absIntString :: String -> String
absIntString s =
  if SCU.take 1 s == "-" then SCU.drop 1 s else s

-- | Is `s` a plain decimal integer literal (optional leading '-', then digits)?
isIntLiteral :: String -> Boolean
isIntLiteral s =
  let body = if SCU.take 1 s == "-" then SCU.drop 1 s else s
  in body /= "" && Array.all (\c -> c >= '0' && c <= '9') (SCU.toCharArray body)

-- | Mongo-ish query: a record of field -> (scalar | { "$op": value }).
matchesQuery :: Value -> Value -> Boolean
matchesQuery query row = case query, row of
  VRecord conds, VRecord fields -> all (matchField fields) conds
  _, _ -> false
  where
  matchField fields (Tuple fname cond) = case lookupField fname fields of
    Nothing -> false
    Just fv -> matchCond cond fv

matchCond :: Value -> Value -> Boolean
matchCond cond fv = case cond of
  VRecord ops -> all (applyOp fv) ops
  scalar -> scalar == fv
  where
  applyOp v (Tuple op rhs) = case op, v, rhs of
    "$eq", _, _ -> v == rhs
    "$gt", VInt a, VInt b -> cmpStr a b > 0
    "$gte", VInt a, VInt b -> cmpStr a b >= 0
    "$lt", VInt a, VInt b -> cmpStr a b < 0
    "$lte", VInt a, VInt b -> cmpStr a b <= 0
    _, _, _ -> false

--------------------------------------------------------------------------------
-- Canonical serialization (used by show + db.hash)
--------------------------------------------------------------------------------

serialize :: Value -> String
serialize = case _ of
  VUnit -> "unit"
  VInt s -> s
  VFixed v sc -> "fixed(" <> v <> "," <> show sc <> ")"
  VRational n d -> Rational.render { num: n, den: d }
  VBool b -> show b
  VString s -> show s
  VList xs -> "[" <> joinWith "," (map serialize xs) <> "]"
  VRecord fs -> "{" <> joinWith "," (map (\(Tuple k v) -> k <> "=" <> serialize v) fs) <> "}"
