module Test.Main where

import Prelude

import Data.Array (all, drop, length)
import Data.Either (Either(..), either, isRight)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), contains, replaceAll, split)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref as Ref
import Foreign.Object as FO
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (setExitCode)
import Data.Tuple (Tuple(..))
import Verdict.Compiler (compile, compileJS, compileProgram, compileProject)
import Verdict.VM.Eval (runProgram)

-- Dependency-free assertion harness.

assert :: Ref.Ref Int -> String -> Boolean -> Effect Unit
assert fails name ok =
  if ok then log ("  ok   " <> name)
  else do
    error ("  FAIL " <> name)
    _ <- Ref.modify (_ + 1) fails
    pure unit

out :: String -> String
out src = case compile src of
  Right o -> o
  Left _ -> ""

hasAll :: String -> Array String -> Boolean
hasAll src needles =
  let o = out src
  in sum (map (\n -> if contains (Pattern n) o then 0 else 1) needles) == 0

hasNone :: String -> Array String -> Boolean
hasNone src needles =
  let o = out src
  in sum (map (\n -> if contains (Pattern n) o then 1 else 0) needles) == 0

-- An effectful builtin lowers to FinVM 1.1.0's async protocol: build the intent
-- (EFFECT_NEW), suspend this process on it (EFFECT_AWAIT), then receive the reply
-- (PROC_RECEIVE) and unwrap it (VARIANT_PAYLOAD) — not the old synchronous
-- EFFECT_REQUEST + LOAD_INPUT, which blocked the whole VM.
hasEffectProtocol :: String -> String -> Boolean
hasEffectProtocol src typ =
  hasAll src [ "\"EFFECT_NEW\"", typ, "\"EFFECT_AWAIT\"", "\"PROC_RECEIVE\"", "\"VARIANT_PAYLOAD\"" ]
    && hasNone src [ "\"EFFECT_REQUEST\"", "\"LOAD_INPUT\"" ]

isError :: String -> Boolean
isError src = case compile src of
  Left _ -> true
  Right _ -> false

errContains :: String -> String -> Boolean
errContains src needle = case compile src of
  Left msg -> contains (Pattern needle) msg
  Right _ -> false

readExample :: String -> Effect String
readExample name = readTextFile UTF8 ("examples/" <> name <> ".verdict")

-- Compile, run on the reference VM, and compare the serialized result value.
evalsTo :: String -> String -> Boolean
evalsTo src expected = case compileProgram src >>= runProgram of
  Right v -> show v == expected
  Left _ -> false

capsOf :: String -> Array String
capsOf src = either (const [ "<error>" ]) _.capabilities (compileProgram src)

funcCount :: String -> Int
funcCount src = either (const (-1)) (FO.size <<< _.functions) (compileProgram src)

hasFunction :: String -> String -> Boolean
hasFunction src name = case compileProgram src of
  Right p -> case FO.lookup name p.functions of
    Just _ -> true
    Nothing -> false
  Left _ -> false

-- Count occurrences of a tag in the output JSON.
countTag :: String -> String -> Int
countTag src tag = length (split (Pattern ("\"" <> tag <> "\"")) (out src)) - 1

m :: String -> String
m body = "module Main exposing (main)\n" <> body <> "\n"

-- Compile a multi-file project (module-name -> source) and run it on the ref VM.
evalsProjectTo :: Array (Tuple String String) -> String -> String -> Boolean
evalsProjectTo mods entry expected =
  case compileProject (FO.fromFoldable mods) entry >>= runProgram of
    Right v -> show v == expected
    Left _ -> false

projectErrors :: Array (Tuple String String) -> String -> Boolean
projectErrors mods entry = case compileProject (FO.fromFoldable mods) entry of
  Left _ -> true
  Right _ -> false

showcaseSource :: String
showcaseSource =
  """
module Main exposing (main)

type Audit a =
  | Audited a Int
  | Skipped

keepPositive : { score : Int, weight : Int } -> Bool
keepPositive row =
  row.score > 0

weighted : { score : Int, weight : Int } -> Int
weighted row =
  row.score * row.weight

add : Int -> Int -> Int
add a b =
  a + b

review : { score : Int, weight : Int } -> Audit { score : Int, weight : Int }
review row =
  if mod(row.score, 2) == 0 then Audited(row, row.score * row.weight)
  else Skipped

reviewPoints : Audit { score : Int, weight : Int } -> Int
reviewPoints audit =
  match audit {
    Audited row points -> points + row.score,
    Skipped -> 0
  }

safeAt : List { score : Int, weight : Int } -> Int -> Option { score : Int, weight : Int }
safeAt xs i =
  if i < length(xs) then Some(xs[i])
  else None

optionScore : Option { score : Int, weight : Int } -> Int
optionScore opt =
  match opt {
    Some row -> row.score,
    None -> 0
  }

sumTo : Int -> Int -> Int
sumTo acc n =
  switch n {
    0 -> acc,
    _ -> sumTo(acc + n, n - 1)
  }

main : Int
main =
  let rows = [
    { score = 5, weight = 2 },
    { score = -1, weight = 9 },
    { score = 4, weight = 3 },
    { score = 6, weight = 1 }
  ] in
  let positives = filter(keepPositive, rows) in
  let weightedScores = map(weighted, positives) in
  let total = foldl(add, 0, weightedScores) in
  let firstScore = optionScore(safeAt(positives, 0)) in
  let parity = mod(total, 7) in
  let inspected = reviewPoints(review(rows[2])) in
  total + firstScore + parity + inspected + sumTo(0, 4)
"""

main :: Effect Unit
main = do
  fails <- Ref.new 0
  log "Verdict compiler tests (MIR pipeline)"

  -- Constant folding: pure integer arithmetic is evaluated at compile time, so
  -- the ADD/MUL disappear and a single folded constant remains.
  let folded = m "main : Int\nmain = 40 + 2 * 3"
  assert fails "folds 40 + 2*3 to a constant (no ADD/MUL emitted)"
    (hasNone folded [ "\"ADD\"", "\"MUL\"" ])
  assert fails "folded result 46 is in the constant pool"
    (hasAll folded [ "\"46\"" ])

  -- BigInt-faithful folding (far beyond 64 bits).
  let big = m "main : Int\nmain = 2 * 100000000000000000000"
  assert fails "folds BigInt multiply exactly"
    (hasAll big [ "200000000000000000000" ] && hasNone big [ "\"MUL\"" ])
  assert fails "mod intrinsic runs -> 1"
    (evalsTo (m "main : Int\nmain = mod(7, 3)") "1")
  assert fails "mod intrinsic exact multiple -> 0"
    (evalsTo (m "main : Int\nmain = mod(10, 5)") "0")
  let foldedMod = m "main : Int\nmain = mod(17, 5)"
  assert fails "folds constant mod"
    (hasNone foldedMod [ "\"MOD\"" ] && evalsTo foldedMod "2")
  let dynamicMod = m "f : Int -> Int\nf n = mod(n, 3)\nmain : Int\nmain = f(7)"
  assert fails "non-constant mod emits MOD"
    (hasAll dynamicMod [ "\"MOD\"" ] && evalsTo dynamicMod "1")
  assert fails "mod is Int-only"
    (isError (m "main : Int\nmain = mod(1.5, 2)"))

  -- Fixed folding mirrors the reference VM's scale-alignment semantics.
  let fixedAdd = m "main : Fixed\nmain = 1.50 + 0.25"
  assert fails "folds Fixed addition"
    (hasNone fixedAdd [ "\"ADD\"" ] && hasAll fixedAdd [ "\"175\"" ])
  assert fails "folded Fixed addition runs -> fixed(175,2)"
    (evalsTo fixedAdd "fixed(175,2)")
  assert fails "folds Fixed multiplication with combined scale"
    (hasAll (m "main : Fixed\nmain = 1.50 * 0.20") [ "\"3000\"" ])

  -- Rational literals are compile-time constants with exact folded arithmetic.
  let rationalAdd = m "main : Rational\nmain = 1 % 3 + 1 % 6"
  assert fails "folds Rational addition"
    (evalsTo rationalAdd "1/2" && hasNone rationalAdd [ "\"ADD\"" ])
  assert fails "emits Rational constants"
    (hasAll rationalAdd [ "\"rational\"", "\"numerator\"" ])
  assert fails "folds Rational division exactly"
    (evalsTo (m "main : Rational\nmain = 2 % 3 / (4 % 9)") "3/2")
  assert fails "reduces Rational output"
    (evalsTo (m "main : Rational\nmain = 1 % 2 + 1 % 2") "1/1")
  assert fails "rejects Rational plus Bool"
    (isError (m "main : Rational\nmain = 1 % 2 + True"))

  -- Comparison folding -> VBool.
  assert fails "folds 3 > 1 to VBool"
    (hasAll (m "main : Bool\nmain = 3 > 1") [ "\"bool\"", "true" ])

  -- Branch elimination after a condition constant-folds.
  let foldedIfTrue = m "main : Int\nmain = if 120 > 100 then 1 else 0"
  assert fails "eliminates constant-true branch jumps"
    (hasNone foldedIfTrue [ "\"JUMP_IF_FALSE\"", "\"JUMP\"" ])
  assert fails "constant-true branch runs -> 1"
    (evalsTo foldedIfTrue "1")
  let foldedIfFalse = m "main : Int\nmain = if 1 > 2 then 1 else 0"
  assert fails "eliminates constant-false conditional jump"
    (hasNone foldedIfFalse [ "\"JUMP_IF_FALSE\"" ])
  assert fails "constant-false branch runs -> 0"
    (evalsTo foldedIfFalse "0")
  let dynamicIf = m "f : Int -> Int\nf x = if x > 0 then 1 else 0\nmain : Int\nmain = f(3)"
  assert fails "keeps non-constant conditional branch"
    (hasAll dynamicIf [ "\"JUMP_IF_FALSE\"" ])
  assert fails "non-constant branch runs -> 1"
    (evalsTo dynamicIf "1")

  -- Tail calls: a self-recursive call in tail position (through `switch`) reuses
  -- the frame as TAIL_CALL, runs correctly, and stays stack-safe at depth.
  let
    sumTo = "sumTo : Int -> Int -> Int\n"
      <> "sumTo acc n = switch n { 0 -> acc, _ -> sumTo(acc + n, n - 1) }\n"
  let tailProg = m (sumTo <> "main : Int\nmain = sumTo(0, 100)")
  assert fails "tail call through switch emits TAIL_CALL"
    (hasAll tailProg [ "\"TAIL_CALL\"" ])
  assert fails "tail-recursive sum runs -> 5050"
    (evalsTo tailProg "5050")
  assert fails "deep tail recursion is stack-safe (50000 -> 1250025000)"
    (evalsTo (m (sumTo <> "main : Int\nmain = sumTo(0, 50000)")) "1250025000")
  -- A call NOT in tail position stays a CALL (no spurious tail-call rewrite).
  let nonTail = m "id : Int -> Int\nid x = x\naddOne : Int -> Int\naddOne x = id(x) + 1\nmain : Int\nmain = addOne(5)"
  assert fails "non-tail call stays a CALL"
    (hasAll nonTail [ "\"CALL\"" ] && hasNone nonTail [ "\"TAIL_CALL\"" ])

  -- Higher-order functions are monomorphized away before lowering.
  let applyProg = m
        ( "apply : (a -> b) -> a -> b\n"
            <> "apply f x = f(x)\n"
            <> "inc : Int -> Int\n"
            <> "inc n = n + 1\n"
            <> "main : Int\n"
            <> "main = apply(inc, 5)"
        )
  assert fails "monomorphizes apply(inc, 5) -> 6"
    (evalsTo applyProg "6")
  assert fails "emits apply$inc specialization and drops original apply"
    (hasFunction applyProg "apply$inc" && not (hasFunction applyProg "apply"))
  let twiceProg = m
        ( "twice : (a -> a) -> a -> a\n"
            <> "twice f x = f(f(x))\n"
            <> "double : Int -> Int\n"
            <> "double n = n * 2\n"
            <> "main : Int\n"
            <> "main = twice(double, 3)"
        )
  assert fails "monomorphizes twice(double, 3) -> 12"
    (evalsTo twiceProg "12")
  let pipeProg = m
        ( "pipe : (a -> b) -> (b -> c) -> a -> c\n"
            <> "pipe f g x = g(f(x))\n"
            <> "inc : Int -> Int\n"
            <> "inc n = n + 1\n"
            <> "double : Int -> Int\n"
            <> "double n = n * 2\n"
            <> "main : Int\n"
            <> "main = pipe(inc, double, 10)"
        )
  assert fails "monomorphizes pipe with two function args -> 22"
    (evalsTo pipeProg "22")
  let passThroughProg = m
        ( "apply : (a -> b) -> a -> b\n"
            <> "apply f x = f(x)\n"
            <> "applyTwice : (a -> a) -> a -> a\n"
            <> "applyTwice f x = apply(f, apply(f, x))\n"
            <> "inc : Int -> Int\n"
            <> "inc n = n + 1\n"
            <> "main : Int\n"
            <> "main = applyTwice(inc, 5)"
        )
  assert fails "monomorphizes pass-through HOF calls -> 7"
    (evalsTo passThroughProg "7")
  let distinctProg = m
        ( "apply : (a -> b) -> a -> b\n"
            <> "apply f x = f(x)\n"
            <> "inc : Int -> Int\n"
            <> "inc n = n + 1\n"
            <> "double : Int -> Int\n"
            <> "double n = n * 2\n"
            <> "main : Int\n"
            <> "main = apply(inc, 5) + apply(double, 6)"
        )
  assert fails "distinct HOF specializations coexist"
    (evalsTo distinctProg "18" && hasFunction distinctProg "apply$inc" && hasFunction distinctProg "apply$double")
  assert fails "rejects function used as a value"
    (isError (m "inc : Int -> Int\ninc n = n + 1\nmain : Int\nmain = inc"))

  -- Strength reduction: x + 0 has no ADD.
  assert fails "strength-reduces n + 0 (no ADD)"
    (hasNone (m "id : Int -> Int\nid n = n + 0\nmain : Int\nmain = id(5)") [ "\"ADD\"" ])

  -- CSE: repeated non-constant expressions are computed once and reused.
  let cseProg = m "f : Int -> Int\nf x = (x * x) + (x * x)\nmain : Int\nmain = f(3)"
  assert fails "CSE emits one MUL for repeated x*x"
    (countTag cseProg "MUL" == 1)
  assert fails "CSE program runs -> 18"
    (evalsTo cseProg "18")

  -- Nullary inlining: a top-level value used from the entry is spliced and the
  -- now-unreachable function is dropped.
  let inlineProg = m "taxRate : Int\ntaxRate = 7\nmain : Int\nmain = taxRate + taxRate"
  assert fails "inlines and drops nullary taxRate"
    (not (hasFunction inlineProg "taxRate") && hasNone inlineProg [ "taxRate" ])
  assert fails "inlined nullary program runs -> 14"
    (evalsTo inlineProg "14")

  let recursiveNullary = m "loop : Int\nloop = loop\nmain : Int\nmain = if 1 > 0 then 1 else loop"
  assert fails "self-recursive nullary is not inlined"
    (hasFunction recursiveNullary "loop")
  assert fails "self-recursive nullary sanity program runs -> 1"
    (evalsTo recursiveNullary "1")

  -- HALT terminates the entrypoint (not RETURN).
  assert fails "entrypoint ends in HALT"
    (hasAll (m "main : Int\nmain = 1") [ "\"HALT\"" ])

  -- Register allocation keeps registerCount tight. A deep arithmetic tree over
  -- *non-constant* values (so it isn't folded away) stays well under the number
  -- of subexpressions thanks to last-use reuse.
  let deep = m "f : Int -> Int\nf x = x*x + x*x + x*x + x*x\nmain : Int\nmain = f(3)"
  assert fails "registerCount stays small via last-use reuse (<=6)"
    (countMaxRegLE deep 6)

  -- New surface features all compile.
  assert fails "if / comparison"
    (isRight (compile (m "main : Int\nmain = if 2 > 1 then 10 else 20")))
  assert fails "let binding"
    (isRight (compile (m "main : Int\nmain = let x = 7 in x + x")))
  assert fails "Fixed decimal literal -> VFixed"
    (hasAll (m "main : Fixed\nmain = 1.50") [ "\"fixed\"", "\"scale\"", "\"150\"" ])
  assert fails "String literal -> VString"
    (hasAll (m "main : String\nmain = \"hi\"") [ "\"string\"", "\"hi\"" ])
  let unitProg = m "main : Unit\nmain = unit"
  assert fails "unit literal runs -> unit"
    (evalsTo unitProg "unit")
  assert fails "unit literal emits null constant"
    (hasAll unitProg [ "null" ])
  assert fails "list literal -> LIST_NEW/APPEND"
    (hasAll (m "main : List Int\nmain = [1, 2]") [ "\"LIST_NEW\"", "\"LIST_APPEND\"" ])
  assert fails "length intrinsic runs -> 3"
    (evalsTo (m "main : Int\nmain = length([10, 20, 30])") "3")
  assert fails "get intrinsic runs -> 20"
    (evalsTo (m "main : Int\nmain = get([10, 20, 30], 1)") "20")
  assert fails "append intrinsic returns extended list"
    (evalsTo (m "main : Int\nmain = get(append([1, 2], 3), 2)") "3")
  assert fails "indexing sugar runs -> 20"
    (evalsTo (m "main : Int\nmain = [10, 20, 30][1]") "20")
  assert fails "indexing sugar chains with field access"
    (evalsTo (m "main : Int\nmain = { items = [1, 2] }.items[0]") "1")
  assert fails "indexing sugar desugars to get"
    (out (m "main : Int\nmain = [10, 20][0]") == out (m "main : Int\nmain = get([10, 20], 0)"))
  let mapProg = m "double : Int -> Int\ndouble n = n * 2\nmain : Int\nmain = get(map(double, [5, 6]), 1)"
  assert fails "generic map runs -> 12"
    (evalsTo mapProg "12")
  assert fails "map emits specialization and LIST_LENGTH"
    (hasFunction mapProg "map$double" && hasAll mapProg [ "\"LIST_LENGTH\"" ])
  assert fails "generic foldl runs -> 10"
    (evalsTo
       (m "add : Int -> Int -> Int\nadd a b = a + b\nmain : Int\nmain = foldl(add, 0, [1, 2, 3, 4])")
       "10")
  assert fails "map then foldl runs -> 12"
    (evalsTo
       (m "double : Int -> Int\ndouble n = n * 2\nadd : Int -> Int -> Int\nadd a b = a + b\nmain : Int\nmain = foldl(add, 0, map(double, [1, 2, 3]))")
       "12")
  assert fails "generic filter runs -> 6"
    (evalsTo
       (m "isPos : Int -> Bool\nisPos n = n > 0\nadd : Int -> Int -> Int\nadd a b = a + b\nmain : Int\nmain = foldl(add, 0, filter(isPos, [-1, 2, -3, 4]))")
       "6")
  assert fails "length intrinsic wins over user declaration"
    (evalsTo (m "length : Int -> Int\nlength n = 999\nmain : Int\nmain = length([1, 2])") "2")

  -- Standard library: list / option / math helpers (written in Verdict, tree-shaken).
  log "  -- stdlib helpers --"
  assert fails "range + sum -> 10" (evalsTo (m "main : Int\nmain = sum(range(5))") "10")
  assert fails "reverse head -> 3" (evalsTo (m "main : Int\nmain = get(reverse([1, 2, 3]), 0)") "3")
  assert fails "concat sum -> 10" (evalsTo (m "main : Int\nmain = sum(concat([1, 2], [3, 4]))") "10")
  assert fails "product -> 24" (evalsTo (m "main : Int\nmain = product([1, 2, 3, 4])") "24")
  assert fails "contains true" (evalsTo (m "main : Bool\nmain = contains(2, [1, 2, 3])") "true")
  assert fails "contains false" (evalsTo (m "main : Bool\nmain = contains(9, [1, 2, 3])") "false")
  assert fails "isEmpty true" (evalsTo (m "main : Bool\nmain = isEmpty([])") "true")
  assert fails "take sum -> 3" (evalsTo (m "main : Int\nmain = sum(take(2, [1, 2, 3, 4]))") "3")
  assert fails "drop sum -> 7" (evalsTo (m "main : Int\nmain = sum(drop(2, [1, 2, 3, 4]))") "7")
  assert fails "mapOption + withDefault -> 10"
    (evalsTo (m "double : Int -> Int\ndouble n = n * 2\nmain : Int\nmain = withDefault(0, mapOption(double, Some(5)))") "10")
  assert fails "isNone of None -> true" (evalsTo (m "main : Bool\nmain = isNone(None)") "true")
  assert fails "andThen Some"
    (evalsTo (m "halfIfEven : Int -> Option Int\nhalfIfEven n = if mod(n, 2) == 0 then Some(n / 2) else None\nmain : Int\nmain = withDefault(0, andThen(halfIfEven, Some(8)))") "4")
  assert fails "andThen None"
    (evalsTo (m "halfIfEven : Int -> Option Int\nhalfIfEven n = if mod(n, 2) == 0 then Some(n / 2) else None\nmain : Int\nmain = withDefault(0, andThen(halfIfEven, None))") "0")
  assert fails "orElse keeps Some"
    (evalsTo (m "main : Int\nmain = withDefault(0, orElse(Some(9), Some(4)))") "4")
  assert fails "orElse uses fallback"
    (evalsTo (m "main : Int\nmain = withDefault(0, orElse(Some(9), None))") "9")
  assert fails "isOk true"
    (evalsTo (m "main : Bool\nmain = isOk(Ok(7))") "true")
  assert fails "isOk false"
    (evalsTo (m "main : Bool\nmain = isOk(Err(\"bad\"))") "false")
  assert fails "okOr Ok"
    (evalsTo (m "main : Int\nmain = okOr(0, Ok(7))") "7")
  assert fails "okOr Err"
    (evalsTo (m "main : Int\nmain = okOr(0, Err(\"bad\"))") "0")
  assert fails "mapResult Ok"
    (evalsTo (m "double : Int -> Int\ndouble n = n * 2\nmain : Int\nmain = okOr(0, mapResult(double, Ok(7)))") "14")
  assert fails "mapResult Err"
    (evalsTo (m "double : Int -> Int\ndouble n = n * 2\nmain : Int\nmain = okOr(0, mapResult(double, Err(\"bad\")))") "0")
  assert fails "max -> 7" (evalsTo (m "main : Int\nmain = max(3, 7)") "7")
  assert fails "abs negative -> 5" (evalsTo (m "main : Int\nmain = abs(0 - 5)") "5")
  assert fails "clamp above -> 10" (evalsTo (m "main : Int\nmain = clamp(0, 10, 15)") "10")
  assert fails "clamp below -> 0" (evalsTo (m "main : Int\nmain = clamp(0, 10, 0 - 3)") "0")
  assert fails "all true"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Bool\nmain = all(isPos, [1, 2, 3])") "true")
  assert fails "all false"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Bool\nmain = all(isPos, [1, 0 - 1, 3])") "false")
  assert fails "any true"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Bool\nmain = any(isPos, [0 - 1, 2, 0 - 3])") "true")
  assert fails "any false"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Bool\nmain = any(isPos, [0 - 1, 0 - 2])") "false")
  assert fails "count positives"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Int\nmain = count(isPos, [0 - 1, 2, 3])") "2")
  assert fails "find present"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Int\nmain = withDefault(0, find(isPos, [0 - 1, 5, 6]))") "5")
  assert fails "find missing"
    (evalsTo (m "isPos : Int -> Bool\nisPos n = n > 0\nmain : Int\nmain = withDefault(0, find(isPos, [0 - 1, 0 - 2]))") "0")
  assert fails "flatMap"
    (evalsTo (m "pair : Int -> List Int\npair n = [n, n + 10]\nmain : Int\nmain = sum(flatMap(pair, [1, 2]))") "26")
  assert fails "replicate"
    (evalsTo (m "main : Int\nmain = sum(replicate(3, 4))") "12")
  assert fails "head Some"
    (evalsTo (m "main : Int\nmain = withDefault(0, head([7, 8]))") "7")
  assert fails "head None"
    (evalsTo (m "main : Int\nmain = withDefault(0, head(drop(1, [7])))") "0")
  assert fails "last Some"
    (evalsTo (m "main : Int\nmain = withDefault(0, last([7, 8]))") "8")
  assert fails "last None"
    (evalsTo (m "main : Int\nmain = withDefault(0, last(drop(1, [7])))") "0")

  -- Standard library: string helpers (str.* FFI builtins).
  log "  -- string helpers --"
  assert fails "strLength -> 5" (evalsTo (m "main : Int\nmain = strLength(\"hello\")") "5")
  assert fails "strConcat -> \"abcd\""
    (evalsTo (m "main : String\nmain = strConcat(\"ab\", \"cd\")") "\"abcd\"")
  assert fails "indexOf -> 2" (evalsTo (m "main : Int\nmain = indexOf(\"hello\", \"ll\")") "2")
  assert fails "indexOf absent -> -1" (evalsTo (m "main : Int\nmain = indexOf(\"hello\", \"z\")") "-1")
  assert fails "strContains true" (evalsTo (m "main : Bool\nmain = strContains(\"hello\", \"ell\")") "true")
  assert fails "strContains false" (evalsTo (m "main : Bool\nmain = strContains(\"hello\", \"xyz\")") "false")
  assert fails "strSlice length -> 3"
    (evalsTo (m "main : Int\nmain = strLength(strSlice(\"hello\", 1, 3))") "3")
  assert fails "split length -> 3"
    (evalsTo (m "main : Int\nmain = length(split(\"a,b,c\", \",\"))") "3")
  assert fails "toUpper indexOf -> 1"
    (evalsTo (m "main : Int\nmain = indexOf(toUpper(\"abc\"), \"B\")") "1")
  assert fails "trim length -> 2"
    (evalsTo (m "main : Int\nmain = strLength(trim(\"  hi  \"))") "2")
  assert fails "fromInt length -> 5"
    (evalsTo (m "main : Int\nmain = strLength(fromInt(12345))") "5")
  assert fails "replace indexOf -> 1"
    (evalsTo (m "main : Int\nmain = indexOf(replace(\"aXbXc\", \"X\", \"-\"), \"-\")") "1")
  assert fails "parseInt valid -> 42"
    (evalsTo (m "main : Int\nmain = withDefault(0, parseInt(\"42\"))") "42")
  assert fails "parseInt invalid -> 0"
    (evalsTo (m "main : Int\nmain = withDefault(0, parseInt(\"nope\"))") "0")
  assert fails "join strings"
    (evalsTo (m "main : String\nmain = join(\",\", [\"a\", \"b\", \"c\"])") "\"a,b,c\"")
  assert fails "join empty"
    (evalsTo (m "main : String\nmain = join(\",\", [])") "\"\"")
  assert fails "startsWith true"
    (evalsTo (m "main : Bool\nmain = startsWith(\"abcdef\", \"abc\")") "true")
  assert fails "startsWith false"
    (evalsTo (m "main : Bool\nmain = startsWith(\"abcdef\", \"bcd\")") "false")
  assert fails "endsWith true"
    (evalsTo (m "main : Bool\nmain = endsWith(\"abcdef\", \"def\")") "true")
  assert fails "endsWith false"
    (evalsTo (m "main : Bool\nmain = endsWith(\"abcdef\", \"abc\")") "false")
  assert fails "repeat string"
    (evalsTo (m "main : String\nmain = repeat(3, \"ab\")") "\"ababab\"")
  assert fails "string program infers [str] capability"
    (capsOf (m "main : Int\nmain = strLength(\"x\")") == [ "str" ])

  -- Standard library: regex helpers (regex.* FFI builtins).
  log "  -- regex helpers --"
  assert fails "regexTest matches digits"
    (evalsTo (m "main : Bool\nmain = regexTest(\"[0-9]+\", \"abc123\")") "true")
  assert fails "regexFindAll extracts words"
    (evalsTo (m "main : String\nmain = regexFindAll(\"[a-z]+\", \"a1bc22\")[1]") "\"bc\"")
  assert fails "regexReplace replaces all matches"
    (evalsTo (m "main : String\nmain = regexReplace(\"[0-9]+\", \"#\", \"a12b3\")") "\"a#b#\"")
  assert fails "regexSplit splits on pattern"
    (evalsTo (m "main : Int\nmain = length(regexSplit(\"[, ]+\", \"a, b c\"))") "3")
  assert fails "invalid regex is a safe no-match"
    (evalsTo (m "main : Bool\nmain = regexTest(\"[\", \"abc\")") "false")
  assert fails "regexFindAll no matches -> empty list"
    (evalsTo (m "main : Int\nmain = length(regexFindAll(\"[0-9]+\", \"abc\"))") "0")
  assert fails "regexFindAll invalid pattern -> empty list"
    (evalsTo (m "main : Int\nmain = length(regexFindAll(\"[\", \"abc\"))") "0")
  assert fails "regexReplace invalid pattern returns original"
    (evalsTo (m "main : String\nmain = regexReplace(\"[\", \"x\", \"abc\")") "\"abc\"")
  assert fails "regexSplit invalid pattern returns whole input"
    (evalsTo (m "main : String\nmain = regexSplit(\"[\", \"abc\")[0]") "\"abc\"")
  assert fails "regexSplit collapses repeated separators"
    (evalsTo (m "main : String\nmain = regexSplit(\"-+\", \"a---b-c\")[1]") "\"b\"")
  assert fails "regex program infers [regex] capability"
    (capsOf (m "main : Bool\nmain = regexTest(\"[0-9]+\", \"7\")") == [ "regex" ])

  -- Standard library: Elm-style JSON decoders / encoders.
  log "  -- json decoders and encoders --"
  assert fails "json field decoder -> 30"
    (evalsTo
       (m "main : Int\nmain =\n  match jsonDecodeString(jsonField(\"age\", jsonIntDecoder), \"{\\\"age\\\":30}\") { Ok n -> n, Err e -> 0 }")
       "30")
  assert fails "json list decoder + sum -> 6"
    (evalsTo
       (m "main : Int\nmain =\n  match jsonDecodeString(jsonListDecoder(jsonIntDecoder), \"[1,2,3]\") { Ok xs -> sum(xs), Err e -> 0 }")
       "6")
  assert fails "json nullable decoder maps null to None"
    (evalsTo
       (m "main : Int\nmain =\n  match jsonDecodeString(jsonField(\"name\", jsonNullable(jsonStringDecoder)), \"{\\\"name\\\":null}\") { Ok o -> match o { Some s -> 1, None -> 0 }, Err e -> 9 }")
       "0")
  assert fails "jsonDecodeValue value decoder"
    (evalsTo
       (m "main : Int\nmain =\n  let v = jsonObject([jsonPair(\"age\", jsonEncodeValue(jsonIntEncoder, 44))]) in\n  match jsonDecodeValue(jsonField(\"age\", jsonIntDecoder), v) { Ok n -> n, Err e -> 0 }")
       "44")
  assert fails "jsonValueDecoder re-encodes dynamic value"
    (evalsTo
       (m "main : Int\nmain = strLength(jsonEncodeString(jsonValueEncoder, okOr(jsonNull, jsonDecodeString(jsonValueDecoder, \"{\\\"age\\\":31}\"))))")
       "10")
  assert fails "jsonStringDecoder"
    (evalsTo
       (m "main : Int\nmain =\n  match jsonDecodeString(jsonStringDecoder, \"\\\"Ada\\\"\") { Ok s -> strLength(s), Err e -> 0 }")
       "3")
  assert fails "jsonBoolDecoder"
    (evalsTo
       (m "main : Int\nmain =\n  match jsonDecodeString(jsonBoolDecoder, \"true\") { Ok b -> if b then 1 else 0, Err e -> 0 }")
       "1")
  assert fails "json decoder errors are Result Err"
    (evalsTo
       (m "main : Bool\nmain =\n  match jsonDecodeString(jsonField(\"age\", jsonIntDecoder), \"{\\\"name\\\":\\\"Ada\\\"}\") { Ok n -> False, Err e -> strLength(e) > 0 }")
       "true")
  assert fails "json string encoder stringifies"
    (evalsTo
       (m "main : String\nmain = jsonEncodeString(jsonStringEncoder, \"Ada\")")
       "\"\\\"Ada\\\"\"")
  assert fails "json bool encoder stringifies"
    (evalsTo
       (m "main : String\nmain = jsonEncodeString(jsonBoolEncoder, True)")
       "\"true\"")
  assert fails "json value encoder stringifies object"
    (evalsTo
       (m "main : Int\nmain = strLength(jsonEncodeString(jsonValueEncoder, jsonObject([jsonPair(\"age\", jsonEncodeValue(jsonIntEncoder, 30))])))")
       "10")
  assert fails "json nullable encoder Some"
    (evalsTo
       (m "main : String\nmain = jsonEncodeString(jsonNullableEncoder(jsonIntEncoder), Some(9))")
       "\"9\"")
  assert fails "json nullable encoder None"
    (evalsTo
       (m "main : String\nmain = jsonEncodeString(jsonNullableEncoder(jsonIntEncoder), None)")
       "\"null\"")
  assert fails "jsonNull is dynamic null"
    (evalsTo
       (m "main : Int\nmain = match jsonDecodeValue(jsonNullable(jsonIntDecoder), jsonNull) { Ok o -> match o { Some n -> n, None -> 0 }, Err e -> 9 }")
       "0")
  assert fails "json list encoder stringifies"
    (evalsTo
       (m "main : String\nmain = jsonEncodeString(jsonListEncoder(jsonIntEncoder), [1, 2, 3])")
       "\"[1,2,3]\"")
  assert fails "jsonObject builds dynamic field values"
    (evalsTo
       (m "main : Int\nmain = jsonObject([jsonPair(\"age\", jsonEncodeValue(jsonIntEncoder, 30))]).age")
       "30")
  assert fails "json program infers [json] capability"
    (capsOf (m "main : Int\nmain = match jsonDecodeString(jsonIntDecoder, \"1\") { Ok n -> n, Err e -> 0 }") == [ "json" ])

  -- Standard library: HTTP / sys I/O / data processing / advanced math.
  log "  -- platform and data libraries --"
  assert fails "httpGet lowers to EFFECT_REQUEST + input result"
    (hasEffectProtocol (m "main : Int\nmain = httpGet(\"https://example.test/data\").status") "http.get")
  assert fails "httpPost lowers a record payload"
    (hasEffectProtocol (m "main : Int\nmain = strLength(httpPost(\"https://example.test\", \"payload\").body)") "http.post"
      && hasAll (m "main : Int\nmain = strLength(httpPost(\"https://example.test\", \"payload\").body)") [ "\"url\"", "\"body\"" ])
  assert fails "http program infers [http] capability"
    (capsOf (m "main : Int\nmain = httpGet(\"https://example.test\").status") == [ "http" ])
  assert fails "sys write/read lowers to ordered effect requests"
    (hasAll
       (m "main : Int\nmain =\n  let _ = sysWriteText(\"/tmp/a.txt\", \"hello\") in\n  strLength(withDefault(\"\", sysReadText(\"/tmp/a.txt\")))")
       [ "sys.writeText", "sys.readText", "__effect.result.sysWriteText.0", "__effect.result.sysReadText.0" ])
  assert fails "sysEnv lowers to EFFECT_REQUEST"
    (hasEffectProtocol (m "main : Int\nmain = strLength(withDefault(\"\", sysEnv(\"VERDICT\")))") "sys.env")
  assert fails "sysCwd infers [sys] capability"
    (capsOf (m "main : String\nmain = sysCwd") == [ "sys" ])
  -- Extensible FFI: `effect("ns.fn@v", ...)` lowers to the async protocol for ANY
  -- namespace the compiler has never heard of (here "telegram") — add effectful
  -- FFI with just a Verdict wrapper + a host handler, no compiler/VM release.
  let customEffect = m
        ( "tgSend : String -> String -> Bool\n"
            <> "tgSend chat text = effect(\"telegram.send@1\", chat, text)\n"
            <> "main : Int\nmain = if tgSend(\"c\", \"hi\") then 1 else 0"
        )
  assert fails "custom effect() lowers to async protocol for an unknown namespace"
    (hasEffectProtocol customEffect "telegram.send")
  assert fails "custom effect() auto-infers its capability namespace"
    (capsOf customEffect == [ "telegram" ])
  assert fails "builtin() stays a pure CALL_BUILTIN (not an effect)"
    (hasAll (m "main : Int\nmain = builtin(\"math.gcd@1\", 12, 8)") [ "\"CALL_BUILTIN\"", "math.gcd@1" ]
      && hasNone (m "main : Int\nmain = builtin(\"math.gcd@1\", 12, 8)") [ "\"EFFECT_AWAIT\"" ])
  -- Effects EXECUTE on the reference VM (the editor's runtime): EFFECT_AWAIT is
  -- fulfilled deterministically through the in-memory mocks, so effectful
  -- programs run end to end, not just emit the right opcodes.
  assert fails "httpGet effect runs on the reference VM -> 200"
    (evalsTo (m "main : Int\nmain = httpGet(\"https://x\").status") "200")
  assert fails "db insert/get round-trips through effects -> 30"
    (evalsTo (m "main : Int\nmain =\n  let id = dbInsert(\"users\", { age = 30 }) in\n  dbGet(\"users\", id).age") "30")
  assert fails "sysLog effect runs and continues"
    (evalsTo (m "main : Int\nmain = let _ = sysLog(\"hi\") in 7") "7")
  assert fails "cache set/get round-trips through effects -> 5"
    (evalsTo (m "main : Int\nmain =\n  let _ = cacheSet(\"ns\", \"k\", 5) in\n  cacheGet(\"ns\", \"k\")") "5")
  assert fails "sortInts orders bigint ints"
    (evalsTo (m "main : Int\nmain = sortInts([30, 10, 20])[0]") "10")
  assert fails "distinctInts + sumIntsFast"
    (evalsTo (m "main : Int\nmain = sumIntsFast(distinctInts([2, 2, 3, 3, 5]))") "10")
  assert fails "averageFloor"
    (evalsTo (m "main : Int\nmain = averageFloor([2, 4, 5])") "3")
  assert fails "data program infers [data] capability"
    (capsOf (m "main : Int\nmain = sumIntsFast([1, 2])") == [ "data" ])
  assert fails "stats min/max"
    (evalsTo (m "main : Int\nmain = statsMin([5, 1, 9]) + statsMax([5, 1, 9])") "10")
  assert fails "stats min/max with negatives"
    (evalsTo (m "main : Int\nmain = statsMin([0 - 10, 5, 0 - 3]) + statsMax([0 - 10, 5, 0 - 3])") "-5")
  assert fails "stats empty min defaults to 0"
    (evalsTo (m "main : Int\nmain = statsMin([])") "0")
  assert fails "stats empty max defaults to 0"
    (evalsTo (m "main : Int\nmain = statsMax([])") "0")
  assert fails "meanFloor"
    (evalsTo (m "main : Int\nmain = meanFloor([1, 2, 4])") "2")
  assert fails "meanFloor rounds toward negative infinity"
    (evalsTo (m "main : Int\nmain = meanFloor([0 - 1, 0 - 2])") "-2")
  assert fails "meanFloor empty -> 0"
    (evalsTo (m "main : Int\nmain = meanFloor([])") "0")
  assert fails "median odd"
    (evalsTo (m "main : Int\nmain = median([5, 1, 2])") "2")
  assert fails "median even floors average"
    (evalsTo (m "main : Int\nmain = median([1, 2, 3, 4])") "2")
  assert fails "median empty -> 0"
    (evalsTo (m "main : Int\nmain = median([])") "0")
  assert fails "percentileNearest"
    (evalsTo (m "main : Int\nmain = percentileNearest(50, [10, 20, 30, 40])") "20")
  assert fails "percentileNearest clamps below 0"
    (evalsTo (m "main : Int\nmain = percentileNearest(0 - 50, [10, 20, 30])") "10")
  assert fails "percentileNearest clamps above 100"
    (evalsTo (m "main : Int\nmain = percentileNearest(150, [10, 20, 30])") "30")
  assert fails "percentileNearest high percentile"
    (evalsTo (m "main : Int\nmain = percentileNearest(90, [10, 20, 30, 40, 50])") "50")
  assert fails "varianceFloor population"
    (evalsTo (m "main : Int\nmain = varianceFloor([2, 4, 4, 4, 5, 5, 7, 9])") "4")
  assert fails "varianceFloor with negatives"
    (evalsTo (m "main : Int\nmain = varianceFloor([0 - 1, 1])") "1")
  assert fails "varianceFloor empty -> 0"
    (evalsTo (m "main : Int\nmain = varianceFloor([])") "0")
  assert fails "stddevFloor population"
    (evalsTo (m "main : Int\nmain = stddevFloor([2, 4, 4, 4, 5, 5, 7, 9])") "2")
  assert fails "stddevFloor empty -> 0"
    (evalsTo (m "main : Int\nmain = stddevFloor([])") "0")
  assert fails "describeInts stddev"
    (evalsTo (m "main : Int\nmain = describeInts([2, 4, 4, 4, 5, 5, 7, 9]).stddev") "2")
  assert fails "describeInts empty count"
    (evalsTo (m "main : Int\nmain = describeInts([]).count") "0")
  assert fails "describeInts bigint sum"
    (evalsTo (m "main : Int\nmain = describeInts([100000000000000000000, 2]).sum") "100000000000000000002")
  assert fails "valueCountsInts counts sorted buckets"
    (evalsTo (m "main : Int\nmain = valueCountsInts([2, 2, 3, 2])[0].count") "3")
  assert fails "valueCountsInts sorts negative buckets"
    (evalsTo (m "main : Int\nmain = valueCountsInts([2, 0 - 1, 0 - 1])[0].value") "-1")
  assert fails "valueCountsInts empty -> empty list"
    (evalsTo (m "main : Int\nmain = length(valueCountsInts([]))") "0")
  assert fails "rollingSumInts"
    (evalsTo (m "main : Int\nmain = rollingSumInts(3, [1, 2, 3, 4])[1]") "9")
  assert fails "rollingSumInts window 1 returns original values"
    (evalsTo (m "main : Int\nmain = rollingSumInts(1, [5, 6])[1]") "6")
  assert fails "rollingSumInts zero window -> empty list"
    (evalsTo (m "main : Int\nmain = length(rollingSumInts(0, [1, 2, 3]))") "0")
  assert fails "rollingSumInts oversized window -> empty list"
    (evalsTo (m "main : Int\nmain = length(rollingSumInts(4, [1, 2, 3]))") "0")
  assert fails "stats program infers [stats] capability"
    (capsOf (m "main : Int\nmain = median([3, 1, 2])") == [ "stats" ])

  -- Standard library: technical-analysis / time-series indicators.
  log "  -- time-series indicators --"
  assert fails "sma trend" (evalsTo (m "main : Int\nmain = sma([1, 2, 3, 4], 2)[3]") "3")
  assert fails "ema trend" (evalsTo (m "main : Int\nmain = ema([1, 2, 3, 4], 2)[3]") "3")
  assert fails "wma trend" (evalsTo (m "main : Int\nmain = wma([1, 2, 3, 4], 2)[3]") "3")
  assert fails "rollingMedian trend" (evalsTo (m "main : Int\nmain = rollingMedian([1, 5, 2, 4], 3)[3]") "4")
  assert fails "momentum" (evalsTo (m "main : Int\nmain = momentum([10, 12, 15], 1)[2]") "3")
  assert fails "roc" (evalsTo (m "main : Int\nmain = roc([10, 15, 20], 1)[2]") "33")
  assert fails "rsi" (evalsTo (m "main : Int\nmain = rsi([1, 2, 3, 4], 2)[3]") "100")
  assert fails "macd" (evalsTo (m "main : Int\nmain = macd([1, 2, 3, 4], 2, 3)[3]") "0")
  assert fails "macdSignal" (evalsTo (m "main : Int\nmain = macdSignal([1, 2, 3, 4], 2, 3, 2)[3]") "0")
  assert fails "macdHistogram" (evalsTo (m "main : Int\nmain = macdHistogram([1, 2, 3, 4], 2, 3, 2)[3]") "0")
  assert fails "slope" (evalsTo (m "main : Int\nmain = slope([1, 3, 5], 3)[2]") "2")
  assert fails "rollingStd" (evalsTo (m "main : Int\nmain = rollingStd([1, 3, 5], 3)[2]") "1")
  assert fails "realizedVol" (evalsTo (m "main : Int\nmain = realizedVol([100, 150, 300], 2)[2]") "25")
  assert fails "ewmStd" (evalsTo (m "main : Int\nmain = ewmStd([10, 20, 30], 2)[2]") "6")
  assert fails "stdevRatio" (evalsTo (m "main : Int\nmain = stdevRatio([1, 2, 4, 8], 2, 3)[3]") "100")
  assert fails "atrApprox close-only" (evalsTo (m "main : Int\nmain = atrApprox([10, 13, 15], 2)[2]") "2")
  assert fails "bollingerUpper" (evalsTo (m "main : Int\nmain = bollingerUpper([1, 3, 5], 3, 2)[2]") "5")
  assert fails "bollingerLower" (evalsTo (m "main : Int\nmain = bollingerLower([1, 3, 5], 3, 2)[2]") "1")
  assert fails "zscore" (evalsTo (m "main : Int\nmain = zscore([1, 3, 5], 3)[2]") "200")
  assert fails "percentileRank" (evalsTo (m "main : Int\nmain = percentileRank([2, 4, 3], 3)[2]") "66")
  assert fails "drawdown" (evalsTo (m "main : Int\nmain = drawdown([10, 12, 9, 13])[2]") "-3")
  assert fails "pctChange" (evalsTo (m "main : Int\nmain = pctChange([10, 15, 30], 2)[2]") "200")
  assert fails "ratio" (evalsTo (m "main : Int\nmain = ratio([10, 20], [2, 5])[1]") "400")
  assert fails "spread" (evalsTo (m "main : Int\nmain = spread([10, 20], [2, 5])[1]") "15")
  assert fails "rollingCorr" (evalsTo (m "main : Int\nmain = rollingCorr([1, 2, 3], [2, 4, 6], 3)[2]") "100")
  assert fails "rollingBeta" (evalsTo (m "main : Int\nmain = rollingBeta([2, 4, 6], [1, 2, 3], 3)[2]") "200")
  assert fails "relativeMomentum" (evalsTo (m "main : Int\nmain = relativeMomentum([1, 4, 9], [1, 3, 6], 1)[2]") "2")
  assert fails "hedgeRatio" (evalsTo (m "main : Int\nmain = hedgeRatio([2, 4, 6], [1, 2, 3], 3)[2]") "200")
  assert fails "series add" (evalsTo (m "main : Int\nmain = add([1, 2], [3, 4])[1]") "6")
  assert fails "series sub" (evalsTo (m "main : Int\nmain = sub([1, 2], [3, 4])[1]") "-2")
  assert fails "series mul" (evalsTo (m "main : Int\nmain = mul([2, 3], [4, 5])[1]") "15")
  assert fails "series div" (evalsTo (m "main : Int\nmain = div([8, 9], [2, 4])[1]") "2")
  assert fails "seriesAbs" (evalsTo (m "main : Int\nmain = seriesAbs([0 - 2, 3])[0]") "2")
  assert fails "clip" (evalsTo (m "main : Int\nmain = clip([1, 5, 9], 2, 6)[2]") "6")
  assert fails "shift" (evalsTo (m "main : Int\nmain = shift([1, 2, 3], 1)[2]") "2")
  assert fails "diff" (evalsTo (m "main : Int\nmain = diff([1, 4, 9])[2]") "5")
  assert fails "series log" (evalsTo (m "main : Int\nmain = log([1, 10])[1]") "2")
  assert fails "rollingMax" (evalsTo (m "main : Int\nmain = rollingMax([1, 5, 3], 2)[2]") "5")
  assert fails "rollingMin" (evalsTo (m "main : Int\nmain = rollingMin([1, 5, 3], 2)[2]") "3")
  assert fails "cummax" (evalsTo (m "main : Int\nmain = cummax([1, 5, 3])[2]") "5")
  assert fails "cummin" (evalsTo (m "main : Int\nmain = cummin([5, 3, 4])[2]") "3")
  assert fails "crossover" (evalsTo (m "main : Int\nmain = crossover([1, 3], [2, 2])[1]") "1")
  assert fails "crossunder" (evalsTo (m "main : Int\nmain = crossunder([3, 1], [2, 2])[1]") "1")
  assert fails "trueRange" (evalsTo (m "main : Int\nmain = trueRange([12, 14], [9, 11], [10, 13])[1]") "4")
  assert fails "atrOhlc" (evalsTo (m "main : Int\nmain = atrOhlc([12, 14], [9, 11], [10, 13], 2)[1]") "3")
  assert fails "vwap" (evalsTo (m "main : Int\nmain = vwap([10, 20], [1, 3], 2)[1]") "17")
  assert fails "obv" (evalsTo (m "main : Int\nmain = obv([10, 12, 11], [5, 7, 3])[2]") "4")
  assert fails "volumeSma" (evalsTo (m "main : Int\nmain = volumeSma([2, 4, 6], 2)[2]") "5")
  assert fails "volumeRatio" (evalsTo (m "main : Int\nmain = volumeRatio([2, 4, 6], 2)[2]") "120")
  assert fails "bodySize" (evalsTo (m "main : Int\nmain = bodySize([10, 15], [12, 11])[1]") "4")
  assert fails "upperWick" (evalsTo (m "main : Int\nmain = upperWick([15], [10], [12])[0]") "3")
  assert fails "lowerWick" (evalsTo (m "main : Int\nmain = lowerWick([8], [10], [12])[0]") "2")
  assert fails "rangePct" (evalsTo (m "main : Int\nmain = rangePct([15], [10])[0]") "50")
  assert fails "series program infers [series] capability"
    (capsOf (m "main : Int\nmain = sma([1, 2, 3], 2)[2]") == [ "series" ])
  assert fails "gcd -> 6"
    (evalsTo (m "main : Int\nmain = gcd(54, 24)") "6")
  assert fails "lcm -> 42"
    (evalsTo (m "main : Int\nmain = lcm(21, 6)") "42")
  assert fails "pow -> 1024"
    (evalsTo (m "main : Int\nmain = pow(2, 10)") "1024")
  assert fails "sqrtFloor -> 12"
    (evalsTo (m "main : Int\nmain = sqrtFloor(150)") "12")
  assert fails "math program infers [math] capability"
    (capsOf (m "main : Int\nmain = gcd(54, 24)") == [ "math" ])

  -- Multi-file modules: imports merge a flat namespace; exports are validated.
  log "  -- multi-file modules --"
  let
    utilMod = "module Util exposing (triple)\ntriple : Int -> Int\ntriple n = n * 3\nhidden : Int -> Int\nhidden n = n\n"
    mainMod = "module Main exposing (main)\nimport Util exposing (triple)\nmain : Int\nmain = triple(7)\n"
  assert fails "imports a function from another module -> 21"
    (evalsProjectTo [ Tuple "Main" mainMod, Tuple "Util" utilMod ] "Main" "21")
  assert fails "importing a non-exported name is an error"
    (projectErrors
       [ Tuple "Main" "module Main exposing (main)\nimport Util exposing (hidden)\nmain : Int\nmain = hidden(7)\n"
       , Tuple "Util" utilMod
       ] "Main")
  assert fails "importing a missing module is an error"
    (projectErrors
       [ Tuple "Main" "module Main exposing (main)\nimport Nope exposing (x)\nmain : Int\nmain = 1\n" ] "Main")
  assert fails "duplicate definition across modules is an error"
    (projectErrors
       [ Tuple "Main" "module Main exposing (main)\nimport Util exposing (triple)\ntriple : Int -> Int\ntriple n = n\nmain : Int\nmain = triple(7)\n"
       , Tuple "Util" utilMod
       ] "Main")
  assert fails "record + field access -> RECORD_SET/GET"
    (hasAll
       (m "mk : Int -> { v : Int }\nmk n = { v = n }\nmain : Int\nmain = mk(3).v")
       [ "\"RECORD_NEW\"", "\"RECORD_SET\"", "\"RECORD_GET\"" ])
  let shapeProg = m
        ( "type Shape =\n"
            <> "  | Circle Int\n"
            <> "  | Rect Int Int\n"
            <> "  | Origin\n"
            <> "area : Shape -> Int\n"
            <> "area s = match s { Circle r -> r, Rect w h -> w * h, Origin -> 0 }\n"
            <> "main : Int\n"
            <> "main = area(Rect(3, 4))"
        )
  assert fails "sum type constructor + match runs -> 12"
    (evalsTo shapeProg "12")
  assert fails "sum types lower to tagged records"
    (hasAll shapeProg [ "\"RECORD_NEW\"", "\"RECORD_SET\"", "\"RECORD_GET\"", "$tag", "Rect" ])
  assert fails "nullary sum constructor matches"
    (evalsTo
       (m
          ( "type Shape =\n"
              <> "  | Circle Int\n"
              <> "  | Origin\n"
              <> "area : Shape -> Int\n"
              <> "area s = match s { Circle r -> r, Origin -> 0 }\n"
              <> "main : Int\n"
              <> "main = area(Origin)"
          ))
       "0")
  assert fails "match wildcard default runs"
    (evalsTo
       (m
          ( "type MaybeInt =\n"
              <> "  | Just Int\n"
              <> "  | Nothing\n"
              <> "fromMaybe : MaybeInt -> Int\n"
              <> "fromMaybe m = match m { Just x -> x, _ -> 7 }\n"
              <> "main : Int\n"
              <> "main = fromMaybe(Nothing)"
          ))
       "7")
  assert fails "match requires exhaustive arms"
    (isError
       (m
          ( "type Shape =\n"
              <> "  | Circle Int\n"
              <> "  | Origin\n"
              <> "area : Shape -> Int\n"
              <> "area s = match s { Circle r -> r }\n"
              <> "main : Int\n"
              <> "main = area(Origin)"
          )))
  assert fails "builtin(...) -> CALL_BUILTIN"
    (hasAll
       (m "main : Int\nmain = builtin(\"logic.and@1\", 1, 0)")
       [ "\"CALL_BUILTIN\"", "logic.and@1" ])
  assert fails "static call -> CALL"
    (hasAll
       (m "g : Int -> Int\ng x = x\nmain : Int\nmain = g(5)")
       [ "\"CALL\"" ])
  assert fails "switch -> EQ + JUMP_IF_FALSE chain"
    (hasAll
       (m "f : Int -> Int\nf n = switch n { 1 -> 10, _ -> 0 }\nmain : Int\nmain = f(1)")
       [ "\"EQ\"", "\"JUMP_IF_FALSE\"" ])
  assert fails "switch requires a default arm"
    (isError (m "main : Int\nmain = switch 1 { 1 -> 10 }"))

  -- compileJS shape.
  assert fails "compileJS ok on success"
    (compileJS (m "main : Int\nmain = 1")).ok
  assert fails "compileJS not ok on error"
    (not (compileJS "garbage").ok)

  -- Type / signature errors.
  assert fails "rejects 1 + True" (isError (m "main : Int\nmain = 1 + True"))
  assert fails "type error reports position of True in 1 + True"
    (errContains (m "main : Int\nmain = 1 + True") "3:12")
  assert fails "rejects unknown name" (isError (m "main : Int\nmain = nope"))
  assert fails "unknown name reports source position"
    (errContains (m "main : Int\nmain = nope") "3:8")
  assert fails "position wrappers are transparent for valid programs"
    (isRight (compile (m "main : Int\nmain = let x = 2 in x + 3")))
  assert fails "requires top-level signature"
    (isError "module M exposing (a)\na = 1\n")
  assert fails "rejects call arity mismatch"
    (isError (m "f : Int -> Int\nf x = x\nmain : Int\nmain = f(1, 2)"))
  assert fails "rejects field on non-record"
    (isError (m "main : Int\nmain = (1).v"))
  assert fails "rejects mismatched if branches"
    (isError (m "main : Int\nmain = if 1 > 0 then 1 else \"x\""))

  -- Execution: compile, then run on the reference VM and check the value.
  log "  -- reference-VM execution --"
  assert fails "runs folded arithmetic -> 46"
    (evalsTo (m "main : Int\nmain = 40 + 2 * 3") "46")
  assert fails "runs BigInt multiply exactly"
    (evalsTo (m "main : Int\nmain = 2 * 100000000000000000000") "200000000000000000000")
  assert fails "runs integer division (RoundDown)"
    (evalsTo (m "main : Int\nmain = 43 / 4") "10")
  assert fails "runs if/comparison -> 10"
    (evalsTo (m "main : Int\nmain = if 3 > 1 then 10 else 20") "10")
  assert fails "runs let -> 14"
    (evalsTo (m "main : Int\nmain = let x = 7 in x + x") "14")
  assert fails "runs a function call -> 49"
    (evalsTo (m "sq : Int -> Int\nsq x = x * x\nmain : Int\nmain = sq(7)") "49")
  assert fails "runs switch -> 20"
    (evalsTo (m "f : Int -> Int\nf n = switch n { 1 -> 10, 2 -> 20, _ -> 0 }\nmain : Int\nmain = f(2)") "20")
  assert fails "runs record build + field access -> 3"
    (evalsTo (m "mk : Int -> { v : Int }\nmk n = { v = n }\nmain : Int\nmain = mk(3).v") "3")
  assert fails "runs logic.and builtin -> 0"
    (evalsTo (m "main : Int\nmain = if builtin(\"logic.and@1\", True, False) then 1 else 0") "0")
  assert fails "runs bigint.modPow builtin -> 445 (4^13 mod 497)"
    (evalsTo (m "main : Int\nmain = builtin(\"bigint.modPow@1\", 4, 13, 497)") "445")
  assert fails "runs bigint.modInv builtin -> 4"
    (evalsTo (m "main : Int\nmain = builtin(\"bigint.modInv@1\", 3, 11)") "4")
  assert fails "direct db builtins lower to effects, not CALL_BUILTIN"
    (hasAll
       (m "main : Int\nmain =\n  let id = builtin(\"db.insert@1\", \"users\", { age = 30 }) in\n  builtin(\"db.get@1\", \"users\", id).age")
       [ "db.insert", "db.get", "\"EFFECT_AWAIT\"", "\"VARIANT_PAYLOAD\"" ]
      && hasNone
           (m "main : Int\nmain =\n  let id = builtin(\"db.insert@1\", \"users\", { age = 30 }) in\n  builtin(\"db.get@1\", \"users\", id).age")
           [ "db.insert@1", "db.get@1" ])

  -- Standard library (prelude wrappers + tree-shaking + capabilities).
  log "  -- standard library --"
  let dbProg = m "main : Int\nmain =\n  let id = dbInsert(\"users\", { age = 30 }) in\n  dbGet(\"users\", id).age"
  assert fails "dbInsert/dbGet wrappers lower to effects"
    (hasAll dbProg [ "db.insert", "db.get", "__effect.result.dbInsert.0", "__effect.result.dbGet.0" ])
  assert fails "db program infers capabilities = [db]" (capsOf dbProg == [ "db" ])
  assert fails "dbUpdate wrapper lowers to db.update intent"
    (hasEffectProtocol (m "main : Int\nmain = if dbUpdate(\"users\", \"missing\", { age = 31 }) then 1 else 0") "db.update")
  assert fails "dbDelete wrapper lowers to db.delete intent"
    (hasEffectProtocol (m "main : Int\nmain =\n  let id = dbInsert(\"users\", { age = 30 }) in\n  let _ = dbDelete(\"users\", id) in\n  if dbGet(\"users\", id) == unit then 1 else 0") "db.delete")
  assert fails "dbQuery wrapper lowers to db.query intent"
    (hasEffectProtocol (m "main : Int\nmain = length(dbQuery(\"users\", { age = 30 }))") "db.query")
  assert fails "dbCreateIndex wrapper lowers to db.createIndex intent"
    (hasEffectProtocol (m "main : Unit\nmain = dbCreateIndex(\"users\", \"age\")") "db.createIndex")
  assert fails "dbHash wrapper lowers to db.hash intent"
    (hasEffectProtocol (m "main : Bool\nmain = strContains(dbHash(\"users\"), \"age=30\")") "db.hash")
  assert fails "cacheSet/cacheGet lower to cache effects"
    (hasAll
       (m "main : Int\nmain =\n  let _ = cacheSet(\"ns\", \"k\", { value = 42 }) in\n  cacheGet(\"ns\", \"k\").value")
       [ "cache.set", "cache.get", "__effect.result.cacheSet.0", "__effect.result.cacheGet.0" ])
  assert fails "cacheDelete lowers to cache.delete intent"
    (hasEffectProtocol (m "main : Int\nmain = if cacheDelete(\"ns\", \"k\") then 1 else 0") "cache.delete")
  assert fails "sysLog lowers to sys.log intent"
    (hasEffectProtocol (m "main : Unit\nmain = sysLog(\"hello\")") "sys.log")
  assert fails "dbGetOpt uses db.get effect result"
    (hasEffectProtocol (m "main : Int\nmain = match dbGetOpt(\"users\", \"no-such-id\") { Some r -> 1, None -> 0 }") "db.get")
  assert fails "user can match the generic prelude Option type"
    (evalsTo (m "main : Int\nmain = match Some(1) { Some v -> v, None -> 0 }") "1")

  -- Parametric polymorphism (generics): type variables in functions resolve at
  -- call sites, and generic data types carry their payload type into `match`.
  log "  -- generics --"
  assert fails "generic identity resolves its result type -> 6"
    (evalsTo (m "identity : a -> a\nidentity x = x\nmain : Int\nmain = identity(5) + 1") "6")
  assert fails "generic identity on a different type -> True"
    (evalsTo (m "identity : a -> a\nidentity x = x\nmain : Bool\nmain = identity(True)") "true")
  assert fails "rigid type variables: a -> b body x is rejected"
    (isError (m "bad : a -> b\nbad x = x\nmain : Int\nmain = bad(5)"))
  assert fails "generic data type round-trips through match -> 7"
    (evalsTo (m "type Box a = Box a\nunbox : Box a -> a\nunbox b = match b { Box v -> v }\nmain : Int\nmain = unbox(Box(7))") "7")
  assert fails "match binds the instantiated payload type -> 6"
    (evalsTo (m "type Box a = Box a\nmain : Int\nmain = match Box(5) { Box n -> n + 1 }") "6")
  assert fails "generic withDefault on present Option -> 9"
    (evalsTo (m "main : Int\nmain = withDefault(0, Some(9))") "9")
  assert fails "generic withDefault on None falls back -> 0"
    (evalsTo (m "main : Int\nmain = withDefault(0, None)") "0")
  assert fails "modPow wrapper runs -> 445" (evalsTo (m "main : Int\nmain = modPow(4, 13, 497)") "445")
  assert fails "modPow program infers capabilities = [bigint]"
    (capsOf (m "main : Int\nmain = modPow(4, 13, 497)") == [ "bigint" ])
  assert fails "modInv wrapper runs -> 4" (evalsTo (m "main : Int\nmain = modInv(3, 11)") "4")
  assert fails "modInv non-invertible -> 0" (evalsTo (m "main : Int\nmain = modInv(6, 9)") "0")
  let pureProg = m "main : Int\nmain = 2 + 3"
  assert fails "pure program has no capabilities" (capsOf pureProg == [])
  assert fails "tree-shaking: pure program pulls in zero prelude functions"
    (funcCount pureProg == 1)
  assert fails "tree-shaking: db program pulls in only the used wrappers"
    (funcCount dbProg == 3)
  assert fails "showcase example runs -> 59"
    (evalsTo showcaseSource "59")

  -- Cooperative process primitives: spawn/send/recv/yield run under the
  -- deterministic reference-VM scheduler.
  log "  -- concurrency --"
  requestReplyExample <- readExample "request_reply"
  fanInExample <- readExample "fan_in"
  assert fails "examples/request_reply.verdict runs -> 42"
    (evalsTo requestReplyExample "42")
  assert fails "examples/fan_in.verdict compiles cache rendezvous as effects"
    (hasAll fanInExample [ "cache.get", "cache.set", "\"EFFECT_AWAIT\"" ])
  assert fails "example programs emit process opcodes"
    (hasAll requestReplyExample [ "\"PROC_SELF\"", "\"PROC_SPAWN\"", "\"PROC_SEND\"", "\"PROC_RECEIVE\"" ]
      && hasAll fanInExample [ "\"PROC_SPAWN\"", "\"PROC_SEND\"", "\"PROC_RECEIVE\"", "\"PROC_YIELD\"" ])
  let procPass = m
        ( "worker : Pid -> Unit\n"
            <> "worker replyTo =\n"
            <> "  let msg = recv() in\n"
            <> "  let _ = send(replyTo, { value = msg.value }) in\n"
            <> "  unit\n"
            <> "main : Int\n"
            <> "main =\n"
            <> "  let me = self() in\n"
            <> "  let p = spawn(worker, me) in\n"
            <> "  let _ = send(p, { value = 37 }) in\n"
            <> "  let _ = yield() in\n"
            <> "  let reply = recv() in\n"
            <> "  reply.value"
        )
  assert fails "spawn/send/recv/yield pass a record between processes -> 37"
    (evalsTo procPass "37")
  assert fails "spawn-only worker is kept alive"
    (hasFunction procPass "worker")
  let requestReply = m
        ( "doubler : Int -> Unit\n"
            <> "doubler s =\n"
            <> "  let req = recv() in\n"
            <> "  let _ = send(req.reply_to, { value = req.n * 2 }) in\n"
            <> "  doubler(s)\n"
            <> "main : Int\n"
            <> "main =\n"
            <> "  let me = self() in\n"
            <> "  let srv = spawn(doubler, 0) in\n"
            <> "  let _ = send(srv, { reply_to = me, n = 21 }) in\n"
            <> "  let reply = recv() in\n"
            <> "  reply.value"
        )
  assert fails "self enables request/reply -> 42"
    (evalsTo requestReply "42")
  assert fails "request/reply emits self and process ops"
    (hasAll requestReply [ "\"PROC_SELF\"", "\"PROC_SPAWN\"", "\"PROC_SEND\"", "\"PROC_RECEIVE\"" ])
  let oneShotReply = m
        ( "worker : Unit\n"
            <> "worker =\n"
            <> "  let req = recv() in\n"
            <> "  let _ = send(req.reply_to, { value = req.value }) in\n"
            <> "  unit\n"
            <> "main : Int\n"
            <> "main =\n"
            <> "  let me = self() in\n"
            <> "  let p = spawn(worker) in\n"
            <> "  let _ = send(p, { reply_to = me, value = 19 }) in\n"
            <> "  let reply = recv() in\n"
            <> "  reply.value"
        )
  assert fails "one-shot worker replies with requested field -> 19"
    (evalsTo oneShotReply "19")
  let fanIn = m
        ( "worker : Pid -> Int -> Unit\n"
            <> "worker boss n =\n"
            <> "  let _ = send(boss, { value = n * n }) in\n"
            <> "  unit\n"
            <> "main : Int\n"
            <> "main =\n"
            <> "  let me = self() in\n"
            <> "  let _ = spawn(worker, me, 1) in\n"
            <> "  let _ = spawn(worker, me, 2) in\n"
            <> "  let _ = spawn(worker, me, 3) in\n"
            <> "  let _ = yield() in\n"
            <> "  let a = recv() in\n"
            <> "  let b = recv() in\n"
            <> "  let c = recv() in\n"
            <> "  a.value + b.value + c.value"
        )
  assert fails "fan-in workers sum through collector -> 14"
    (evalsTo fanIn "14")
  assert fails "process opcodes are emitted"
    (hasAll fanIn [ "\"PROC_SPAWN\"", "\"PROC_SEND\"", "\"PROC_RECEIVE\"", "\"PROC_YIELD\"" ])

  log "  -- actor framework --"
  let actorCounter = m
        ( "type CounterMsg = Add Int | Get Pid\n"
            <> "counterHandle : CounterMsg -> Int -> { stop : Bool, state : Int }\n"
            <> "counterHandle msg state = match msg {\n"
            <> "  Add n -> actorContinue(state + n),\n"
            <> "  Get replyTo -> let _ = send(replyTo, { value = state }) in actorContinue(state)\n"
            <> "}\n"
            <> "counter : Int -> Unit\n"
            <> "counter state = actorLoop(counterHandle, state)\n"
            <> "makeGet : Pid -> CounterMsg\n"
            <> "makeGet replyTo = Get(replyTo)\n"
            <> "main : Int\n"
            <> "main =\n"
            <> "  let c = actorStart(counter, 0) in\n"
            <> "  let _ = actorSend(c, Add(4)) in\n"
            <> "  let _ = actorSend(c, Add(5)) in\n"
            <> "  let reply = actorCall(c, makeGet) in\n"
            <> "  reply.value"
        )
  assert fails "actorStart/actorSend/actorCall counter -> 9"
    (evalsTo actorCounter "9")
  assert fails "actor framework emits process opcodes"
    (hasAll actorCounter [ "\"PROC_SPAWN\"", "\"PROC_SEND\"", "\"PROC_RECEIVE\"", "\"TAIL_CALL\"" ])
  assert fails "actorStart keeps spawned actor and handler alive"
    (hasFunction actorCounter "counter" && hasFunction actorCounter "counterHandle")
  let actorStop = m
        ( "type OnceMsg = Once Pid\n"
            <> "onceHandle : OnceMsg -> Int -> { stop : Bool, state : Int }\n"
            <> "onceHandle msg state = match msg {\n"
            <> "  Once replyTo -> let _ = send(replyTo, { value = state }) in actorStop(state)\n"
            <> "}\n"
            <> "once : Int -> Unit\n"
            <> "once state = actorLoop(onceHandle, state)\n"
            <> "makeOnce : Pid -> OnceMsg\n"
            <> "makeOnce replyTo = Once(replyTo)\n"
            <> "main : Int\n"
            <> "main =\n"
            <> "  let a = actorStart(once, 7) in\n"
            <> "  let reply = actorCall(a, makeOnce) in\n"
            <> "  reply.value"
        )
  assert fails "actorLoop ActorStop exits after reply"
    (evalsTo actorStop "7")
  assert fails "actorSelf wraps current pid"
    (evalsTo
       (m "main : Unit\nmain =\n  let me = actorSelf(unit) in\n  actorReply(me, { ok = True })")
       "unit")
  assert fails "actorSend rejects wrong message type for typed ActorRef"
    (isError
       (m
          ( "type Msg = Ping\n"
              <> "bad : ActorRef Msg -> Unit\n"
              <> "bad ref = actorSend(ref, \"not a message\")\n"
              <> "main : Unit\nmain = bad(actorSelf(unit))"
          )))
  -- Async effects compose with actors: an actor handler that does I/O lowers to
  -- the async protocol (EFFECT_AWAIT suspends only that actor) and the loop stays
  -- tail-recursive — so siblings keep running while one actor awaits.
  let actorIo = m
        ( "type Msg = Fetch String Pid\n"
            <> "handle : Msg -> Int -> { stop : Bool, state : Int }\n"
            <> "handle msg state = match msg {\n"
            <> "  Fetch url replyTo ->\n"
            <> "    let body = httpGet(url).body in\n"
            <> "    let _ = send(replyTo, { body = body }) in actorContinue(state + 1)\n"
            <> "}\n"
            <> "server : Int -> Unit\nserver state = actorLoop(handle, state)\n"
            <> "main : Int\nmain = let s = actorStart(server, 0) in 1"
        )
  assert fails "actor handler can do async I/O (EFFECT_AWAIT, no whole-VM block)"
    (hasAll actorIo [ "\"EFFECT_AWAIT\"", "\"VARIANT_PAYLOAD\"", "\"TAIL_CALL\"", "\"PROC_SPAWN\"" ]
      && hasNone actorIo [ "\"EFFECT_REQUEST\"", "\"LOAD_INPUT\"" ])

  n <- Ref.read fails
  if n == 0 then log "\nAll tests passed."
  else do
    error ("\n" <> show n <> " test(s) failed.")
    setExitCode 1

-- Check that every registerCount in the output JSON is <= bound.
countMaxRegLE :: String -> Int -> Boolean
countMaxRegLE src bound =
  let
    o = replaceAll (Pattern " ") (Replacement "") (out src)
    nums = map leadingInt (drop 1 (split (Pattern "\"registerCount\":") o))
  in
    all (\v -> v <= bound) nums

leadingInt :: String -> Int
leadingInt s =
  fromMaybe 0 (Int.fromString (SCU.takeWhile (\c -> c >= '0' && c <= '9') s))
