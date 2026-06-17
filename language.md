# Verdict Language Summary For AI Agents

Verdict is a small Elm-like language that compiles to FinVM bytecode JSON. It is
purely expression-oriented, statically checked, and uses a tree-shaken prelude.

Use this file as the compact source of truth when generating Verdict programs.

## File Shape

Every file starts with a module header:

```verdict
module Main exposing (main)
```

Top-level functions require signatures:

```verdict
double : Int -> Int
double n = n * 2

main : Int
main = double(21)
```

Programs may declare typed run-time inputs before functions:

```verdict
input signalThreshold : Int
input assetsCsv : String

main : Int
main = signalThreshold + strLength(assetsCsv)
```

Input names are in scope as values of their declared type. The compiler lowers
reads to `input.get@1("name")` and emits an `inputs.schema` block in the
program JSON (no run-time values — the host supplies those separately).

Optional inputs declare a literal default:

```verdict
input pageSize : Int = 50
```

These emit `"required": false` and an `inputs.defaults` map (program-defined
constants, not encrypted run-time payloads).

For dynamic access by name (escape hatch), the prelude provides `inputGet`,
`inputInt`, `inputBool`, and `inputString` — all lower to `input.get@1`.

Function calls use parentheses and commas: `f(a, b)`, not curried call syntax.
Definitions bind parameters with spaces: `f a b = ...`.

Multi-file programs can import exposed names:

```verdict
import Util exposing (triple)
```

Imports are currently flattened into one namespace, so avoid duplicate top-level
names across modules.

## Identifiers

- Value/function identifiers are normal lower-case names such as `foo`, `fooBar`.
- Constructors and type names are uppercase such as `Some`, `Result`.
- Identifiers are alphanumeric only; do not use underscores.
- Use camelCase names: `macdSignal`, `rollingStd`, `jsonDecodeString`.

## Types

Supported surface types:

- `Int`: arbitrary-precision integer payloads.
- `Fixed`: decimal fixed-point literal, e.g. `1.50`.
- `Rational`: literal form `1 % 2`; mainly compile-time folded.
- `Bool`: `True`, `False`.
- `String`.
- `Unit`: value literal `unit`.
- `Pid`: opaque process id from `spawn`/`self`.
- `Json`: dynamic FFI/DB/cache/message payload type.
- `List T`.
- Records: `{ age : Int, name : String }`.
- Arrow types: `A -> B`.
- Type variables: `a`, `b`.
- Generic data types: `type Option a = Some a | None`.

The prelude defines:

```verdict
type Option a = Some a | None
type Result e a = Err e | Ok a
type Decoder a = Decoder Json
type Encoder a = Encoder Json
```

## Expressions

Core expression forms:

```verdict
if n > 0 then 1 else 0

let x = 10 in x + 1

switch n { 0 -> 10, 1 -> 20, _ -> 0 }

match maybeAge { Some age -> age, None -> 0 }

{ age = 30, name = "Ada" }
row.age

[1, 2, 3]
xs[0]

builtin("namespace.name@1", arg1, arg2)
```

`switch` needs a `_` default. `match` over sum types must be exhaustive or have
`_ -> ...`.

Effects are sequenced with `let _ = effect in next`.

## Operators And Intrinsics

Operators:

- Arithmetic: `+`, `-`, `*`, `/`.
- Integer division is floored.
- Comparisons: `==`, `<`, `>`.
- Rational literal syntax uses `%`, e.g. `1 % 2`; `%` is not modulo.

Reserved intrinsic calls:

- `mod(a, b) : Int`
- `length(xs) : Int`
- `get(xs, i) : a`
- `append(xs, x) : List a`
- `spawn(fn, args...) : Pid`
- `actorStart(fn, args...) : ActorRef msg`
- `send(pid, msg) : Unit`
- `recv() : Json`
- `yield() : Unit`
- `self() : Pid`

List indexing `xs[i]` desugars to `get(xs, i)`.

## Higher-Order Functions

Verdict supports higher-order functions by whole-program monomorphization.
Function values are not runtime values. A function argument must be a bare
top-level function name:

```verdict
inc : Int -> Int
inc n = n + 1

main : Int
main = get(map(inc, [1, 2]), 0)
```

Do not store functions in records/lists or return them.

## Sum Types

Sum types are monomorphic or generic:

```verdict
type Shape = Circle Int | Rect Int Int | Origin

area : Shape -> Int
area s =
  match s {
    Circle r -> r * r,
    Rect w h -> w * h,
    Origin -> 0
  }
```

Constructors are called like functions: `Rect(3, 4)`. Nullary constructors can
be used as values: `Origin`.

Implementation note: variants lower to records with private fields `"$tag"`,
`"$0"`, `"$1"`, etc. No FinVM variant opcodes are required.

## Concurrency

Actors are ordinary top-level functions running as processes. The raw process
intrinsics are available, and the prelude adds a typed actor layer:

- `ActorRef msg` wraps a `Pid` with the message type it accepts.
- Actor handlers return `{ stop : Bool, state : state }`.
- `actorContinue(state)` and `actorStop(state)` build handler results.
- `actorSend(ref, msg)` sends a typed message.
- `actorCall(ref, makeMsg)` sends a request built from the caller pid and then
  receives the reply.
- `actorLoop(handle, state)` implements the standard receive/handle/recurse loop.

```verdict
type CounterMsg = Add Int | Get Pid

counterHandle : CounterMsg -> Int -> { stop : Bool, state : Int }
counterHandle msg state = match msg {
  Add n -> actorContinue(state + n),
  Get replyTo -> let _ = send(replyTo, { value = state }) in actorContinue(state)
}

counter : Int -> Unit
counter state = actorLoop(counterHandle, state)

makeGet : Pid -> CounterMsg
makeGet replyTo = Get(replyTo)

main : Int
main =
  let c = actorStart(counter, 0) in
  let _ = actorSend(c, Add(4)) in
  let _ = actorSend(c, Add(5)) in
  let reply = actorCall(c, makeGet) in
  reply.value
```

`Pid` is opaque in source. The reference VM uses deterministic string pids:
`"main"`, `"p0"`, `"p1"`, etc. The reference scheduler is FIFO and deterministic,
but real FinVM scheduling order is not specified. Prefer order-insensitive actor
protocols.

## JSON

Use Elm-style decoder/encoder recipes:

```verdict
main : Int
main =
  match jsonDecodeString(jsonField("age", jsonIntDecoder), "{\"age\":30}") {
    Ok n -> n,
    Err e -> 0
  }
```

Available JSON helpers:

- Decoders: `jsonValueDecoder`, `jsonIntDecoder`, `jsonStringDecoder`,
  `jsonBoolDecoder`, `jsonField`, `jsonListDecoder`, `jsonNullable`.
- Decode: `jsonDecodeValue`, `jsonDecodeString`.
- Encoders: `jsonValueEncoder`, `jsonIntEncoder`, `jsonStringEncoder`,
  `jsonBoolEncoder`, `jsonListEncoder`, `jsonNullableEncoder`.
- Encode: `jsonEncodeValue`, `jsonEncodeString`.
- Construction: `jsonNull`, `jsonPair`, `jsonObject`.

There is no general `Decoder.map` because functions are not runtime values.
Transform decoded values by matching `Ok`.

## Standard Library

The prelude is auto-injected and tree-shaken. Only reached wrappers are emitted.

### Logic

- `and`, `or`, `not`.

### BigInt / Math

- `modPow`, `modInv`.
- `max`, `min`, `abs`, `clamp`.
- `gcd`, `lcm`, `pow`, `sqrtFloor`.

### Lists

- `map`, `filter`, `foldl`.
- `isEmpty`, `range`, `reverse`, `concat`.
- `sum`, `product`, `contains`, `take`, `drop`.
- `all`, `any`, `count`, `find`, `flatMap`.
- `replicate`, `head`, `last`.

### Option / Result

- `mapOption`, `isNone`, `andThen`, `orElse`.
- `withDefault`, `isSome`.
- `isOk`, `okOr`, `mapResult`.

### Strings

- `strLength`, `strConcat`, `strSlice`, `indexOf`, `strContains`.
- `split`, `toUpper`, `toLower`, `trim`, `fromInt`, `replace`, `parseInt`.

### Regex

- `regexTest(pattern, input)`.
- `regexFindAll(pattern, input)`.
- `regexReplace(pattern, replacement, input)`.
- `regexSplit(pattern, input)`.

Invalid regexes fail safely in the reference VM.

### HTTP

- `httpGet(url) : { status : Int, ok : Bool, body : String }`.
- `httpPost(url, body) : { status : Int, ok : Bool, body : String }`.

The reference VM returns deterministic mock responses. Real effects depend on
the FinVM host implementing `http.*`.

### System I/O

- `sysLog(msg) : Unit`.
- `sysCwd : String`.
- `sysReadText(path) : Option String`.
- `sysWriteText(path, contents) : Bool`.
- `sysEnv(name) : Option String`.

The reference VM models files/logs/env in memory for deterministic tests.

### DB / Cache

- `dbInsert`, `dbGet`, `dbGetOpt`, `dbUpdate`, `dbDelete`, `dbQuery`,
  `dbCreateIndex`, `dbHash`.
- `cacheSet`, `cacheGet`, `cacheDelete`.

`dbGetOpt` assumes `db.get@1` returns `unit` when missing.

### Data Processing / Stats

Fast integer-column helpers:

- `sortInts`, `distinctInts`, `sumIntsFast`, `averageFloor`.
- `statsMin`, `statsMax`, `meanFloor`, `median`.
- `percentileNearest`, `varianceFloor`, `stddevFloor`.
- `describeInts`.
- `valueCountsInts`.
- `rollingSumInts`.

`describeInts(xs)` returns:

```verdict
{ count, sum, min, max, mean, median, variance, stddev }
```

### Time-Series / Technical Analysis

All technical-analysis helpers take `List Int` and return `List Int`. Feed scaled
integers, e.g. cents or basis points, when decimal precision matters.

Trend:

- `sma`, `ema`, `wma`, `rollingMedian`.

Momentum:

- `momentum`, `roc`, `rsi`, `macd`, `macdSignal`, `macdHistogram`, `slope`.

Volatility:

- `rollingStd`, `realizedVol`, `ewmStd`, `stdevRatio`.
- `atrApprox`, `bollingerUpper`, `bollingerLower`.

Stats:

- `zscore`, `percentileRank`, `drawdown`, `pctChange`.

Pair:

- `ratio`, `spread`, `rollingCorr`, `rollingBeta`.
- `relativeMomentum`, `hedgeRatio`.

Arithmetic:

- `add`, `sub`, `mul`, `div`.
- `seriesAbs`, `clip`, `shift`, `diff`, `log`.

Range:

- `rollingMax`, `rollingMin`, `cummax`, `cummin`.

Boolean crossover:

- `crossover`, `crossunder`.

OHLCV:

- `atrOhlc`, `trueRange`, `vwap`, `obv`.
- `volumeSma`, `volumeRatio`.
- `bodySize`, `upperWick`, `lowerWick`, `rangePct`.

Name caveats:

- Use `rollingMedian` for rolling series median; `median` is scalar stats.
- Use `seriesAbs` for series absolute value; `abs` is scalar `Int -> Int`.
- Use camelCase, not underscores: `macdSignal`, not `macd_signal`.

## Capabilities

Capabilities are inferred from reached builtins. A program using only pure code
has no capabilities. Useful namespaces include:

- `logic`
- `bigint`
- `str`
- `regex`
- `json`
- `math`
- `http`
- `sys`
- `data`
- `stats`
- `series`
- `db`
- `cache`

The generated FinVM JSON contains a `capabilities` array.

## Runtime / VM Model

Verdict emits FinVM JSON in tagless/positional format:

- Instructions are arrays like `["ADD", d, a, b]`.
- Constants are tagless values like `{ "int": "42" }` or
  `{ "string": "hello" }`.

The reference VM executes the actual emitted `ProgramVM`. It is deterministic
and includes in-memory models for DB, cache, sys I/O, HTTP mock responses,
process scheduling, JSON, regex, stats, and time-series builtins.

## Optimization Pipeline

Current MIR/source optimizations include:

- Constant folding for `Int`, `Fixed`, and `Rational`.
- Strength reduction.
- Branch elimination for constant conditions.
- Nullary function inlining.
- Common subexpression elimination.
- Dead-code elimination.
- Liveness-based register allocation.
- Tail-call peephole to FinVM `TAIL_CALL`.
- Whole-program monomorphization for higher-order functions.

## Common Pitfalls For Generated Code

- Always include top-level type signatures.
- Do not use underscores in identifiers.
- Do not write `foo bar`; calls are `foo(bar)`.
- Use `mod(a, b)`, not `%`.
- Use `let _ = effect in ...` to sequence effects.
- Do not construct or inspect `Pid` values manually.
- Do not store functions in records/lists or return them.
- For JSON decoders, transform after `match`ing `Ok`.
- For time-series decimals, use scaled integers.
- `Json` is dynamic; field access on runtime JSON is checked by execution, not
  statically by a precise schema.
