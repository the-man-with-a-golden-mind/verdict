# Verdict

Verdict is a small Elm-like language that compiles to **FinVM bytecode JSON**.
The compiler is written in PureScript and exposes the same pure core on Node and
in the browser.

```elm
module Main exposing (..)

type Option a = Some a | None

double : Int -> Int
double n = n * 2

add : Int -> Int -> Int
add a b = a + b

main : Int
main =
  foldl(add, 0, map(double, [1, 2, 3]))
```

That program emits ordinary FinVM calls and list instructions. The higher-order
call to `map(double, ...)` is specialized at compile time; there are no runtime
function values.

## What Works Today

- Modules use `module M exposing (..)` syntax, and a program may span multiple
  files via `import Foo exposing (bar)` (see Modules below).
- Top-level functions require signatures.
- The typechecker supports type variables and generic data types.
- Sum types and `match` are exhaustiveness checked.
- Higher-order functions compile through whole-program monomorphization.
- Tail recursion emits FinVM `TAIL_CALL`; the reference VM trampolines.
- Concurrency uses FinVM process primitives through intrinsic calls.
- The standard prelude is auto-injected, tree-shaken, and capability-aware.
- The emitted JSON uses the current FinVM tagless/positional wire format.

## Compiler Pipeline

```
source
  │  Parser
  │  Typecheck
  │  Monomorphize higher-order calls
  ▼
AST
  │  Lower to MIR
  ▼
MIR with virtual registers
  │  Optimize
  │  Register allocation
  ▼
MIR with physical registers
  │  Emit
  ▼
FinVM JSON
```

The source-level monomorphization pass specializes higher-order functions before
lowering. For example, `map(double, xs)` becomes a static call to a generated
function such as `map$double(xs)`. Lowering, FinVM emission, and the VM never need
closures.

## Language Reference

| Feature | Example | Notes |
|---|---|---|
| Module | `module Main exposing (..)` | One source module today. |
| Signature | `f : Int -> Int` | Required for every top-level declaration. |
| Function | `f x = x + 1` | Calls use `f(a, b)`. |
| Type variable | `id : a -> a` | Rigid inside a signature, instantiated at call sites. |
| Generic data type | `type Option a = Some a \| None` | Constructors are called like functions. |
| Match | `match o { Some x -> x, None -> 0 }` | Constructor patterns are exhaustiveness checked. |
| Switch | `switch n { 0 -> a, _ -> b }` | Value match; `_` default is required. |
| If | `if c then a else b` | Strict expression form. |
| Let | `let x = e in body` | Expression-local binding. |
| Records | `{ age = 30 }`, `row.age` | Structural records. |
| Lists | `[1, 2, 3]`, `xs[i]` | Indexing desugars to `get(xs, i)`. |
| Builtins | `builtin("db.insert@1", "users", row)` | Dynamic FinVM builtin bridge. |

### Types

Verdict currently supports:

- `Int`: arbitrary-precision integer payloads.
- `Fixed`: decimal fixed-point literals such as `1.50`.
- `Rational`: literals such as `1 % 2`; rationals are compile-time constants.
- `Bool`: `True` and `False`.
- `String`: string literals.
- `Unit`: the `unit` literal.
- `List T`.
- Records: `{ field : T }`.
- Arrow types: `A -> B`.
- Type variables: `a`, `b`, etc.
- Generic data types: `type Result e a = Err e | Ok a`.
- `Json`: dynamic type used for FFI, DB, and cache payloads.

### Operators And Intrinsics

| Operation | Example | Notes |
|---|---|---|
| Arithmetic | `+ - * /` | Works on numeric types where valid; integer `/` is floored division. |
| Comparison | `== < >` | Produces `Bool`. |
| Modulo | `mod(a, b)` | Int-only intrinsic. `%` is reserved for rational literals. |
| Length | `length(xs)` | List intrinsic. |
| Get | `get(xs, i)` / `xs[i]` | List intrinsic. |
| Append | `append(xs, x)` | Returns a new list. |
| Spawn | `spawn(worker, arg)` | Starts a top-level function as a process. |
| Send | `send(pid, { value = 1 })` | Sends a record message. |
| Receive | `recv()` | Blocks for the next message; returns `Json`. |
| Yield | `yield()` | Cooperatively yields. |
| Self | `self()` | Returns this process's `Pid`. |

## Modules

A program can span multiple files. A module exports names with its header and
pulls in others with `import`:

```elm
-- Util.verdict
module Util exposing (triple)

triple : Int -> Int
triple n = n * 3
```

```elm
-- Main.verdict
module Main exposing (main)

import Util exposing (triple)

main : Int
main = triple(7)
```

`verdictc Main.verdict` resolves `import Foo` to `Foo.verdict` in the entry
file's directory (transitively), and the compiler core stays IO-free — the CLI
reads the files and hands the core a `module-name -> source` map.

v1 model: imports merge into one flat namespace, so top-level names must be
unique across files; `import … exposing (n)` is validated against the source
module's export list; a missing module or an unexported name is a compile error.
(Qualified `Foo.bar` access and strict per-module visibility are not enforced
yet.) The prelude is still auto-injected on top of every module.

## Concurrency

Verdict exposes FinVM's process primitives as reserved intrinsic calls:

- `spawn(fn, a, b, ...) : Pid`
- `actorStart(fn, a, b, ...) : ActorRef msg`
- `send(pid, msg) : Unit`
- `recv() : Json`
- `yield() : Unit`
- `self() : Pid`

The raw primitives are still available, but the prelude also provides a small
typed actor framework:

- `ActorRef msg`: typed wrapper around an opaque `Pid`.
- Actor handlers return `{ stop : Bool, state : state }`.
- `actorContinue(state)` and `actorStop(state)` build handler results.
- `actorStart(fn, args...)`: spawn a top-level actor function and wrap its pid.
- `actorSend(ref, msg)`: send a typed message.
- `actorCall(ref, makeMsg)`: request/reply helper; `makeMsg` receives `self()`.
- `actorLoop(handle, state)`: receive, handle, and tail-recurse while continuing.
- `actorSelf(unit)`, `actorPid(ref)`, `actorReceive(unit)`, `actorReply(ref,msg)`.

An actor is still just an ordinary top-level function running as a process.
Actor services usually delegate to `actorLoop`, which stays stack-safe because
Verdict emits `TAIL_CALL`.

```elm
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

`spawn` and `actorStart` take a bare top-level function name as their first
argument; that name is checked against the function's arity and parameter types.
Effects are sequenced with `let _ = effect in next`.

Messages are records. Programs commonly tag them with a field such as `type`, or
dispatch by record shape and field access. `recv()` returns `Json`, so message
payloads are dynamic at the boundary.

`Pid` is opaque in Verdict source. In the runtime model pids are strings:
`"main"` for the root process, then `"p0"`, `"p1"`, and so on for spawned
processes. These pid values are produced only by `spawn` and `self`; user code
should not construct or inspect them.

Request/reply works by including `reply_to = self()` in a request record and then
waiting with `recv()` for the response.

**Determinism caveat:** the reference VM uses a deterministic FIFO ready queue
and FIFO mailboxes, with no clock or randomness. The real FinVM scheduling order
is not pinned by Verdict, so programs whose final result depends on message
interleaving may differ on real FinVM. For portable programs, make actor results
order-insensitive, for example with commutative accumulation or a rendezvous
through `cache`/state.

## Sum Types

FinVM has no variant opcodes, deliberately. Verdict lowers variants to tagged
records:

```elm
type Shape = Circle Int | Rect Int Int | Origin
```

A value such as `Rect(3, 4)` is encoded as a record with private fields:

- `"$tag" = "Rect"`
- `"$0" = 3`
- `"$1" = 4`

`match` lowers to `RECORD_GET`, string equality, and branches. User record fields
cannot collide with the `$` namespace because source identifiers cannot start
with `$`.

## Generics

Generics are a typechecker feature only. FinVM values are uniformly boxed, so a
generic first-order function such as:

```elm
id : a -> a
id x = x
```

uses one body for every instantiation. Generic data types are likewise erased to
their runtime tagged-record representation.

## Higher-Order Functions

Verdict supports higher-order functions under a restricted, compile-time model:
function values may be passed by bare top-level name to arrow-typed parameters.
The compiler then specializes the callee and removes the function parameter.

```elm
apply : (a -> b) -> a -> b
apply f x = f(x)

inc : Int -> Int
inc n = n + 1

main : Int
main = apply(inc, 5)
```

The emitted program contains a static specialization such as `apply$inc`. There
are no runtime closures or function values.

## Standard Prelude

`Verdict.Std.Prelude` is injected before typechecking and tree-shaken from the
entry point. It currently provides:

- Logic wrappers: `and`, `or`, `not`.
- BigInt wrappers: `modPow`, `modInv`.
- DB wrappers: `dbInsert`, `dbGet`, `dbUpdate`, `dbDelete`, `dbQuery`,
  `dbCreateIndex`, `dbHash`.
- Cache wrappers: `cacheSet`, `cacheGet`, `cacheDelete`.
- Generic data types: `Option a = Some a | None`,
  `Result e a = Err e | Ok a`.
- Option helpers: `withDefault`, `isSome`, `isNone`, `mapOption`,
  `dbGetOpt : String -> String -> Option Json`.
- Generic list functions: `map`, `filter`, `foldl`, `reverse`, `concat`, `take`,
  `drop`, `range`, `contains`, `isEmpty`, and (for `Int` lists) `sum`, `product`.
- Math helpers: `max`, `min`, `abs`, `clamp`.
- String helpers (`str.*` FFI builtins): `strLength`, `strConcat`, `strSlice`,
  `indexOf`, `strContains`, `split`, `toUpper`, `toLower`, `trim`, `fromInt`,
  `replace`, and `parseInt : String -> Option Int`.

Tree-shaking is capability-sensitive: a pure program emits no DB/cache wrappers
and has no capabilities. A program that reaches `dbInsert` or `dbGetOpt` infers
the `db` capability.

## Optimizer

The MIR optimizer currently performs:

- Constant folding for `Int`, `Fixed`, and compile-time `Rational` arithmetic.
- Strength reduction such as `x + 0`, `x * 1`, `x * 0`, `x / 1`.
- Branch elimination when conditions fold to constants.
- Nullary function inlining and removal of now-dead functions.
- Common-subexpression elimination in straight-line regions.
- Dead-code elimination.
- Liveness-based register allocation with move coalescing.
- Tail-call peephole rewriting to FinVM `TAIL_CALL`.

The tests assert both JSON shape and reference-VM result values.

## FinVM JSON

Verdict emits the compact FinVM shape:

- Values are tagless objects such as `{ "int": "42" }`, `{ "string": "hi" }`, or
  JSON `null` for `unit`.
- Instructions are positional arrays such as `["LOAD_CONST", 0, 1]`.
- Functions are stored in an object keyed by function name.
- Program JSON includes `version`, `constants`, `entrypoint`, `functions`, and
  `capabilities`.

The reference interpreter executes the actual emitted `ProgramVM`. It includes a
deterministic in-memory model for `db`, `cache`, `logic`, and `bigint` builtins,
plus a FIFO cooperative scheduler for process instructions.

## Build And Run

Prerequisites: Node, PureScript, and Spago.

```sh
npm run build
npm test

node bin/verdictc.mjs example.verdict
node run.mjs example.verdict

npm run build:web
node web/serve.mjs
```

`Verdict.Compiler` exports:

- `compile : String -> Either String String`
- `compileJson : String -> Either String Json`
- `compileProgram : String -> Either String ProgramVM`
- `compileJS : String -> { ok :: Boolean, output :: String, error :: String }`

## Roadmap / Not Yet

Done: multi-file modules/imports, required signatures, records, lists, indexing,
`Unit`, `Fixed`, compile-time `Rational`, `mod`, generic data types, sum types,
`match`, higher-order functions through monomorphization, generic
`map/filter/foldl`, a list/option/math/string standard library (`str.*` FFI
helpers), tail calls, concurrency process intrinsics, MIR optimization, source
positions in type errors, reference VM, tree-shaken prelude, capability
inference, and end-to-end conformance against the real FinVM.

Still not implemented:

- Qualified imports (`Foo.bar`) and strict per-module visibility enforcement
  (the v1 model is a flat merged namespace).
- Package management.
- Dense `switch` jump-table lowering.
