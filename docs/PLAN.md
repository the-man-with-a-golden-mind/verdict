# Verdict Plan

Verdict is no longer in the "first-order core" phase described by the original
plan. The compiler now has a usable typed surface, a tree-shaken standard
library, whole-program monomorphization for higher-order functions, tail-call
optimization, source-positioned type errors, actor-style concurrency over FinVM
process primitives, a reference VM, and a MIR optimizer pipeline that is
validated by value-level tests.

## Done

- Reference VM: `Verdict.VM.Eval` runs emitted `ProgramVM` values with a pure,
  trampolined interpreter and deterministic `db.*`, `cache.*`, `logic.*`, and
  `bigint.*` builtins.
- FinVM JSON output: tagless values and positional instruction arrays, with
  capabilities inferred from reachable builtins.
- Standard library: auto-injected and tree-shaken `Prelude` wrappers for logic,
  bigint, db/cache, generic `Option`/`Result`, `dbGetOpt`, and generic
  `map`/`filter`/`foldl`.
- Type system: `Int`, `Fixed`, compile-time `Rational`, `Bool`, `String`,
  `Unit`, `Json`, lists, records, arrows, type variables, and generic data
  types.
- Sum types: constructors and exhaustive `match`, lowered to tagged records
  using `$tag`, `$0`, `$1`, and so on. No variant opcodes were added to FinVM.
- Higher-order functions: whole-program monomorphization specializes named
  function arguments away before lowering. The VM still has no runtime function
  values.
- Recursion: tail-call peephole emits FinVM `TAIL_CALL`, and the reference VM
  trampolines deep recursion.
- Concurrency: `spawn`, `send`, `recv`, `yield`, and `self` lower to FinVM
  `PROC_*` instructions. Actors are ordinary top-level functions, pids are
  opaque `Pid` values, and the reference VM uses deterministic FIFO scheduling
  with string pids (`"main"`, then `"p0"`, `"p1"`, and so on).
- List ergonomics: `length(xs)`, `get(xs, i)`, `append(xs, x)`, and indexing
  sugar `xs[i]`.
- Numeric ergonomics: BigInt arithmetic, Fixed and Rational constant folding,
  floored integer division, and integer `mod(a, b)`.
- MIR optimizer: constant folding, strength reduction, branch elimination,
  nullary inlining, CSE, DCE, liveness register allocation, and tail-call
  rewriting.

## Current Shape

The design intentionally keeps the VM model small. Generic functions share one
boxed VM body unless they are higher-order, in which case monomorphization
creates first-order specializations such as `map$double`. Sum types erase to
records, and `Rational` values can only enter the VM as canonical constants.

Tests compile programs, run the actual emitted `ProgramVM` on the reference VM,
and assert serialized result values. Shape tests remain where they protect a
wire-format or optimizer contract.

For concurrency, portable Verdict programs should avoid depending on message
interleaving order. The reference VM is deterministic, but Verdict does not pin
the real FinVM scheduler's order.

## Roadmap / Not Yet

- String manipulation: concatenation, slicing, length, search, and conversion
  helpers are not yet part of the Verdict surface.
- Multi-file modules and imports.
- Denser switch lowering, such as jump tables for dense integer cases.

## Near-Term Priorities

1. Design a minimal string builtin surface and deterministic reference-VM
   semantics.
2. Split module handling into explicit imports while preserving tree-shaking and
   capability inference from the entry point.
3. Add denser `switch` lowering for dense integer cases.
