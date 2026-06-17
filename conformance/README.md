# Verdict / FinVM Conformance Corpus

Each file in `conformance/cases` is a standalone Verdict program. The
conformance runner compiles each case to FinVM JSON, runs it on a real FinVM
provided by `FINVM_CMD`, runs the same source on Verdict's reference VM oracle,
and deep-compares the tagless result JSON.

## Cases

- `int_bigint.verdict`: exact arbitrary-precision integer arithmetic.
- `div_floor_neg.verdict`: negative integer division is floored, not truncated.
- `mod_floor_neg.verdict`: modulo follows the same floored division identity.
- `fixed_scale.verdict`: fixed-point addition preserves aligned scale.
- `rational_reduce.verdict`: rational constants fold exactly and reduce.
- `lists_hof.verdict`: list indexing plus monomorphized `map`/`filter`/`foldl`.
- `records_fields.verdict`: record construction and field projection.
- `sum_match.verdict`: generic sum type construction and exhaustive `match`.
- `generics_identity.verdict`: type-variable instantiation at call sites.
- `tail_deep.verdict`: deep tail recursion uses `TAIL_CALL` stack-safely.
- `option_db.verdict`: `dbGetOpt` relies on `db.get@1` returning `unit` on miss.
- `proc_request_reply.verdict`: `PROC_SELF`, request/reply, and process mailboxes.
- `proc_fan_in.verdict`: order-insensitive fan-in with process sends and cache
  rendezvous.
- `str_numeric.verdict`: `str.*` numeric results (`strLength`, `indexOf`,
  `parseInt`).
- `str_text.verdict`: `str.*` string results (`toUpper`, `replace`, `strConcat`,
  `trim`) — probes `VString` encoding parity.
- `inputs.verdict`: typed `input` declarations fulfilled via `input.get@1` and a
  sidecar `inputs.inputs.json` runtime value map.

## Running

```sh
npm run build
FINVM_CMD="finvm run" node conformance/run.mjs
```

Reference-VM only (no FinVM host required):

```sh
npm run test:conformance:oracle
```

`FINVM_CMD` is invoked with the compiled program JSON path appended. The real
FinVM stdout may contain logs; the runner compares the `result` field from the
last JSON object it can parse.
