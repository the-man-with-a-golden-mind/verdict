# Verdict FinVM Effect Intent Contract

Verdict lowers effectful `builtin("name@version", ...)` calls to FinVM effect
intents. Pure builtins still lower to `CALL_BUILTIN`.

## Effectful Namespaces

Effectful namespaces are:

- `http.*`
- `db.*`
- `cache.*`
- `sys.*`

The compiler also reserves `ws.*`, `time.*`, and `random.*` for the same lowering
path.

All other current standard-library builtins are pure and remain synchronous
`CALL_BUILTIN` operations.

## Intent Shape

Each effect request is emitted as:

```json
["EFFECT_NEW", intentReg, "<type>", payloadReg]
["EFFECT_REQUEST", intentReg]
["LOAD_INPUT", resultReg, "__effect.result.<function>.<n>"]
```

`<type>` is the builtin id without the version suffix, for example
`http.get@1` becomes `http.get`.

The FinVM value created by `EFFECT_NEW` is:

```purescript
{ type_ :: String, payload :: Value }
```

For FinVM 1.0.1 host drivers, `payload` is always a record containing a string
`key` field. That `key` is the same string later read by `LOAD_INPUT`.

## Payloads

HTTP:

- `http.get@1(url)` -> `type_ = "http.get"`, `payload = { key, url }`
- `http.post@1(url, body)` -> `type_ = "http.post"`,
  `payload = { key, url, body }`

DB:

- `db.insert@1(table, record)` -> `{ key, table, record }`
- `db.get@1(table, id)` -> `{ key, table, id }`
- `db.update@1(table, id, record)` -> `{ key, table, id, record }`
- `db.delete@1(table, id)` -> `{ key, table, id }`
- `db.query@1(table, query, options)` -> `{ key, table, query, options }`
- `db.createIndex@1(table, field)` -> `{ key, table, field }`
- `db.hash@1(table)` -> `{ key, table }`

Cache:

- `cache.set@1(ns, cacheKey, value)` -> `{ key, ns, cacheKey, value }`
- `cache.get@1(ns, cacheKey)` -> `{ key, ns, cacheKey }`
- `cache.delete@1(ns, cacheKey)` -> `{ key, ns, cacheKey }`

System:

- `sys.log@1(message)` -> `{ key, message }`
- `sys.cwd@1()` -> `{ key }`
- `sys.readText@1(path)` -> `{ key, path }`
- `sys.writeText@1(path, contents)` -> `{ key, path, contents }`
- `sys.env@1(name)` -> `{ key, name }`

Reserved future effect namespaces use `{ key, args }` until a more specific
payload shape is added.

## Result Correlation

FinVM 1.0.1 `EFFECT_REQUEST` appends the intent to `machine.outbox`; it does not
define a result register or an input key convention. Verdict therefore uses a
compiler-generated monotonic request site id per lowered function.

The result key is:

```text
__effect.result.<function>.<n>
```

where `<function>` is the lowered Verdict function id and `<n>` starts at `0`
and increments in deterministic lowering order within that function. The same
string is included as `payload.key`. After the host fulfills an intent, it writes
the fulfilled value into `machine.input[payload.key]`. The value loaded from
that key is the value of the original Verdict call, so public Verdict type
signatures are unchanged.

Hosts should preserve request order when fulfilling batches and repeated
requests. `EFFECT_BATCH_NEW` and `EFFECT_BATCH_APPEND` are emitted by the
assembler when present in MIR; current Verdict stdlib wrappers issue single
requests.
