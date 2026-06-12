#!/usr/bin/env node
// Run a Verdict program on the reference VM and print a conformance-friendly
// result envelope:
//   {"status":"completed","result":<tagless JSON>}
//   {"status":"error","result":null,"error":"..."}
//
// Usage:
//   verdictrun <file.verdict>     run a file (resolving `import`s by name)
//   verdictrun                    run stdin (single module, no imports)
//
// Run `npm run build` first so output/ exists.
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import { loadProject } from './project.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const { runJS, runProjectJS } = await import(
  resolve(here, '../output/Verdict.Compiler/index.js')
);

const file = process.argv[2];
let result;

if (!file) {
  result = runJS(readFileSync(0, 'utf8'));
} else {
  try {
    const { modules, entryName } = loadProject(file);
    result = runProjectJS(modules)(entryName); // curried PureScript fn
  } catch (e) {
    result = { ok: false, error: e.message };
  }
}

if (result.ok) {
  process.stdout.write(JSON.stringify({
    status: 'completed',
    result: JSON.parse(result.output)
  }, null, 2) + '\n');
} else {
  process.stdout.write(JSON.stringify({
    status: 'error',
    result: null,
    error: result.error
  }, null, 2) + '\n');
  process.exit(1);
}
