#!/usr/bin/env node
// Run a Verdict program on the reference VM and print a conformance-friendly
// result envelope:
//   {"status":"completed","result":<tagless JSON>}
//   {"status":"error","result":null,"error":"..."}
//
// Usage:
//   verdictrun <file.verdict>              run a file (resolving `import`s by name)
//   verdictrun --inputs <file.json> <file.verdict>
//   verdictrun                               run stdin (single module, no imports)
//
// When a program declares `input` parameters, supply runtime values as a JSON
// object mapping input names to tagless FinVM values, e.g.
//   {"threshold":{"int":"41"}}
//
// If `<stem>.inputs.json` sits beside `<stem>.verdict`, it is loaded automatically.
//
// Run `npm run build` first so output/ exists.
import { readFileSync, existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import { loadProject } from './project.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const { runJS, runProjectJS } = await import(
  resolve(here, '../output/Verdict.Compiler/index.js')
);

const argv = process.argv.slice(2);
let inputsPath = null;
let file = null;

for (let i = 0; i < argv.length; i += 1) {
  if (argv[i] === '--inputs') {
    inputsPath = argv[i + 1];
    i += 1;
  } else if (!file) {
    file = argv[i];
  }
}

const inputValues = loadInputValues(inputsPath, file);
let result;

if (!file) {
  result = runJS(readFileSync(0, 'utf8'))(inputValues);
} else {
  try {
    const { modules, entryName } = loadProject(file);
    result = runProjectJS(modules)(entryName)(inputValues);
  } catch (e) {
    result = { ok: false, error: e.message };
  }
}

if (result.ok) {
  process.stdout.write(JSON.stringify({
    status: 'completed',
    result: parseRunOutput(result.output)
  }, null, 2) + '\n');
} else {
  process.stdout.write(JSON.stringify({
    status: 'error',
    result: null,
    error: result.error
  }, null, 2) + '\n');
  process.exit(1);
}

function loadInputValues(explicitPath, verdictPath) {
  const path = explicitPath
    ?? (verdictPath ? autoInputPath(verdictPath) : null);
  if (!path) return {};
  return JSON.parse(readFileSync(path, 'utf8'));
}

function autoInputPath(verdictPath) {
  const sidecar = verdictPath.replace(/\.verdict$/, '.inputs.json');
  return existsSync(sidecar) ? sidecar : null;
}

function parseRunOutput(output) {
  try {
    return JSON.parse(output);
  } catch {
    return output;
  }
}
