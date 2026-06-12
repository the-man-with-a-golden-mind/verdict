#!/usr/bin/env node
// Verdict CLI (Node). Thin wrapper over the platform-agnostic compiler core in
// output/Verdict.Compiler — the exact same module the browser bundle uses.
//
// Usage:
//   verdictc <file.verdict>     compile a file (resolving `import`s by name)
//   verdictc                    compile stdin (single module, no imports)
//
// Run `npm run build` (spago build) first so output/ exists.
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import { loadProject } from './project.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const { compileJS, compileProjectJS } = await import(
  resolve(here, '../output/Verdict.Compiler/index.js')
);

const file = process.argv[2];
let result;

if (!file) {
  result = compileJS(readFileSync(0, 'utf8'));
} else {
  try {
    const { modules, entryName } = loadProject(file);
    result = compileProjectJS(modules)(entryName); // curried PureScript fn
  } catch (e) {
    process.stderr.write(e.message + '\n');
    process.exit(1);
  }
}

if (result.ok) {
  process.stdout.write(result.output + '\n');
} else {
  process.stderr.write(result.error + '\n');
  process.exit(1);
}
