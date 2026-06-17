#!/usr/bin/env node
import { mkdtempSync, readdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, join, resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('..', import.meta.url));
const casesDir = join(root, 'conformance', 'cases');
const finvmCmd = process.env.FINVM_CMD;
const oracleOnly =
  process.env.ORACLE_ONLY === '1' || process.argv.includes('--oracle-only');

if (!finvmCmd && !oracleOnly) {
  console.error('Usage: FINVM_CMD="finvm run" node conformance/run.mjs');
  console.error('       ORACLE_ONLY=1 node conformance/run.mjs');
  console.error('The runner appends the compiled program JSON path to FINVM_CMD.');
  process.exit(1);
}

const tmp = mkdtempSync(join(tmpdir(), 'verdict-conformance-'));
let failures = 0;

try {
  const cases = readdirSync(casesDir)
    .filter((name) => name.endsWith('.verdict'))
    .sort();

  console.log(
    oracleOnly
      ? 'case\tstatus\toracle result'
      : 'case\tstatus\tfinvm result\toracle result'
  );

  for (const name of cases) {
    const srcPath = join(casesDir, name);
    const label = basename(name, '.verdict');

    try {
      const oracle = runNode(['bin/verdictrun.mjs', srcPath]);
      const oracleJson = parseLastJsonObject(oracle.stdout);
      const oracleResult = oracleJson.result;

      if (oracleOnly) {
        const ok = oracleJson.status === 'completed';
        if (!ok) failures += 1;
        printOracleRow(label, ok ? 'PASS' : 'FAIL', oracleResult);
        continue;
      }

      const jsonPath = join(tmp, name.replace(/\.verdict$/, '.json'));
      const compiled = runNode(['bin/verdictc.mjs', srcPath]);
      writeFileSync(jsonPath, compiled.stdout);

      const finvm = runShell(`${finvmCmd} ${shellQuote(jsonPath)}`);
      const finvmJson = parseLastJsonObject(finvm.stdout);
      const finvmResult = finvmJson.result;

      const ok = deepEqualJson(finvmResult, oracleResult);
      if (!ok) failures += 1;
      printRow(label, ok ? 'PASS' : 'FAIL', finvmResult, oracleResult);
    } catch (err) {
      failures += 1;
      if (oracleOnly) {
        printOracleRow(label, 'FAIL', String(err.message || err));
      } else {
        printRow(label, 'FAIL', String(err.message || err), null);
      }
    }
  }
} finally {
  rmSync(tmp, { recursive: true, force: true });
}

if (failures > 0) {
  console.error(`\n${failures} conformance case(s) failed.`);
  process.exit(1);
}

console.log('\nAll conformance cases passed.');

function runNode(args) {
  const result = spawnSync(process.execPath, args, {
    cwd: root,
    encoding: 'utf8'
  });
  if (result.status !== 0) {
    throw new Error((result.stderr || result.stdout || `node ${args.join(' ')} failed`).trim());
  }
  return result;
}

function runShell(command) {
  const result = spawnSync(command, {
    cwd: root,
    shell: true,
    encoding: 'utf8'
  });
  if (result.status !== 0) {
    throw new Error((result.stderr || result.stdout || `${command} failed`).trim());
  }
  return result;
}

function printRow(label, status, finvmResult, oracleResult) {
  console.log([
    label,
    status,
    compact(finvmResult),
    compact(oracleResult)
  ].join('\t'));
}

function printOracleRow(label, status, oracleResult) {
  console.log([label, status, compact(oracleResult)].join('\t'));
}

function compact(value) {
  if (typeof value === 'string') return value;
  return JSON.stringify(value);
}

function deepEqualJson(a, b) {
  return JSON.stringify(canonical(a)) === JSON.stringify(canonical(b));
}

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map((k) => [k, canonical(value[k])]));
  }
  return value;
}

function parseLastJsonObject(text) {
  const objects = [];
  let start = -1;
  let depth = 0;
  let inString = false;
  let escaped = false;

  for (let i = 0; i < text.length; i += 1) {
    const ch = text[i];
    if (inString) {
      if (escaped) escaped = false;
      else if (ch === '\\') escaped = true;
      else if (ch === '"') inString = false;
      continue;
    }
    if (ch === '"') {
      inString = true;
      continue;
    }
    if (ch === '{') {
      if (depth === 0) start = i;
      depth += 1;
      continue;
    }
    if (ch === '}') {
      depth -= 1;
      if (depth === 0 && start >= 0) {
        const candidate = text.slice(start, i + 1);
        try {
          objects.push(JSON.parse(candidate));
        } catch {
          // Ignore braces in log text that do not form JSON.
        }
        start = -1;
      }
    }
  }

  if (objects.length === 0) {
    throw new Error(`no JSON object found in output: ${text.trim()}`);
  }
  return objects[objects.length - 1];
}

function shellQuote(s) {
  return `'${s.replace(/'/g, `'\\''`)}'`;
}
