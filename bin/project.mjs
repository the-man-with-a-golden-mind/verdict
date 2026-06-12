// Shared multi-file loader for the Verdict CLIs. The compiler core is IO-free;
// this resolves `import Foo` to `Foo.verdict` (relative to the entry file) and
// returns a plain { moduleName -> source } map plus the entry module name.
import { readFileSync } from 'node:fs';
import { dirname, resolve, join } from 'node:path';

export const moduleNameOf = (src) => {
  const m = src.match(/^\s*module\s+([A-Za-z][A-Za-z0-9]*)/m);
  return m ? m[1] : 'Main';
};

export const importNamesOf = (src) => {
  const out = [];
  const re = /^\s*import\s+([A-Za-z][A-Za-z0-9]*)/gm;
  let m;
  while ((m = re.exec(src))) out.push(m[1]);
  return out;
};

// Throws Error with a friendly message if a referenced module file is missing.
export function loadProject(file) {
  const entrySrc = readFileSync(file, 'utf8');
  const dir = dirname(resolve(file));
  const entryName = moduleNameOf(entrySrc);
  const modules = { [entryName]: entrySrc };
  const queue = importNamesOf(entrySrc);
  while (queue.length) {
    const name = queue.shift();
    if (modules[name] !== undefined) continue;
    const path = join(dir, name + '.verdict');
    let src;
    try {
      src = readFileSync(path, 'utf8');
    } catch {
      throw new Error(`Cannot find module '${name}' (expected ${path})`);
    }
    modules[name] = src;
    for (const dep of importNamesOf(src)) {
      if (modules[dep] === undefined) queue.push(dep);
    }
  }
  return { modules, entryName };
}
