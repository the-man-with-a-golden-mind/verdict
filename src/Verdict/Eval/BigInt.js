// FFI for Verdict.Eval.BigInt. Uses the host BigInt (Node + browser).
export const addStr = a => b => (BigInt(a) + BigInt(b)).toString();
export const subStr = a => b => (BigInt(a) - BigInt(b)).toString();
export const mulStr = a => b => (BigInt(a) * BigInt(b)).toString();
export const cmpStr = a => b => {
  const x = BigInt(a), y = BigInt(b);
  return x < y ? -1 : x > y ? 1 : 0;
};
export const normalizeStr = a => BigInt(a).toString();
export const divFloorStr = a => b => {
  const x = BigInt(a), y = BigInt(b);
  let q = x / y;
  const r = x % y;
  if (r !== 0n && ((r < 0n) !== (y < 0n))) q -= 1n;
  return q.toString();
};
export const modStr = a => b => {
  const x = BigInt(a), y = BigInt(b);
  let q = x / y;
  const r = x % y;
  if (r !== 0n && ((r < 0n) !== (y < 0n))) q -= 1n;
  return (x - q * y).toString();
};
export const gcdStr = a => b => {
  let x = BigInt(a);
  let y = BigInt(b);
  if (x < 0n) x = -x;
  if (y < 0n) y = -y;
  while (y !== 0n) {
    const r = x % y;
    x = y;
    y = r;
  }
  return x.toString();
};
export const powStr = a => b => {
  const exp = BigInt(b);
  if (exp < 0n) throw new Error("pow exponent must be non-negative");
  return (BigInt(a) ** exp).toString();
};
export const sqrtFloorStr = a => {
  const n = BigInt(a);
  if (n < 0n) throw new Error("sqrtFloor input must be non-negative");
  if (n < 2n) return n.toString();
  let lo = 1n;
  let hi = n;
  let ans = 1n;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1n;
    const sq = mid * mid;
    if (sq <= n) {
      ans = mid;
      lo = mid + 1n;
    } else {
      hi = mid - 1n;
    }
  }
  return ans.toString();
};
export const modPowStr = b => e => m => {
  let mod = BigInt(m);
  if (mod === 1n) return "0";
  let base = ((BigInt(b) % mod) + mod) % mod;
  let exp = BigInt(e);
  let result = 1n;
  while (exp > 0n) {
    if (exp & 1n) result = (result * base) % mod;
    exp >>= 1n;
    base = (base * base) % mod;
  }
  return result.toString();
};
export const modInvStr = a => m => {
  const mod = BigInt(m);
  if (mod === 0n) return "0";
  let t = 0n, newT = 1n;
  let r = mod < 0n ? -mod : mod;
  let newR = ((BigInt(a) % r) + r) % r;
  while (newR !== 0n) {
    const q = r / newR;
    [t, newT] = [newT, t - q * newT];
    [r, newR] = [newR, r - q * newR];
  }
  if (r > 1n) return "0";
  if (t < 0n) t += (mod < 0n ? -mod : mod);
  return t.toString();
};
