const nums = xs => xs.map(x => Number(x));
const period = p => Math.max(1, Math.trunc(Number(p)));
const out = xs => xs.map(x => {
  const n = Number.isFinite(x) ? Math.trunc(x) : 0;
  return Object.is(n, -0) ? "0" : String(n);
});
const minLen = (a, b) => Math.min(a.length, b.length);
const windowOf = (xs, i, p) => i + 1 >= p ? xs.slice(i - p + 1, i + 1) : null;
const sum = xs => xs.reduce((a, b) => a + b, 0);
const mean = xs => xs.length === 0 ? 0 : sum(xs) / xs.length;
const std = xs => {
  if (xs.length === 0) return 0;
  const m = mean(xs);
  return Math.sqrt(mean(xs.map(x => (x - m) * (x - m))));
};
const med = xs => {
  if (xs.length === 0) return 0;
  const s = [...xs].sort((a, b) => a - b);
  const mid = Math.floor(s.length / 2);
  return s.length % 2 === 1 ? s[mid] : Math.floor((s[mid - 1] + s[mid]) / 2);
};
const rolling = (src, p, f) => {
  const xs = nums(src), n = period(p);
  return out(xs.map((_, i) => {
    const w = windowOf(xs, i, n);
    return w == null ? 0 : f(w, i, xs);
  }));
};
const emaNums = (xs, p) => {
  if (xs.length === 0) return [];
  const a = 2 / (period(p) + 1);
  let e = xs[0];
  return xs.map((x, i) => {
    e = i === 0 ? x : a * x + (1 - a) * e;
    return e;
  });
};
const binary = (a, b, f) => {
  const xs = nums(a), ys = nums(b), n = minLen(xs, ys);
  const r = [];
  for (let i = 0; i < n; i++) r.push(f(xs[i], ys[i], i, xs, ys));
  return out(r);
};

export const sma = src => p => rolling(src, p, mean);
export const ema = src => p => out(emaNums(nums(src), p));
export const wma = src => p => rolling(src, p, w => {
  const den = w.length * (w.length + 1) / 2;
  return sum(w.map((x, i) => x * (i + 1))) / den;
});
export const rollingMedian = src => p => rolling(src, p, med);

export const momentum = src => p => {
  const xs = nums(src), n = period(p);
  return out(xs.map((x, i) => i >= n ? x - xs[i - n] : 0));
};
export const roc = src => p => {
  const xs = nums(src), n = period(p);
  return out(xs.map((x, i) => i >= n && xs[i - n] !== 0 ? ((x - xs[i - n]) * 100) / xs[i - n] : 0));
};
export const rsi = src => p => {
  const xs = nums(src), n = period(p);
  return out(xs.map((_, i) => {
    if (i < n) return 0;
    let gain = 0, loss = 0;
    for (let j = i - n + 1; j <= i; j++) {
      const d = xs[j] - xs[j - 1];
      if (d >= 0) gain += d; else loss -= d;
    }
    if (loss === 0) return 100;
    const rs = gain / loss;
    return 100 - (100 / (1 + rs));
  }));
};
export const macd = src => fast => slow => {
  const xs = nums(src);
  const f = emaNums(xs, fast), s = emaNums(xs, slow);
  return out(f.map((x, i) => x - s[i]));
};
export const macdSignal = src => fast => slow => sig => out(emaNums(nums(macd(src)(fast)(slow)), sig));
export const macdHistogram = src => fast => slow => sig => {
  const m = nums(macd(src)(fast)(slow)), s = emaNums(m, sig);
  return out(m.map((x, i) => x - s[i]));
};
export const slope = src => p => rolling(src, p, w => w.length <= 1 ? 0 : (w[w.length - 1] - w[0]) / (w.length - 1));

export const rollingStd = src => p => rolling(src, p, std);
export const realizedVol = src => p => rolling(roc(src)("1"), p, std);
export const ewmStd = src => p => {
  const xs = nums(src), a = 2 / (period(p) + 1);
  let m = xs[0] ?? 0, v = 0;
  return out(xs.map((x, i) => {
    if (i === 0) return 0;
    const old = m;
    m = a * x + (1 - a) * m;
    v = (1 - a) * (v + a * (x - old) * (x - old));
    return Math.sqrt(v);
  }));
};
export const stdevRatio = src => shortP => longP => {
  const s = nums(rollingStd(src)(shortP)), l = nums(rollingStd(src)(longP));
  return out(s.map((x, i) => l[i] === 0 ? 0 : (x * 100) / l[i]));
};
export const atrApprox = src => p => sma(diff(src).map(x => String(Math.abs(Number(x)))))(p);
export const bollingerUpper = src => p => nstd => {
  const m = nums(sma(src)(p)), st = nums(rollingStd(src)(p)), n = Number(nstd);
  return out(m.map((x, i) => x + n * st[i]));
};
export const bollingerLower = src => p => nstd => {
  const m = nums(sma(src)(p)), st = nums(rollingStd(src)(p)), n = Number(nstd);
  return out(m.map((x, i) => x - n * st[i]));
};

export const zscore = src => p => {
  const xs = nums(src), m = nums(sma(src)(p)), st = nums(rollingStd(src)(p));
  return out(xs.map((x, i) => st[i] === 0 ? 0 : ((x - m[i]) * 100) / st[i]));
};
export const percentileRank = src => p => rolling(src, p, w => {
  const last = w[w.length - 1];
  return (w.filter(x => x <= last).length * 100) / w.length;
});
export const drawdown = src => {
  const xs = nums(src);
  let peak = -Infinity;
  return out(xs.map(x => {
    peak = Math.max(peak, x);
    return x - peak;
  }));
};
export const pctChange = src => p => roc(src)(p);

export const ratio = a => b => binary(a, b, (x, y) => y === 0 ? 0 : (x * 100) / y);
export const spread = a => b => binary(a, b, (x, y) => x - y);
export const rollingCorr = a => b => p => {
  const xs = nums(a), ys = nums(b), n = minLen(xs, ys), per = period(p), r = [];
  for (let i = 0; i < n; i++) {
    if (i + 1 < per) { r.push(0); continue; }
    const xw = xs.slice(i - per + 1, i + 1), yw = ys.slice(i - per + 1, i + 1);
    const mx = mean(xw), my = mean(yw);
    const cov = mean(xw.map((x, j) => (x - mx) * (yw[j] - my)));
    const den = std(xw) * std(yw);
    r.push(den === 0 ? 0 : (cov / den) * 100);
  }
  return out(r);
};
export const rollingBeta = a => b => p => {
  const xs = nums(a), ys = nums(b), n = minLen(xs, ys), per = period(p), r = [];
  for (let i = 0; i < n; i++) {
    if (i + 1 < per) { r.push(0); continue; }
    const xw = xs.slice(i - per + 1, i + 1), yw = ys.slice(i - per + 1, i + 1);
    const mx = mean(xw), my = mean(yw);
    const cov = mean(xw.map((x, j) => (x - mx) * (yw[j] - my)));
    const vb = mean(yw.map(y => (y - my) * (y - my)));
    r.push(vb === 0 ? 0 : (cov / vb) * 100);
  }
  return out(r);
};
export const relativeMomentum = a => b => p => binary(momentum(a)(p), momentum(b)(p), (x, y) => x - y);
export const hedgeRatio = a => b => p => rollingBeta(a)(b)(p);

export const seriesAdd = a => b => binary(a, b, (x, y) => x + y);
export const seriesSub = a => b => binary(a, b, (x, y) => x - y);
export const seriesMul = a => b => binary(a, b, (x, y) => x * y);
export const seriesDiv = a => b => binary(a, b, (x, y) => y === 0 ? 0 : x / y);
export const seriesAbs = src => out(nums(src).map(Math.abs));
export const clip = src => lo => hi => out(nums(src).map(x => Math.max(Number(lo), Math.min(Number(hi), x))));
export const shift = src => p => {
  const xs = nums(src), n = Math.max(0, Math.trunc(Number(p)));
  return out(xs.map((_, i) => i >= n ? xs[i - n] : 0));
};
export const diff = src => {
  const xs = nums(src);
  return out(xs.map((x, i) => i === 0 ? 0 : x - xs[i - 1]));
};
export const logSeries = src => out(nums(src).map(x => x > 0 ? Math.log(x) : 0));
export const rollingMax = src => p => rolling(src, p, w => Math.max(...w));
export const rollingMin = src => p => rolling(src, p, w => Math.min(...w));
export const cummax = src => {
  let m = -Infinity;
  return out(nums(src).map(x => (m = Math.max(m, x))));
};
export const cummin = src => {
  let m = Infinity;
  return out(nums(src).map(x => (m = Math.min(m, x))));
};
export const crossover = a => b => binary(a, b, (x, y, i, xs, ys) => i > 0 && xs[i - 1] <= ys[i - 1] && x > y ? 1 : 0);
export const crossunder = a => b => binary(a, b, (x, y, i, xs, ys) => i > 0 && xs[i - 1] >= ys[i - 1] && x < y ? 1 : 0);

export const trueRange = high => low => close => {
  const h = nums(high), l = nums(low), c = nums(close), n = Math.min(h.length, l.length, c.length);
  const r = [];
  for (let i = 0; i < n; i++) {
    const prev = i === 0 ? c[i] : c[i - 1];
    r.push(Math.max(h[i] - l[i], Math.abs(h[i] - prev), Math.abs(l[i] - prev)));
  }
  return out(r);
};
export const atrOhlc = high => low => close => p => sma(trueRange(high)(low)(close))(p);
export const vwap = close => volume => p => {
  const c = nums(close), v = nums(volume), n = minLen(c, v), per = period(p), r = [];
  for (let i = 0; i < n; i++) {
    if (i + 1 < per) { r.push(0); continue; }
    let pv = 0, vv = 0;
    for (let j = i - per + 1; j <= i; j++) { pv += c[j] * v[j]; vv += v[j]; }
    r.push(vv === 0 ? 0 : pv / vv);
  }
  return out(r);
};
export const obv = close => volume => {
  const c = nums(close), v = nums(volume), n = minLen(c, v), r = [];
  let acc = 0;
  for (let i = 0; i < n; i++) {
    if (i > 0) acc += c[i] > c[i - 1] ? v[i] : c[i] < c[i - 1] ? -v[i] : 0;
    r.push(acc);
  }
  return out(r);
};
export const volumeSma = volume => p => sma(volume)(p);
export const volumeRatio = volume => p => {
  const v = nums(volume), m = nums(volumeSma(volume)(p));
  return out(v.map((x, i) => m[i] === 0 ? 0 : (x * 100) / m[i]));
};
export const bodySize = open => close => binary(open, close, (o, c) => Math.abs(c - o));
export const upperWick = high => open => close => {
  const h = nums(high), o = nums(open), c = nums(close), n = Math.min(h.length, o.length, c.length);
  return out(Array.from({ length: n }, (_, i) => h[i] - Math.max(o[i], c[i])));
};
export const lowerWick = low => open => close => {
  const l = nums(low), o = nums(open), c = nums(close), n = Math.min(l.length, o.length, c.length);
  return out(Array.from({ length: n }, (_, i) => Math.min(o[i], c[i]) - l[i]));
};
export const rangePct = high => low => binary(high, low, (h, l) => l === 0 ? 0 : ((h - l) * 100) / l);
