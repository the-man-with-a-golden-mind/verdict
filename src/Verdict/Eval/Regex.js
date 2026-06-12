const compile = pattern => {
  try {
    return new RegExp(pattern, "g");
  } catch (_) {
    return null;
  }
};

export const regexTest = pattern => input => {
  const re = compile(pattern);
  return re == null ? false : re.test(input);
};

export const regexFindAll = pattern => input => {
  const re = compile(pattern);
  if (re == null) return [];
  return Array.from(input.matchAll(re), match => match[0]);
};

export const regexReplace = pattern => replacement => input => {
  const re = compile(pattern);
  return re == null ? input : input.replace(re, replacement);
};

export const regexSplit = pattern => input => {
  const re = compile(pattern);
  return re == null ? [input] : input.split(re);
};
