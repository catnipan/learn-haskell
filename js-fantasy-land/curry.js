const __ = Symbol();

const _mergeArgs = (oldArgs, newArgs) => {
  const resArgs = Array(oldArgs.length);
  for (let oi = 0, ni = 0, mi = 0; mi < resArgs.length; mi += 1) {
    if (oldArgs[oi] === __) {
      resArgs[mi] = newArgs[ni] || __
      ni += 1
      oi += 1
    } else {
      resArgs[mi] = oldArgs[oi]
      oi += 1
    }
  }
  return resArgs
}

const _curry = (func, oldArgs) => (...newArgs) => {
  const resArgs = _mergeArgs(oldArgs, newArgs)
  if (resArgs.every(arg => arg !== __)) {
    return func(...resArgs)
  }
  return _curry(func, resArgs)
}

const curry = func => _curry(func, Array(func.length).fill(__))

module.exports = curry
