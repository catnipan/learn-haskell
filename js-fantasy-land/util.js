const print = val => console.log(val && val.toString())

const __ = Symbol();

const first = ls => ls[0]
const tail = ls => {
  const [x,...xs] = ls;
  return xs
}

const zipWith = fn => lx => ly => {
  const zipper = (_lx, _ly, rs) => {
    const [x, ...xs] = _lx;
    const [y, ...ys] = _ly;
    if (x === undefined || y === undefined) {
      return rs;
    }
    return zipper(xs, ys, [...rs,fn(x,y)]);
  }
  return zipper(lx, ly, []);
}

const zip = zipWith((x,y) => [x,y])

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

module.exports = {
  print,
  curry,
}