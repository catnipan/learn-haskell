const print = val => console.log(val && val.toString())

const __ = Symbol()

// const add4 = (x,y,z,w) => x + y + z + w
// curry(add4)(__,1,3)

const first = ls => ls[0]
const tail = ls => {
  const [x,...xs] = ls;
  return xs
}

const zipWith = fn => xs => ys => {
  const newList = Array(Math.min(xs.length, ys.length))
  for (let i = 0; i < newList.length; i += 1) {
    newList[i] = fn(xs[i], ys[i])
  }
  return newList
}

console.log(zipWith((x,y) => x + y)([1,2,3,4])([5,6,8,9]))

const _curry = (func, oldArgs) => (...newArgs) => {
  const mergedArgs = Array(func.length);
  for (let oi = 0, ni = 0, mi = 0; mi < func.length; mi += 1) {
    if (oldArgs[oi] === __) {
      mergedArgs[mi] = newArgs[ni]
      ni += 1
      oi += 1
    } else {
      mergedArgs[mi] = oldArgs[oi]
      oi += 1
    }
  }
  if (mergedArgs.every(arg => arg !== __)) {
    return func(...mergedArgs)
  }
  return _curry(func, mergedArgs)
}

const curry = func => _curry(func, Array(func.length).fill(__))

console.log(curry((a,b,c,d) => [a,b,c,d])(__,4,__,5)(2))

module.exports = {
  print
}