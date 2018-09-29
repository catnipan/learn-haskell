const print = val => console.log(val && val.toString())

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

const composeTwoFunc = (f, g) => (...args) => f(g(...args))
const compose = (...funcs) => funcs.reduce(composeTwoFunc)
const pipe = (...funcs) => funcs.reduceRight(composeTwoFunc)

const map = fn => functor => functor.map(fn)
const ap = mFunctor => sFunctor => sFunctor.ap(mFunctor)
const flatMap = fn => monad => monad.flatMap(fn)

module.exports = {
  print,
  compose,
  pipe,
  map,
  flatMap,
  ap,
  identity: x => x,
}