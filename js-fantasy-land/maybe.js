const { print } = require('./util');

function Maybe(val) {
  this.__value = val
}

Just = val => new Maybe(val)
Nothing = Just(null)

Maybe.prototype.toString = function () {
  if (this.isNothing()) {
    return "Nothing"
  }
  return "Just " + this.__value
}

Maybe.prototype.isNothing = function() {
  return (this.__value === null || this.__value === undefined)
};

Maybe.prototype.map = function (f) {
  if (this.isNothing()) {
    return Nothing
  }
  return Just(f(this.__value))
}

Maybe.prototype.join = function () {
  return this.__value
}

Maybe.prototype.flatMap = function (f) {
  return this.map(f).join()
}

Maybe.prototype.orElse = function (value) {
  if (this.isNothing()) {
    return Just(value)
  }
  return this
}

Maybe.prototype.ap = function (otherMaybeVal) {
  if (this.isNothing()) {
    return Nothing
  }
  return otherMaybeVal.map(this.__value)
}

// const liftA2 = curry((fn, m1, m2) => m1.map(fn).ap(m2));
// const map = curry((fn, m) => m.map(fn))
// const ap = curry((mf, m) => mf.ap(m))
// const flatMap = curry((fn, m) => m.flatMap(fn))

const add = a => b => a + b

print(Just(x => x + 1).ap(Just(3)))
print(Nothing.ap(Just(3)))
print(Just(x => x + 1).ap(Nothing))
print(Just(add).ap(Just(3)).ap(Just(4)))
