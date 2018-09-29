const { print } = require('./util')

function PairC(a,b) {
  this.__a = a
  this.__b = b
}

const Pair = (a,b) => new PairC(a,b)

PairC.prototype.toString = function () {
  return "( " + this.__a + ", " + this.__b + " )"
}

print(Pair(5,6))