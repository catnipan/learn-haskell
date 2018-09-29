const { print } = require('./util')

function PairC(fst,snd) {
  this.__fst = fst
  this.__snd = snd
}

const Pair = (fst,snd) => new PairC(fst,snd)

PairC.prototype.toString = function () {
  return "( " + this.__fst + ", " + this.__snd + " )"
}

PairC.prototype.fst = function () {
  return this.__fst
}

PairC.prototype.snd = function () {
  return this.__snd
}

module.exports = Pair

// print(Pair(5,6))