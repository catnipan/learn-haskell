const { print } = require('./util')

function FuncC(func) {
  this.__func__ = func
}

Func = func => new FuncC(func)

Func.mempty = x => x

FuncC.prototype.toString = function () {
  return this.__func__.toString()
}

FuncC.prototype.map = function (g) {
  return Func(x => g.__func__(this.__func__(x)))
}

print(Func(x => x + 1).map(Func(x => x * 3)).__func__(4))