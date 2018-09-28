const { print } = require('./util')

function Either(left, right) {
  this.__left = left
  this.__right = right
}

Left = left => new Either(left)
Right = right => new Either(null, right)

Either.prototype.toString = function () {
  if (this.isLeft()) {
    return "Left " + this.__left
  }
  return "Right " + this.__right
}

Either.prototype.map = function (f) {
  if (this.isLeft()) {
    return this
  }
  return Right(f(this.__right))
}

Either.prototype.isLeft = function () {
  return !(this.__left === undefined || this.__left === null)
}

print(Left("calc Error").map(x => x * 5))
print(Right(3).map(x => x + 1))