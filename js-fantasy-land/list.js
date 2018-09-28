const { print } = require('./util')

function _List_(list) {
  this.__list = list
}

_List_.prototype.toString = function () {
  return '[' + this.__list.toString() + ']'
}

_List_.prototype.map = function (f) {
  return _List_.new(this.__list.map(f))
}

_List_.prototype.join = function () {
  return _List_.new(this.__list.reduce((prevList, currList) => [...prevList, ...currList],[]))
}

_List_.prototype.flatMap = function (f) {
  return _List_.new(this.__list.map(f).join())
}

_List_.prototype.ap = function (anotherList) {
  return _List_.new(this.map(f => anotherList.map(f).__list).__list)
}

_List_.new = list => new _List_(list);
const List = (...list) => _List_.new(list);

// flatMap(f => [f(1), f(2)])

// print(List(x => x + 1, x => x * 3).ap(List(1,2)))
// const add = x => y => x + y;
// const mult = x => y => x * y;
// print(List(add, mult).ap(List(1,2)).ap(List(3,4)))
// print(List(1,2,3).map(x => x + 1))
// print(List(1,2,3,4).map(x => x + 1).flatMap(x => [x,x]))