const { print, map, ap, flatMap, pipe } = require('./util')

function ListC(list) {
  this.__list__ = list
}

ListC.prototype.toString = function () {
  return '[' + this.__list__.toString() + ']'
}

ListC.prototype.map = function (f) {
  return ListC.new(this.__list__.map(f))
}

ListC.prototype.join = function () {
  return ListC.new(
    this.__list__.reduce(
      (prevList, currList) => prevList.concat(currList),
      ListC.mempty
    ).__list__
  )
}

ListC.prototype.concat = function(anotherList) {
  return ListC.new(this.__list__.concat(anotherList.__list__))
}

ListC.prototype.flatMap = function (f) {
  return this.map(f).join()
}

ListC.prototype.ap = function (anotherList) {
  return this.flatMap(f => anotherList.map(f))
}

ListC.new = list => new ListC(list);
ListC.mempty = new ListC([])
const List = (...list) => ListC.new(list);

// flatMap(f => [f(1), f(2)])

ListC.prototype.filterM = function (filter) {
  // undefined
}

// List(1,2,3,4).filterM(x => List(True,False))

const add = x => y => x + y;
const mult = x => y => x * y;

// print(List(1,2,3,4).flatMap(x => List(x + 1, x + 2)))
// print(List(1,2,3,4).concat(List(4,5,6,7)))
// print(List(List(1,2), List(3,4), List(5,6)).join())
// print(List(x => x + 1, x => x * 3).ap(List(1,2)))
// print(List(add, mult).ap(List(1,2)).ap(List(3,4)))

pipe(
  ap(List(1,2)),
  ap(List(3,4,5)),
  ap(List(4,3,2,3,2,1)),
  print,
)(List(x => y => z => List(x,y,z)))

pipe(
  map(x => x + 1),
  flatMap(y => List(y, y * 2)),
  print,
)(List(1,2,3,4,5))
