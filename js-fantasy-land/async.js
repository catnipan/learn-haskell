const { pipe, map, flatMap, identity } = require('./util')

function AsyncC(func) {
  this.__func = func;
}

const Async = func => new AsyncC(func)

AsyncC.prototype.map = function (f) {
  return Async(resolve => this.__func(val => resolve(f(val))));
}

AsyncC.prototype.join = function () {
  return Async(resolve => {
    return this.__func(async => {
      return async.run(resolve)
    })
  })
}

AsyncC.prototype.flatMap = function (f) {
  return this.map(f).join();
}

AsyncC.prototype.run = function (f = identity) {
  return this.__func(f);
}

const runIt = x => x.run()


Async(resolve => {
  setTimeout(resolve, 1000, 5)
}).map(val => {
  return val + 1
}).flatMap(val => {
  console.log(val)
  return Async(resolve => {
    setTimeout(resolve, 1000, val + 2)
  })
}).run(val => {
  console.log(val);
})


pipe(
  map(val => val + 1),
  flatMap(val => {
    console.log(val)
    return Async(resolve => {
      setTimeout(resolve, 1000, val + 2)
    })
  }),
  map(val => {
    console.log(val)
  }),
  runIt,
)(Async(resolve => {
  setTimeout(resolve, 2000, 5)
}))
