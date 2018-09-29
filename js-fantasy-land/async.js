const { pipe, map, flatMap } = require('./util')

function AsyncC(func) {
  this.__func = func;
}

const Async = func => new AsyncC(func)

AsyncC.prototype.map = function (f) {
  return Async(resolve => this.__func(val => resolve(f(val))));
}

AsyncC.prototype.join = function () {
  return Async(resolve => {
    return this.__func(val => {
      return val.map(resolve).run()
    })
  })
}

AsyncC.prototype.flatMap = function (f) {
  return this.map(f).join();
}

AsyncC.prototype.run = function () {
  return this.__func(() => {});
}

const inOneSecond = (func, ...params) => setTimeout(func, 1000, ...params);

const runIt = x => x.run()


// Async(resolve => {
//   inOneSecond(resolve, Async(resolve1 => {
//     inOneSecond(resolve1, 5)
//   }))
// }).join().map(x => {
//   console.log(x)
// }).run();

// Async((resolve) => {
//   setTimeout(() => {
//     resolve(Async(resolve => {
//       setTimeout
//     }))
//   }, 1000)
// })

// Async(resolve => {
//   inOneSecond(resolve, 5)
// }).map(val => {
//   return val + 1
// }).flatMap(val => {
//   console.log(val)
//   return Async(resolve => {
//     inOneSecond(resolve, val + 2)
//   })
// }).map(val => {
//   console.log(val)
// }).run()


pipe(
  map(val => val + 1),
  flatMap(val => {
    console.log(val)
    return Async(resolve => {
      inOneSecond(resolve, val + 2)
    })
  }),
  map(val => {
    console.log(val)
  }),
  runIt,
)(Async(resolve => {
  inOneSecond(resolve, 5)
}))

// Async(resolve => {
//   inOneSecond(resolve, 5)
// }).map(val => {
//   console.log(val)
//   return val + 1;
// }).map(val => {
//   console.log(val);
//   return val
// }).run()


// .flatMap(val => {
//   console.log(val);
//   return Async((resolve) => {
//     setTimeout(() => {
//       resolve(val + 3)
//     }, 2000)
//   });
// }).map(val => {
//   console.log(val);
// }).run()
