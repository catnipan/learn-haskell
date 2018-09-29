const { identity, compose } = require('./util')

function PromiC(func) {
  this.__func = func;
}

const Promi = func => new PromiC(func);

PromiC.prototype.then = function (f) {
  return Promi((resolve, reject) => {
    this.__func(compose(resolve, f), reject)
  })
}

PromiC.prototype.catch = function (f) {
  return Promi((resolve, reject) => {
    this.__func(resolve, compose(reject, f))
  })
}

PromiC.prototype.run = function () {
  this.__func(identity, identity);
}

Promi((resolve, reject) => {
  setTimeout(reject, 1000, 5)
}).then(val => {
  console.log('success!')
  return val;
}).catch(val => {
  console.log('failed!')
  return val;
}).run();