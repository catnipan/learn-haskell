const LAZY = Symbol();
const LAZY_TYPE = {
  map: Symbol(),
  filter: Symbol(),
  take: Symbol(),
};

const map = fn => {
  const mapFunc = xs => xs.map(fn);
  mapFunc[LAZY] = {
    type: LAZY_TYPE.map,
    val: fn,
  };
  return mapFunc;
}

const filter = fn => {
  const filterFunc = xs => xs.filter(fn);
  filterFunc[LAZY] = {
    type: LAZY_TYPE.filter,
    val: fn,
  };
  return filterFunc;
}

const take = n => {
  const takeFunc = xs => {
    const result = [];
    for (let i = 0; i < xs.length && i < n; i++) {
      result.push(xs[i]);
    }
    return result;
  }
  takeFunc[LAZY] = {
    type: LAZY_TYPE.take,
    val: n,
  };
  return takeFunc;
}

const lazyFlow = (fn...) => source => {
  // createSourceBuffer
  // askForUpperSource
  // if done, then pipe down downward
}

lazyFlow(
  map(x => x + 3),
  filter(x => x < 8),
  take(2),
)([1,2,3,4,5]);

lazyMap = fn => xs => {
  
}

sourceBuffer.next() 1
sourceBuffer.next() 2
sourceBuffer.next() 3
sourceBuffer.next() 4
sourceBuffer.next() 5, done

const y = take(2)(filter(x => x < 8)(map(x => x + 3)([1,2,3,4,6,7,8,9])));
console.log(y);