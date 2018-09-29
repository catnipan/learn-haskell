const Pair = require('./pair')
const { identity, head, tail } = require('./util')

function StateC (runState) {
  this.__runState = runState
}

// runState :: s -> (a, s)

StateC.prototype.runState = function (state) {
  return this.__runState(state)
}

const State = runState => new StateC(runState)

StateC.prototype.chainState = function (f) {
  return State(state => {
    const res = this.__runState(state)
    const a = res.fst()
    const newState = res.snd()
    const newRunState = f(a)
    return newRunState(newState)
  })
}

const pop = stack => Pair(head(stack), tail(stack))
const push = newItem => stack => Pair(undefined, [newItem, ...stack])
const readTop = stack => Pair(head(stack), stack)
const read = stack => Pair(stack, stack)
const put = newStack => () => Pair(undefined, newStack)

// State(pop).runState([1,2,3,4])
console.log(State(pop).chainState((val) => push(val + 1)).chainState(() => readTop).chainState(val => state => {
  console.log(val);
  return Pair(undefined, state)
}).runState([1,2,3,4]))