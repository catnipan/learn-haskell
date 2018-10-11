[https://bartoszmilewski.com/2013/05/15/understanding-yoneda/]()

Functors in Hask are described by the type class Functor

```haskell
class Functor f where
fmap :: (a -> b) -> (f a -> f b)
```

A Haskell Functor
- maps types into types: by a type constructor
- maps functions into functions: by the fmap

A type contructor is a mapping from one type to another. For instance, a list type constructor takes any type a and creates a list type, [a].

对于那些范畴，存在函子把它里面的东西映射到集合上？
Such categories are called representable, meaning they have a representation in Set .

