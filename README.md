# tapl-haskell
TaPL(Types and Programming Language)的Haskell代码实现，并没有严格按照原书内容

## Example

```shell
stack ghci
> :m +Control.Monad.State
> term = either undefined id $ parse "(lambda [x:Bool] (if x (lambda [x:Nat] x) (lambda [x:Nat] 0)))" 
> putStrLn (showTerm term)
> evalStateT (typeOf term) []
> evalStateT (eval term) []
```
