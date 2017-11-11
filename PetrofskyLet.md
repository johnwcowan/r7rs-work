The usual Schemes were asked to evaluate `(let - ((n (- 1))) n)`.  All of them returned -1 with the following exceptions:

Shoe, BDC do not support named let.

Rep, Owl Lisp return 1.

Llava reports that [SymbolImpl](SymbolImpl.md) cannot be cast to Pair.

