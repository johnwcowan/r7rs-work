
## Immutable data

This optional proposal is about managing data immutability.

Scheme data is mutable by default (except self-evaluating one like numbers or symbols), additional functions creating/managing immutable data are proposed.

In some case we wish to forbid change of some piece of data by further function call for instance.

Immutability is not the same as constant, as the content of an immutable data may refer to mutable data, while any part of a constant data is constant.

Primitives have to work on immutable data as on mutable one, except of course mutators which should return errors instead.

All data is either a mutable one or an immutable one.

### Associated procedures

`(make-immutable data)`

> Returns an immutable data containing a copy of data.

`(mutable? obj)`

> Returns #t if obj is a mutable, otherwise returns #f.

`(immutable->mutable obj)`

> This is an error if obj is not an immutable, else it returns a mutable copy of the data contained in obj.

### Notes

Copy in both direction is needed in order to avoid side-effect mutation. Of course if data is a literal or is computed "in place" (without content being the same as another variable) then copy is not needed.

Of course it is impossible to mutate the value contained inside an 'immutable' object:
```
(let ((a (make-immutable (list 1 2 3)))) (set-car! a 10) a) ;; Error
```

However the 'immutable' object itself could be mutated. For instance it is possible to mutate a variable like a list containing an 'immutable' object :
```
(define av (vector (make-immutable (list 1 2 3))))
(vector-set! av 0 10) ;; Ok
```

### Problems

An immutable data is globally immutable. Another proposal is needed if someone wants more data access management.

Do we need a proposal for defining constant data ?

Do we add a toggle-immutability! (sic) function ?

Helper functions to create immutable lists / tree may be added as sugar.

### Implementations and issues

> Immutability tag bit: cost in space and in time as immutability has to be checked with mutators.
>
> Memory Management Unit support: not available in general for embedded devices.
