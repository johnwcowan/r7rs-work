Mutable environments

Note that these environments are like `(interaction-environment)`.

`(make-mutable-environment)`

Returns a mutable environment.

`(environment-freeze! *env*`)`

Change *env* to be an ordinary (immutable) environment.

The R[67]RS procedure `environment` accepts environments as arguments as well as library names,
thus allowing `eval` to work with more than one mutable environment.

## Issues

1. Definition in a hybrid environement:  Given

```
(define e1 (make-environment))
(define e2 (make-environment))
(define e1+2 (environment e1 e2))
(eval '(define x 10) e1+2)
```

which environment is x defined in, e1 or e2, or is it an error?
