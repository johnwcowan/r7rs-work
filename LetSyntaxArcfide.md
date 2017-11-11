# `let-syntax` Proposal

By Aaron W. Hsu

## Proposal

Remove the `let-syntax` form replacing it with a better behaved form.

## Rationale

`let-syntax` is ambiguous and troublesome. We will not have a good solution by trying to fix a broken form. It is a fundamentally inferior form due to the different ways that it has been interpreted and may continue to be interpreted. Specifying one of these interpretations as the official interpretation only makes an arbitrary distinction for no clear gain. It is better to remove the form entirely.

## Implementation and Proposed Replacement

There are two ways, at least, to go here. The simplest is this form:

```
(module (id ...) defs ... exps ...)
```

When evaluating the above form, all of the bindings `id ...` (expected to be defined by `defs ...`) are visible to the enclosing scope of the form. Otherwise, the body of the `module` behaves like a `let` form.

The problem with this form is that there is little way to easily port code using `let-syntax` to it without doing a manual code walk through your program and doing the appropriate transformations. If this idea were extended, however, as in the following syntax forms, backwards compatibility becomes easier.

```
(module [name] defs ... exps ...)
(export export-spec ...)
(import import-spec ...)
```

See [ModulesShinn](ModulesShinn.md) for the nature of `export-spec` and `import-spec`.

Now, if we have this, the we can specify a macro that transforms `let-syntax` into a `module` form:

```
(define-syntax (let-syntax x)
  (syntax-case x ()
    [[(k|([id exp]] ...) body ...)
     (with-syntax ([[define|(datum->syntax #'k 'define)]])
       #'(module (import (rename (only (rnrs) define) (define %define)))
           (...
             (define-syntax define
               (syntax-rules ()
                 [[(_|(id args ...) body ...)
                  (begin 
                    (%define (id args ...) body ...)
                    (export id))]]
                 [[(_|id exp)
                  (begin (%define id exp) (export id))]])))
           (define-syntax id exp) ...
           body ...))]))
```

The main difference here is that we are actually specifying `let*-syntax` above.

## Comment
I'd strongly recommend against any change which breaks an R5RS-compliant program.  w.r.

Is it possible to define let-syntax and friends directly in terms of module using syntax-rules?  --JohnCowan
