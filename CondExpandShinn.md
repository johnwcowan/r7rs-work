In the small language you can test library availability with:

```
(define-library (foo bar)
  ...
  (cond-expand
   ((library (scheme regexp))
    ...)
   (else
    ...)))
```

but you might want to be sure that the `regexp-unicode` feature was available, like this:

```
(define-library (foo bar)
  ...
  (cond-expand
   ((library-provides (scheme regexp) regexp-unicode)
    ...)
   (else
    ...)))
```

