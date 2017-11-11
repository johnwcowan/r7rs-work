The existing standard and SRFI record systems for Scheme (R6RS, R7RS, SRFI 57, SRFI 99, SRFI 131, SRFI 136) all claim backward compatibility with SRFI 9, which has huge support among implementations.  However, neither SRFI 9 nor any of the extensions make it completely clear whether the field names in SRFI 9 are hygienic identifiers or raw symbols.  Specifically, there are two questions:

1. Whether *define-record-type* matches field names in the constructor to names in the fields hygienically.
2. Whether the field names are stored in the rtd hygienically or are stripped of hygiene as with *syntax->datum*.

Regarding 2, there is absolutely no semantic meaning for SRFI 9.  For this we can only consider consistency with 1 and with possible future extensions.

Regarding 1, the problem for SRFI 9 arises with the potential renaming of field names.  For example, one might want a "tuple" type with anonymous field names, which is potentially more efficient than a vector as there's no need to store length:

```
;; This test is for R5RS + SRFI 9, or equivalently R7RS.
(define-syntax define-tuple-type
  (syntax-rules ()
    ((define-tuple-type name make pred x-ref (defaults ...))
     (deftuple name (make) pred x-ref (defaults ...) (defaults ...) ()))))

(define-syntax deftuple
  (syntax-rules ()
    ((deftuple name (make args ...) pred x-ref defaults (default . rest)
       (fields ...))
     (deftuple name (make args ... tmp) pred x-ref  defaults rest
       (fields ... (tmp tmp))))
    ((deftuple name (make args ...) pred x-ref (defaults ...) ()
       ((field-name get) ...))
     (begin
       (define-record-type name (make-tmp args ...) pred
         (field-name get) ...)
       (define (make . o)
         (if (pair? o) (apply make-tmp o) (make-tmp defaults ...)))
       (define x-ref
         (let ((accessors (vector get ...)))
           (lambda (x i)
             ((vector-ref accessors i) x))))))))

;; (make-point [[first|second]]) returns a new point
;; (point-ref pt <0 or 1>) fetches the first or second field
(define-tuple-type point make-point point? point-ref (0 0))

;; will output (0 0) if field names are matched hygienically
(let ((pt (make-point)))
  (write (list (point-ref pt 0) (point-ref pt 1)))
  (newline))
;; will output (1 2) if field names are matched hygienically
(let ((pt (make-point 1 2)))
  (write (list (point-ref pt 0) (point-ref pt 1)))
  (newline))
```

## Compatibility

For the {{{define-tuple}}} form above, MIT Scheme reports {{{The object (tmp tmp), passed as an argument to make-record-type, is not a list of unique symbols.}}}  I presume that this means that it matches unhygienically.

What do other implementations do?

## Arguments Supporting the Hygienic Interpretation of SRFI 9 Field Names

1. SRFI 9 lists the field names as "identifiers" in the sense of section 8.1.6 of the R5RS, the same context in which *define* defines an identifier.
2. The primary motivation for SRFI 9 was to be definable with *syntax-rules*, suggesting hygiene was of high importance.
3. Kelsey and Rees' original implementation of *define-record-type* on which SRFI 9 was based matches hygienically.
4. Scheme48 and Racket pass the above test, and are generally reliable when it comes to syntax.

## Arguments Supporting the Raw Symbol Interpretation of SRFI 9 Field Names

1. The portable reference implementation of SRFI 9, though not normative, matches unhygienically (on the other hand, the portable implementation is a hack and is incompatible with library systems).

If we cannot agree on the semantics of SRFI 9, then it is in effect undefined, and remains so for the R7RS small language.  Future SRFIs can opt to include their own clarifying semantics, and for this we should consider interaction with various extensions.

## Advantages of the Hygienic Interpretation of Field Names

1. The only ubiquitous extension to SRFI 9 is the removal of the restriction to the top-level.  With lexical scope name conflicts become much more common.  If hygiene is stripped, then *define-record-type* effectively becomes a fragile construct which simply won't work everywhere you expect.
> {{{
> (define-syntax trap-lambda
> (syntax-rules ()
> ((trap-lambda (param ...) body ...)
> (let ()
> (define-record-type name (make param ...) is-name? (param param) ...)
> (lambda (param ...)
> (guard (exn (else (trace-exception (make param ...) exn)))
> body ...))))))
> (define (trace-exception params exn)
> (display "caught exception in lambda: ")
> (write params)
> (newline)
> (raise exn))
> ((trap-lambda (x y) (+ x y)) 1 "2")
> }}}
2. Pattern matching and similar extensions such as those found in SRFI 57 become impossible to implement in *syntax-rules*.  With the addition of low-level macros these could become possible again, but the risk of name collisions is even higher.

## Advantages of the Raw Symbol Interpretation of Field Names

1. Procedural extensions which make use of field names may need introspection to access those names.  Specifically, to inherit from a syntactic record type procedurally while allowing parent fields in the constructor requires a utility to get the rtd fields in a fixed order (such as in the R6RS record inspection layer), and moreover is clumsy.
2. Syntactic extensions which provide inheritance and allow this to refer to field names in the parent type must match the field name hygienically.  This means when referring to a record type in another library the name must either be unbound in both libraries, or share the same binding (e.g. as auxiliary syntax).  With field names typically short, collision probabilities are high.
