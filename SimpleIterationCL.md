## While and until

The `while` and `until` macros provide simple iteration.
They are not directly present in Common Lisp, but are two
of the simplest cases of the `loop` macro.  

Syntax: `(while `*expr*` . `*body*`)`
Syntax: `(until `*expr*` . `*body*`)`

Semantics: The *expr* is evaluated, and if it is true/false,
the expressions in *body* are evaluated for effect;
their values are discarded.
Declarations are permitted at the beginning of *body*.
This algorithm is then repeated until *expr* is false/true.
The macro then returns an unspecified value.

# Do-times

The `do-times` macro is based on the very similar Common Lisp `dotimes` macro.
It is an abstraction of iterating over a range of consecutive exact integers,
comparable to Fortran `do`, Basic `for`...`next`, and of one of the basic uses
of `for` in C and other more recent languages.
There is no way to escape the loop except using `call/cc` or the equivalent.

It is less complex than Scheme `do`, to say nothing of various loop macros, and is
intended to make the processing of vectors one element at a time almost as easy as
processing lists similarly.
Because the use of a step value (other than 1) is a comparatively rare case,
stepping is not provided for.

Syntax: `(do-times (`*var [start*] *end* *result*`) .` *body*`)`

Semantics: Repeatedly evaluate the expressions in *body*
for effect; their values are discarded.  
Declarations are permitted at the beginning of *body*.
On the first iteration, *var* is bound to *start* (default value 0);
on the second iteration to *start* + 1, and so on.  When *var* would take
the value of *end*, the value of *result* is returned.

It is an error unless *var* is a single identifier
and *start* and *end* evaluate to exact integers.
They are evaluated before the loop begins,
whereas *result* is not evaluated until the end of the loop.
None of the three expressions can refer to *var*.

# Implementation

```
(define-syntax while
  (syntax-rules ()
    ((while expr . body)
     (let loop ()
       (when expr
         (let () . body)
         (loop))))))
         
(define-syntax until
  (syntax-rules ()
    ((until expr . body)
     (let loop ()
       (unless expr
         (let () . body)
         (loop))))))

(define-syntax while
  (syntax-rules ()
    ((while expr . body)
     (let loop ()
       (when expr
         (let () . body)
         (loop))))))
         
(define-syntax until
  (syntax-rules ()
    ((until expr . body)
     (let loop ()
       (unless expr
         (let () . body)
         (loop))))))

(define-syntax do-times
  (syntax-rules ()
      ((do-times (var end result) . body)
       (do-times (var 0 end result) . body))
      ((do-times (var start end result) . body)
        (begin
          (let ((s start) (e end))
            (let loop ((var s))
              (when (not (= var e))
                (let () . body)
                (loop (+ var 1)))))
          result))))
```
  