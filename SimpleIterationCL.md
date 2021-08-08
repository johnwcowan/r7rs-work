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

## Do-times

The `do-times` macro is based on the very similar Common Lisp `dotimes` macro.
It is an abstraction of iterating over a range of consecutive exact integers,
comparable to Fortran `do`, Basic `for`...`next`, and of one of the basic uses
of `for` in C and other more recent languages.
There is no way to escape the loop except using `call/cc` or the equivalent.

It is less complex than Scheme `do`, to say nothing of various loop macros, and is
intended to make the processing of vectors one element at a time almost as easy as
processing lists similarly.

Syntax: `(do-times *var* `(start `*start*`)` `(end` *end*`)` [`(step `*step*`)` `(return `*expr4*`)` .` *body*`)`

Semantics: Repeatedly evaluate the expressions in *body*
for effect; their values are discarded.
The values of *start*, *end*, and *step* are evaluated first.

On the first iteration, *var* is bound to *start*.
On the second iteration, *var* is incremented by *step* and so on.
When *var* is greater than *end* (no default), *return* is evaluated and returned.
Declarations are permitted at the beginning of *body*.

## Do-list

The `do-list` macro is based on the very similar Common Lisp `dolist` macro.
It is an abstraction of iterating over the elements of a list.
There is no way to escape the loop except using `call/cc` or the equivalent.

It is less complex than Scheme `do`, to say nothing of various loop macros, and is
intended to make the processing of lists one element at a time easy.

Syntax: `(do-list `*var* *list*` . `*body*`)`

Semantics: Repeatedly evaluate the expressions in *body*
for effect; their values are discarded.
The value of *list* is evaluated first.

On each iteration, *var* is bound to a successive element of *list*.
Declarations are permitted at the beginning of *body*.

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


(define-syntax do-times
  (syntax-rules ()
      ((do-times var (start st) (end en) (step st) (result re) . body)
        (begin
          (let ((s st) (e en) (t st))
            (let loop ((var s))
              (when (< var en)
                (let () . body)
                (loop (+ var st)))))
          result))))
```
  
