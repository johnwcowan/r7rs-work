This proposal supersedes the [association-list section of SRFI 1](http://srfi.schemers.org/srfi-1/srfi-1.html#AssociationLists), maintaining compatibility but bringing it to parity with Common Lisp.

FIXME: More will be added later.

## Association lists

An "association list" (or "alist") is a list of pairs. The car of each pair contains a key value, and the cdr contains the associated data value. They can be used to construct simple look-up tables in Scheme. Note that association lists are probably inappropriate for performance-critical use on large data; in these cases, hash tables or some other alternative should be employed.

`(assoc `*key alist* [[|*=* ]]`)`

`(assq `*key alist*`)`

`(assv `*key alist*`)`

The *alist* argument must be an association list -- a list of pairs. These procedures find the first pair in *alist* whose car field is *key*, and returns that pair. If no pair in *alist* has *key* as its car, then `#f` is returned. `assq` uses `eq?` to compare *key* with the car fields of the pairs in *alist*, while `assv` uses `eqv?` and `assoc` uses *=* if given and `equal?` otherwise.  (R5RS, SRFI 1, Common Lisp ASSOC.)

```
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)                            =>  (a 1)
(assq 'b e)                            =>  (b 2)
(assq 'd e)                            =>  #f
(assq (list 'a) '(((a)) ((b)) ((c))))  =>  #f
(assoc (list 'a) '(((a)) ((b)) ((c)))) =>  ((a))
(assq 5 '((2 3) (5 7) (11 13)))	       =>  *unspecified*
(assv 5 '((2 3) (5 7) (11 13)))	       =>  (5 7)
```

`(reverse-assoc `*obj alist* [[|*=* ]] `)`

`(reverse-assq `*obj alist*`)`

`(reverse-assv `*obj alist*`)`

The *alist* argument must be an association list -- a list of pairs. These procedures find the first pair in *alist* whose cdr (rather than car) field is *key* , and return that pair. If no pair in *alist* has *obj* as its cdr, then `#f` (not the empty list) is returned. The `assq` procedure uses `eq?` to compare obj with the cdr fields of the pairs in *alist*, while `assv` uses `eqv?` and `assoc` uses *=* if given and `equal?` otherwise.  (Common Lisp RASSOC.)

```
(define e '((a 1) (b 2) (c 3)))
(reverse-assq 1 e)                            =>  (a 1)
(reverse-assq 2 e)                            =>  (b 2)
(reverse-assq 4 e)                            =>  #f
(reverse-assq 7 '((2 3) (5 7) (11 13)))	      =>  *unspecified*
(reverse-assv 7 '((2 3) (5 7) (11 13)))	      =>  (5 7)
```

The comparison procedure of `assoc` and `reverse-assoc` is used to compare the elements of *alist* to *key* in this way:  The first argument is always *key*, and the second argument is one of the elements of *list*. Thus one can reliably find the first entry of *alist* whose key is greater than five with `(assoc 5 alist <)`.

`(make-alist `*keys values*`)`

Returns a newly allocated alist whose pairs are constructed from the elements of the lists *keys* and *values*.  (Common Lisp PAIRLIS.)

`(alist-cons `*key datum alist*`)`

Cons a new alist entry that maps *key* to *datum* onto the beginning of *alist*.  (SRFI 1, Common Lisp ACONS.)

`(alist-copy `*alist*`)`

Make a newly allocated copy of *alist*. This means copying each pair that forms an association as well as the spine of the list.  (SRFI 1, Common Lisp COPY-ALIST.)

`(alist-delete `*key alist* [[|*=* ]]`)`

`(alist-delete! `*key alist* [[|*=* ]]`)`

`alist-delete` deletes all associations from *alist* with the given *key*, using the key-comparison procedure *=*, which defaults to `equal?`. The dynamic order in which the various applications of *= *are made is not specified.  Return values may share common tails with the *alist* argument. The *alist *is not disordered -- elements that appear in the result occur in the same order as they occur in *alist*.  The comparison procedure is used in the same way as by `assoc`. (SRFI 1.)

`alist-delete!` is the linear-update variant of `alist-delete`. It is allowed, but not required, to alter cons cells from the *alist* parameter to construct the result.  (SRFI 1.)

