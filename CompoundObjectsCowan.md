Compound objects are a generalization of SRFI 35 and R6RS compound conditions,
and are suitable for use in creating and handling conditions among other purposes.
They encapsulate an immutable sequence of subobjects, which can be
any object except another compound object.  

## Rationale

Compound objects belong to a disjoint type.  Consequently, they can
be used to represent multiple otherwise unrelated aspects of a value
or situation.  Because they are sequences, they can be used to
represent priorities of interpretation from higher to lower.  Most
of the operations described
in this section treat a simple condition identically to a compound
condition with itself as its own sole component. 

## Specification

When a compound object is created, a type for it may be specified:
this is either a symbol or `#f`, meaning that the object has no type.
Associated key-value properties are also specified in the form of an alist.
It is an error to mutate any list passed to or returned from these procedures.

## Procedures

`(make-compound ` *typesym alist obj* ...`)`

Create a compound object whose type is *typesym*
and whose properties are in *props*.
It contains the objects in *list* in the specified order.
If any object in *list* is itself a compound object,
it is flattened into its subobjects,
which are then added to the compound object in sequence.

```
;; The following definitions are referenced in later examples

(define-record-type <student>
  (student year gpa)
  student?
  (year year)      ; expected B.S. graduation year
  (gpa gpa))       ; grade-point average
  
(define-record-type <teacher>
  (teacher hired salary)
  teacher?
  (hired hired)     ; integer year
  (salary salary))  ; annualized
  
(define alyssa (student 1986 4.0)
(define guy (teacher 1981 25000)
(define george
  (make-compound 'ta '((quality . curiosity)) ; teaching assistant
    (student 1982 3.8)
    (teacher 1983 1000)))
    
(define (uni-member? obj)
  (or
    (student? obj)
    (teacher? obj)
    (and (compound? obj) (eqv? 'ta (compound-type obj)))))

(define (uni-member-date obj)
  (cond
    ((student? obj) (year obj))
    ((teacher? obj) (hired obj))
    (else #f)))
```

`(compound? `*obj*`)`

Returns `#t` if *obj* is a compound object, and `#f` otherwise.

```
(compound? alyssa) => #f
(compound? george) => #t
```

`(compound-type `*obj*`)`

If *obj* is a compound type, returns its type symbol.
If not, returns `#f`.

```
(compound-type alyssa) => #f
(compound-type? george) => #t
```

`(compound-properties `*obj*`)`

If *obj* is a compound type, returns its properties.
If not, returns `()`.

```
(compound-properties alyssa) => ()
(compound-properties george) => ((quality . curiosity))
```

`(compound-subobjects `*obj*`)`

If *obj* is a compound object, returns a list of its subobjects;
Otherwise, returns a list containing only *obj*.

```
(compound-subobjects alyssa) => (#<student>)
(compound-subobjects george) => (#<student> #<teacher>)
```

`(compound-length `*obj*`)`

If *obj* is a compound object, returns the number of its subobjects as an exact
integer.  Otherwise, it returns 1.

```
(compound-length alyssa) => 1
(compound-length george) => 2
```

`(compound-ref `*obj k*`)`

If *obj* is a compound object, returns the *k*th subobject.
Otherwise, *obj* is returned.
In either case, it is an error if *k* is less than
zero, or greater than or equal to `(compound-length `*obj*`)`.

```
(compound-ref alyssa 0) => #<student>
(compound-ref george 1) => #<teacher>
```

`(compound-map `*typesym alist mapper obj*`)`

If *obj* is a compound object, returns a compound object
of type *typesym* and with the properties in *alist*.
The subobjects result from invoking *mapper* on each subobject of *obj*.
Although the subobjects of the result are in the same order as the subobjects of *obj*,
the order in which *mapper* is applied to them is unspecified.
If any resulting subobject is itself a compound object, it is flattened into its subobjects,
which are then added to the result in sequence.

If *obj* is not a compound object, returns a compound object
whose typesym is `#f`, whose alist is `()`, and
whose only subobject is the result of applying *mapper* to *obj*.

```
(compound-map uni-member? alyssa) => #<compound: #t>
(compound-map uni-member? george) => #<compound: #t #t>
```

`(compound-map->list `*mapper obj*`)`

If *obj* is a compound object, returns a list
whose elements result from invoking *mapper* on each subobject of *obj*.
Although the elemnts of the result are in the same order as the subobjects of *obj*,
the order in which *mapper* is applied to them is unspecified.

If *obj* is not a compound object, returns a list
whose only element is the result of applying *mapper* to *obj*.

```
(compound-map->list uni-member? alyssa) => (#t)
(compound-map->list uni-member? george) => (#t #t)
```

`(compound-filter `*typesym alist pred obj*`)`

Returns a compound object
whose typesym is *typesym* and whose properties are in *alist*.
It contains the subobjects of *obj* that satisfy *pred*.

If *obj* is not a compound object, it returns a compound object
whose typesym is *typesym* and whose properties are in *alist*.
If *obj* satisfies *pred*, the only subobject of the result is *obj*.
If *obj* does not satisfy *pred*, the result has no subobjects.

```
(compound-filter teacher? alyssa) => #<compound>
(compound-filter teacher? george) =>
  #<compound: #<teacher>>
```

`(compound-predicate `*pred*`)`

Returns a procedure taking a single argument *obj*
that behaves as follows:

If *obj* is an object that:

 * satisfies *pred*
 * or is a compound object whose type is not `#f` 
   and its type satisfies *pred*
 * or at least one of its subobjects satisfies *pred*

then the procedure returns `#t`.  Otherwise it returns `#f`.

```
(define (teaches? obj) (compound-pred teacher?))
(teaches? alyssa) => #f
(teaches? george) => #t
```

`(compound-accessor `*pred accessor default*`)`

Returns a procedure taking a single argument *obj*
that behaves as follows:

If *obj* is a compound object, *accessor* is applied to
the first subobject of *obj* that satisfies *pred* and the result is returned;
if there is no such subobject, *default* is returned.

If *obj* is not a compound object, then if the object satisfies *pred*,
it applies *accessor* to *obj* and returns what it returns.
If *obj* does not satisfy *pred*, *default* is returned.

```
(define uni-member-hired (compound-accessor teacher? hired #f)
(uni-member-hired alyssa) => #f
(uni-member-hired guy) => 1981
(uni-member-hired george) => 1983
(uni-member-hired (make-compound #f '() 27 42 98 fire!) => #f
```

