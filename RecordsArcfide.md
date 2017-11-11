# WG1 Record Proposal =

By Aaron W. Hsu

Current Revision: 7 June 2011

## Executive Summary

The following record proposal is designed to satisfy the following
constraints:

* Respect for existing standards and practices
* Easy to implement and broadly useful
* Easy to extend and wrap
* Simple and Straightforward to use

It is designed as a counterpoint to SRFI-9 specifically to alleviate a
transition from SRFI-9 and R6RS based code in a WG2 friendly manner
without adding any controversial or unnecessary complexity. The design
emphasizes a respect for SRFI-9's feature-set while presenting the
user and implementor with a more flexible and extensible interface
for future extensions, such as implementation extensions or extensions
that may be defined by WG2.

## Benefits compared to SRFI-9

SRFI-9 is a well respected and well established de facto standard.
With this understanding, this proposal tries to take this into account
while improving SRFI-9 in the following ways:

* Eliminates filtering constructors in favor of other extensions
> (not specified in this proposal). See the rationale for why this
> is a good thing.
* Provides an extensible syntax compatible with previous standards
* Does not intrude on implementation record systems

## Specification

Syntax: `define-disjoint-type`

Auxiliary Keyword: `fields`[#BR]]
Auxiliary Keyword: `mutable`[#BR]]
Auxiliary Keyword: `immutable`

The general syntax for `define-disjoint-type` is as follows:

`(define-disjoint-type (name constructor predicate) clause ...)`

The `clause` is one or more record clauses that define features and
behaviors of the record. Only one is defined for this proposal, but
others may be provided by implementations:

`(fields field-spec ...)`

Only one `fields` clause should appear in the list.
In a `fields` clause the `field-spec` has one of the following forms:

`(field-name immutable field-accessor)`[#BR]]
`(field-name mutable field-accessor field-mutator)`

This has the following effect in the scope of the call
to `define-disjoint-type`.

* `name` is bound to an implementation specific value (may be syntax)
* `constructor` is bound to a procedure that takes as many arguments
> as there are `field-name`s. Applying `constructor` creates an instance
> of the record whose field values are those specified by the arguments
> to the `constructor` in the same order as the `field-names`.
* `predicate` is bound to a procedure of one argument that returns true
> only for objects returned by `constructor`; implementations may of
> course extend this behavior if there are other ways of creating
> instances of `name` records. For all other values, `predicate` returns
> false.
* Each `field-accessor` is bound to a procedure of one argument, which
> when passed an instance of a `name` record, will return the value of
> the slot associated with the accessor.
* Each `field-mutator` is bound to a procedure of two arguments, which
> when passed an instance of a `name` record and a value will mutate
> the slot associated with `field-mutator` to contain the value passed
> to `field-mutator`.

## Compatibility with SRFI-9

A trivial macro layered over `define-disjoint-type` is sufficient to
make/use SRFI-9 code:

```
(define-syntax define-record-type
  (syntax-rules (%)
    ((_ % (name constructor predicate) (cargs ...) ((fn m rest ...) ...) ())
     (begin
       (define-disjoint-type (name %constructor predicate)
	 (fields (fn m rest ...) ...))
       (define constructor 
	 (let ()
	   (define-syntax in-field
	       (syntax-rules (fn ...)
	       ((_ fn) #t) ...
	       ((_ else) #f)))
	   (unless (in-field cargs) 
	     (error 'define-record-type "no such field" 'cargs))
	   ...
	   (lambda (cargs ...)
	     (define-syntax get-val
	       (syntax-rules (cargs ...)
		 ((_ cargs) cargs) ...
		 ((_ filtered) (if #f #f))))
	     (%constructor (get-val fn) ...))))))
    ((_ % bindings cargs (fcs ...) ((fn fa) rest ...))
     (define-record-type % bindings cargs 
       (fcs ... (fn immutable fa)) (rest ...)))
    ((_ % bindings cargs (fcs ...) ((fn fa fm) rest ...))
     (define-record-type % bindings cargs 
       (fcs ... (fn mutable fa fm)) (rest ...)))
    ((_ name (constructor cargs ...) predicate fields ...)
     (define-record-type % (name constructor predicate) 
       (cargs ...) () (fields ...)))))
```

See the rationale notes about the filtering constructor for more
discussion about this.

Additionally, if an implementation wishes to implement this records system
on top of their own (assuming that they have SRFI-9 already implemented),
then the following macro is sufficient:

```
(define-syntax define-disjoint-type
  (syntax-rules (fields)
    ((_ (name constructor predicate) (fields (fn m rest ...) ...))
     (define-record-type name (constructor fn ...) predicate
       (fn rest ...) ...))))
```

The above assumes that a hygienic implementation of SRFI-9 is available.
Without a hygienic implementation of SRFI-9, the implementor must take
care to deal with the cases where the field names are symbolically
equal but not hygienically/lexically equivalent.

## Rationale

**Dropping the filtering constructor.**
The most notable and indeed the most important divergence of this
proposal from the core semantics of SRFI-9 is the elimination of
the filtering constructor as defined in SRFI-9. This proposal
does **not** preclude extensions which enable a more general
functionality, such as R6RS protocols, and other implementation
proposals, but it does eliminate a filtering constructor in the
way that it is handled by SRFI-9, which is rife with subtle
problems.

*Problem 1*. The filtering constructor provides no means of
determining or specifying the default values of the record slots
that are not passed values explicitly in the call to the constructor.
This requires that all record slots which are to be useful that
do not have an explicit constructor field must be mutable, since
the only way to fill the record slot with something useful after
construction is to mutate the slot. This makes it impossible to
have an immutable record slot that is not given an explicit value
in the constructor using the filtering constructor mechanism. This
is done very often (I use this often to embed things like hidden
lookup tables into an object; these lookup tables will never change,
and I can get important gains by enforcing immutability on these
slots). Instead, if one wishes to achieve this sort of thing,
one must avoid the use of the filtering constructor and use
an explicit procedure that calls the constructor. This is a much
more common case in SRFI-9 usage, and makes the filtering constructor
much less useful.

One may also argue that the filtering constructor so used suggests
to the user a less functional style in programming, which is arguably
less elegant, since other record systems achieve a more general
result without requiring mutation.

*Problem 2*. The filtering constructor is also ill-defined by
SRFI-9 as to whether or not field names are matched hygienically or
symbolically. In the reference implementation they are matched
symbolically, which breaks hygiene and makes the use of SRFI-9
records inside of macros dangerous in combination with filtering
constructors. The above implementation that I have provided for
SRFI-9 does match field names hygienically, improving the security
of the implementation, but this diverges from the reference implementation
of SRFI-9. Both SRFI-9 and SRFI-99 share this symbolic equality
matching limitation. I do not know whether existing implementations
follow the reference implementation in this matter.

*Comparison*. The above problems are addressed by this proposal
in two ways. Firstly, by removing the filtering constructor, no
functionality has been lost, but issues with hygiene and mutability
have been addressed. The above implementation of SRFI-9 in terms of
this proposal demonstrate how the same functionality in SRFI-9
filtering constructors may be achieved with relative ease. Moreover,
by removing the filtering constructor, we have removed all situations
where it will make a difference whether field names are matched
hygienically or symbolically. Since the field-names are not used for
anything except place-holders in the current proposal, this means that
this issue is effectively side-stepped, and more expansive record
systems can deal with the issue explicitly. This reduces complexity
of the specification for records in WG1 and makes the overall
system simpler.

**Lack of inheritance.**
My initial proposal included a provision for single inheritance. This
proposal did not receive enough consensus and lost to SRFI-9. I therefore
remove the single inheritance in favor of remaining closer and less
divergent from SRFI-9.

**Divergence from SRFI-9 syntax.**
The syntax of this proposal is a very small subset of R6RS. This is a
divergence from SRFI-9, but was chosen for particular reasons. Namely,
this syntax is much more forward compatible with possible extensions
that may be defined by implementations (such as R6RS or WG2 implementations)
and it is compatible with existing R6RS record definitions. There is
a lot of R6RS code out there.

When choosing a syntax, either SRFI-9 or R6RS, there will be backwards
compatibility issues. Those users who use R6RS will be left out if
one uses SRFI-9, and those who use R6RS might leave behind those who
use SRFI-9. Thus, neither syntax provides inherently better backwards
compatibility than the other, since there exist many code repositories
with both, sometimes intermingled. The choice to go with the R6RS syntax
was made based on its cleanliness and overall elegance in comparison to
SRFI-9, not the least of which is the removal of the filtering
constructor.

In summary, the SRFI-9 syntax doesn't really gain us that much in
backwards compatibility and makes our future compatibilities much more
complex. Continuing with the SRFI-9 syntax is very likely to cause us
pain in the future and make useful progress more difficult. It is
better to have a single syntax framework that enables us to move forward
on a single record syntax rather than encouraging implementations and
working groups in the future, such as WG2, to diverge from the WG1
record system entirely.

**Using the name `define-disjoint-type`.**
A major complaint to the R6RS record system was that it used the same
name as the SRFI-9 system. In reference to this, I have chosen not to
use the `define-record-type` name so that it does not conflict with
either the R6RS syntax or the SRFI-9 implementations. R6RS implementations
can support the new syntax by aliasing, while SRFI-9 implementations can
continue providing SRFI-9 while also providing this syntax, with
translations between the two being straightforward.

**Use of keywords for immutability of fields.**
The use of keywords for fields (whether or not explicitly bound) is
in keeping with the R6RS and allows for extensions that improve the
brevity of record specifications. These extensions, which are common
in implementations, automatically construct the identifiers for
accessor and mutators. The keywords provide disambiguation for these
situations.

**Divergence from R6RS.**
This proposal differs from the equivalent subset of R6RS' record
functionality in one incompatible way: the keywords for mutability occur
after the field names. This was done to emphasize the field names as
more important. Doing so does not introduce any ambiguity in the syntax,
but it will require R6RS implementations to make minor changes to adapt
their existing macros.

**Lack of R6RS name "construction".**
Many people feel that the construction of identifiers using other
identifiers (something that is not possible in pure syntax-rules) is
not a good thing. While I have no problem with it, and encourage this
extension as an implementation extension, possibly as part of WG2,
I recognize that this is something that can be done later, and there
is no need to introduce such a controversial feature into the WG1
record system.

**Binding of `name`.**
The binding of name is to ensure that a certain concept of record
semantics common in most systems is available. Most more complete
record systems will bind the name, and it is important not to let
user's think that they may in fact bind `name` to something else
with the same results. This ensures upwards compatibility with
an extended record system provided by WG2.

## R5RS Portable Implementation

The following is a somewhat faithful portable implementation of the
above proposal in R5RS Scheme. Types fail to be disjoint against
vectors but otherwise the major features of the above are provided.
I also assume an error reporting mechanism and `when/unless`.
I would actually prefer to do this all as a single macro, but it
works like this. If you are willing to accept common extensions to
R5RS such as internal define-syntax then you can do this as one
macro quite nicely.

```
(define-syntax define-disjoint-type
  (syntax-rules (fields)
    ((_ (name constructor predicate)
	(fields (fn m rest ...) ...))
     (begin
       (define name (cons 'name '()))
       (define (predicate val)
	 (and (vector? val)
	      (= (vector-length val)
		 (+ 1 (length '(fn ...))))
	      (eq? name (vector-ref val 0))))
       (define (constructor fn ...) (vector name fn ...))
       (define-field-procs* 0 predicate (fn m rest ...) ...)))))
(define-syntax define-field-procs*
  (syntax-rules ()
    ((_ counter predicate field-spec) 
     (define-field-procs counter predicate field-spec))
    ((_ counter predicate field-spec rest ...)
     (begin
       (define-field-procs counter predicate field-spec)
       (define-field-procs* (+ 1 counter) predicate rest ...)))))
(define-syntax define-field-procs
  (syntax-rules (immutable mutable)
    ((_ counter predicate (name immutable accessor))
     (define (accessor val)
       (unless (predicate val)
	 (error 'accessor "invalid record" val))
       (vector-ref val (+ 1 counter))))
    ((_ counter predicate (name mutable accessor mutator))
     (begin
       (define (accessor val)
	 (unless (predicate val)
	   (error 'accessor "invalid record" val))
	 (vector-ref val (+ 1 counter)))
       (define (mutator rec val)
	 (unless (predicate rec)
	   (error 'mutator "invalid record" rec))
	 (vector-set! rec (+ 1 counter) val))))))
```

Here is a transcript of the expansion and evaluation of the above:

```
> (expand 
    '(let () 
       (define-disjoint-type (test make test?) 
         (fields (a immutable get-a) (b mutable get-b set-b!)))
       (list (make 3 4) (test? (make 3 4)) 
             (get-a (make 3 4)) (get-b (make 3 4)) 
             (let ((v (make 3 4))) (set-b! v 7) (get-b v)))))
(letrec* ((test.35 (#2%cons 'test '()))
          (test?.36 (lambda (val.37)
                      (if (#2%vector? val.37)
                          (if (#2%=
                                (#2%vector-length val.37)
                                (#2%+ 1 (#2%length '(a b))))
                              (#2%eq? test.35 (#2%vector-ref val.37 0))
                              #f)
                          #f)))
          (make.38 (lambda (a.39 b.40) (#2%vector test.35 a.39 b.40)))
          (get-a.41 (lambda (val.42)
                      (if (#2%not (test?.36 val.42))
                          (#2%error 'get-a "invalid record" val.42)
                          (#2%void))
                      (#2%vector-ref val.42 (#2%+ 1 0))))
          (get-b.43 (lambda (val.44)
                      (if (#2%not (test?.36 val.44))
                          (#2%error 'get-b "invalid record" val.44)
                          (#2%void))
                      (#2%vector-ref val.44 (#2%+ 1 (#2%+ 1 0)))))
          (set-b!.45 (lambda (rec.46 val.47)
                       (if (#2%not (test?.36 rec.46))
                           (#2%error 'set-b! "invalid record" rec.46)
                           (#2%void))
                       (#2%vector-set!
                         rec.46
                         (#2%+ 1 (#2%+ 1 0))
                         val.47))))
  (#2%list (make.38 3 4) (test?.36 (make.38 3 4))
    (get-a.41 (make.38 3 4)) (get-b.43 (make.38 3 4))
    (let ((v.48 (make.38 3 4)))
      (set-b!.45 v.48 7)
      (get-b.43 v.48))))
> (expand/optimize 
    '(let () 
       (define-disjoint-type (test make test?) 
         (fields (a immutable get-a) (b mutable get-b set-b!)))
       (list (make 3 4) (test? (make 3 4)) 
             (get-a (make 3 4)) (get-b (make 3 4)) 
             (let ((v (make 3 4))) (set-b! v 7) (get-b v)))))
(let ((test.49 (#2%cons 'test '())))
  (let ((val.50 (#2%vector test.49 3 4)))
    (let ((val.51 (#2%vector test.49 3 4)))
      (let ((val.52 (#2%vector test.49 3 4)))
        (let ((v.53 (#2%vector test.49 3 4)))
          (#2%list (#2%vector test.49 3 4)
            (if (#2%vector? val.50)
                (if (#2%= (#2%vector-length val.50) 3)
                    (#2%eq? test.49 (#2%vector-ref val.50 0))
                    #f)
                #f)
            (begin
              (if (#2%not
                    (if (#2%vector? val.51)
                        (if (#2%= (#2%vector-length val.51) 3)
                            (#2%eq? test.49 (#2%vector-ref val.51 0))
                            #f)
                        #f))
                  (#2%error 'get-a "invalid record" val.51)
                  (#2%void))
              (#2%vector-ref val.51 1))
            (begin
              (if (#2%not
                    (if (#2%vector? val.52)
                        (if (#2%= (#2%vector-length val.52) 3)
                            (#2%eq? test.49 (#2%vector-ref val.52 0))
                            #f)
                        #f))
                  (#2%error 'get-b "invalid record" val.52)
                  (#2%void))
              (#2%vector-ref val.52 2))
            (begin
              (if (#2%not
                    (if (#2%vector? v.53)
                        (if (#2%= (#2%vector-length v.53) 3)
                            (#2%eq? test.49 (#2%vector-ref v.53 0))
                            #f)
                        #f))
                  (#2%error 'set-b! "invalid record" v.53)
                  (#2%void))
              (#2%vector-set! v.53 2 7)
              (if (#2%not
                    (if (#2%vector? v.53)
                        (if (#2%= (#2%vector-length v.53) 3)
                            (#2%eq? test.49 (#2%vector-ref v.53 0))
                            #f)
                        #f))
                  (#2%error 'get-b "invalid record" v.53)
                  (#2%void))
              (#2%vector-ref v.53 2))))))))
> (let () 
    (define-disjoint-type (test make test?) 
      (fields (a immutable get-a) (b mutable get-b set-b!)))
    (list (make 3 4) (test? (make 3 4)) 
          (get-a (make 3 4)) (get-b (make 3 4)) 
          (let ((v (make 3 4))) (set-b! v 7) (get-b v))))
(#((test) 3 4) #t 3 4 7)
```
