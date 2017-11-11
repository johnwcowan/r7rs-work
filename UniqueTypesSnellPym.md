
# Background

I feel that records are too complex and controversial and varied for standardisation in WG1.

We all love records, but there's a number of ways of doing them.

There's widespread consensus that defining a record type FOO with fields X, Y, and Z should result in procedures FOO?, FOO-X, FOO-Y, FOO-Z, MAKE-FOO, and sometimes FOO-X-SET!, FOO-Y-SET! and FOO-Z-SET!; however, there's less consensus about definition forms, let alone more esoteric features like purely functional mutators, constructs that open up a record by creating a lexical environment in which X is bound to (FOO-X <record>) and so on, etc.

Perhaps most importantly, records need to be distinct types. If you implement them in terms of vectors, everything seems to work fine, but a subtle kind of hygiene is broken. If somebody writes a function that dispatches on type for some reason, and they have a case for handling vectors that comes before the case for some record type, then the vector case will be triggered unexpectedly. Oh, noes!

Also, there is potential variation in implementation. It's widely accepted that programming languages should generally support records in the sense of first-class values in memory, but third-party libraries (or the outer reaches of a more sprawling language) may well want to implement a record-like interface - at least FOO? FOO-<field>, FOO-<field>-SET! *and type disjointness* - to things like persistent data in a database, data accessed via some network protocol, and other such forms. Clearly, Thing One already allows the definition of the procedures, and type disjointness is all we need 'added'.

And, obviously, any manner of in-memory record abstraction can be implemented with suitable macros - if we have a means of forming disjoint types.

Therefore, I propose that WG1 should standardise a primitive mechanism to create disjoint types, allowing portable libraries to implement SRFI-9, SRFI-99, R6RS records, Chicken records, CLOS, persistent databases, remote access to data on servers, and the like; WG2 should probably pick or create a record standard, but that's not my problem (and I'm happy either way, as I can have whatever record system I fancy as a portable library anyway).

It is possible to build arbitrary record-like disjoint types on top of (eg) SRFI-9 records, by exposing the `FOO?` procedure and wrapping the accessors and (where applicable) constructors and mutators, but this gives me a non-jewel-like feeling.

# The meat of the proposal

The semantics of such a system are fairly simple and obvious, and the syntax used in the Kernel programming language seems as good as any. Slightly altered for Schemier style, here it is:

```
(make-encapsulation-type <size>)
```

Returns three or more values: procedures `e`, `p?` and `d`, and perhaps some implementation-defined extra ones after that. Each call to `(make-encapsulation-type)` returns different procedures.

* `e` is a procedure that takes <size> arguments, and returns a fresh encapsulation with those arguments as the content. Different calls to `e` produce encapsulations that are not `eq?`, but will be `equal?` if their contents are equal? and they were both produced by `e`.

* `p?` is a procedure that takes one argument, and returns `#t` if the argument is an encapsulation that was returned by a call to `e`, and `#f` in all other cases.

* `d` is a procedure that takes two arguments. If the first is an encapsulation that was returned by a call to `e`, and N is an exact integer more than or equal to zero and less than <size>, then the Nth content of the encapsulation (where N is the second argument) is returned. Otherwise, an error is signalled.

```
(make-encapsulation-type <size> <symbol>)
```

As above, but annotates the type with a descriptive symbol. The implementation is free to ignore the symbol, but may use it to aid debugging or providing printable representations of the encapsulated type. However, implementors be warned that the users are in no way obliged to make descriptive symbols be unique, so don't go using it in ways that would assume this.

# Written form

One of the reasons to have a unique type encapsulation system is for security, to prevent access to the contents of an object, and to prevent objects of that type being constructed by arbitrary code.

Therefore, the result of calling `write` on an encapsulation should not reveal the contents of the encapsulation's fields by default, and `read` should be incapable of creating encapsulations by default.

However, mechanisms for attaching `read` and `write` semantics to encapsulations should be addressed by WG2 or an SRFI. My quick proposal is that it should be possible to attach encode/decode procedures to an encapsulation type, which map an instance of the encapsulation type to a list of fields, and the inverse; then `write` should produce syntax something like `#<symbol>(<...list of fields returned by encoder...>)`, where the `<symbol>` is the registered name of the type (and must be unique), and `read` should parse that syntax by calling the decoder function registered under that symbolic name, with the list of parsed fields.

How to maintain uniqueness of type-name symbols globally is left as an exercise for the reader.

# Optional extension: Type Inheritance (please vote as `snellpym+inheritance` instead of `snellpym` if you want this)

```
(make-encapsulation-subtype <e> <d> <extra-size> [<symbol>])
```

Creates a subtype T2 of an existing encapsulation type T1. The parent type's `e` and `d` procedures must both be provided to demonstrate the caller's existing ability to construct and deconstruct the parent type (`p?` is really just a convenience, that could be crafted from `d` and a condition handler, so access to it need not be proven). The returned values are as per `make-encapsulation-type`, except that the <size> is equal to the parent's <size> plus <extra-size>; however, the definition of `make-encapsulation-type`s return values (and, therefore, the return values of `make-encapsulation-subtype`) is extended such that `p?` also returns true for encapsulations created by any subtype's `e` procedure, and `d` will work for encapsulations created by subtypes' `e` procedures, and when called on an encapsulation created by a subtype's `e` procedure, will work for indices up to `(+ <parent-size> <extra-size>)`.

# Optional extension: Per-Field Mutability (append `+mutate` to your vote if you want this)

```
(make-mutable-encapsulation-type <mutaflags> <symbol>)
```

Creates a mutable encapsulation type. Rather than specifying an exact integer size, the caller instead provides a list, the length of which is the number of contents (<size>). If the list entry corresponding to a content is #f, then that content is immutable; otherwise, it is mutable.

An extra return value, `m!`, is returned; it is a procedure of three arguments. If the first is an encapsulation that was returned by a call to `e`, and N is an exact integer more than or equal to zero and less than <size>, and the Nth content of the encapsulation (where N is the second argument) is mutable, then the Nth content of the encapsulation is mutated to become the third argument. Otherwise, an error is signalled.

```
(make-mutable-encapsulation-subtype <e> <d> <m!> <extra-mutaflags> [<symbol>])
```

If `snellpym+inheritance+mutate` wins, then you also get this extra procedure for FREE. Its semantics are what you might expect. The `extra-mutaflags` are appended onto the end of the parents' mutaflags.

`m!` is an optional argument of sorts - if #f is supplied (as it is not available or the parent type isn't actually a mutable record), then the returned subtype `m!` considers all the parent contents to be immutable and can only mutate contents explicitly marked as mutable in the `extra-mutaflags`

# Rationale

I originally specified a single "content item" in the encapsulations, suggesting that users drop a vector in there if they wanted to, but without loss of generality (and only a little loss of simplicity), implementations can easily avoid an extra indirection by providing an embedded vector for free.

I have made the encapsulation types have a hard-coded size when the type is created, in order to avoid requiring implementations to store a type descriptor *and* a length in every instance of that type. Variable-length behaviour can be had by embedding a vector created at instance creation time, if so desired.
