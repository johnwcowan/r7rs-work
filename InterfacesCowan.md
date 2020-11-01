## Abstract

Interfaces are a new type of Scheme component
distinct from programs, libraries, and REPL scripts.
They consist solely of a `define-interface` declaration.
An interface specifies identifiers
that can be exported from libraries as a group.

## Rationale

Identifiers provide a higher-level representation
of the export-import graph of a program or REPL script
and its libraries.
If a component specifies the interfaces that it requires,
then any imported libraries that provide
those interfaces can be replaced as long as the new
imports also provide those interfaces.

## Specification

If an interface is named but no definition is available,
or an interface is required but not implemented,
an expand-time error is signaled.

If an interfaces is required by a component but not implemented
anywhere in the import-export graph,
an expand-time error is signaled.

This SRFI recommends but does not require
that a source file containing an interface definition
be given the extension `.sif`.
This extension is used in a number of other contexts,
but those uses would be unlikely to collide with this one.

`(define-interface `*interface-name identifier-or-interface-name* ...`)`

Defines an interface, a collection of identifiers.
Interface names are syntactically like library names,
but belong to a separate namespace.

It is an error for a `define-identifier` to appear
within a library, program, or REPL script; it is a fourth
type of top-level component.

The identifiers may be mentioned individually
or by mentioning the name of another interface,
which implicitly mentions all the identifiers and interfaces
mentioned directly or indirectly in that interface.
An expand-time error is signaled if an interface
is mentioned but no definition is available.

It is not an error to mention an identifier
both explicitly and through an interface,
or through two or more different interfaces.
Loops in an interface graph are resolved
by flattening them, so that:

```
(define-interface (a) foo (b))
(define-interface (b) bar (a))
```

simply causes the interfaces `(a)` and `(b)` to mention both `foo` and `bar`.

## Library declarations for interfaces

`(implement-interface `*interface-name* ...`)`

If any of the identifiers mentioned
directly or indirectly are not exported
by the library, an expand-time error is signaled.
Otherwise, no action is taken.

It is not an error for an identifier
to be mentioned directly or indirectly in more than
one of the named interfaces.

`(require-interface `*interface-name* ...`)`

If the identifiers mentioned
directly or indirectly in the named interfaces
are not imported into it,
an expand-time error is signaled.
Otherwise, no action is taken.

It is not an error for an identifier
to be mentioned directly or indirectly in more than
one of the named interfaces.

## Extension to `cond-expand`

This SRFI requires an additional type of feature requirement
in `cond-expand` declarations and expressions, the
`interface` feature.  It is exactly equivalent
syntactically and semantically
to the `library` feature, except that it checks for the
existence of an interface rather than a library.

## Implementation

Chicken provides `define-interface` and `export-interface`
with different syntax: see
[Chicken modules](http://wiki.call-cc.org/man/5/Modules)
for details.
