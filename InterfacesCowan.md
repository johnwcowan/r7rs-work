Interfaces are a new type of Scheme component distinct from programs, libraries, and REPL scripts. They consist solely of a `define-interface` declaration.  An interface specifies identifiers that will be exported from libraries as a group.

`(define-interface `*interface-name identifier-or-interface-name* ...`)`

Defines an interface by name.  Interface names are syntactically like library names; it is not an error if a library and an interface have the same name.  The identifiers to be exported may be specified individually or by mentioning the name of another interface, which implicitly specifies the identifiers and interfaces listed in that interface.

An implementation may resolve loops in the interface graph by flattening them, so that:

```
(define-interface (a) foo (b))
(define-interface (b) bar (a))
```

simply causes the interfaces `(a)` and `(b)` to export both `foo` and `bar`.

## Library declarations for interfaces

`(interface `*interface-name* ...`)`

The identifiers mentioned in the named interfaces are exported; it is an error if any of them are not defined by the library.  See "Interfaces" below.

`(require-interface `*interface-name* ...`)`

The implementation must not warn the user if the identifiers mentioned in the named interfaces are used but not defined in the library.  If these identifiers are used, but not defined in another library, further errors may of course result.
