This is a proposal to add further library declarations and import-set types to the large language.

**Issue:  Phases are not yet dealt with.**

Issue: Earlier versions of this proposal included type declarations for identifiers; these have been omitted.

## Library declarations

In R7RS it is not an error to provide a library declaration that a specific implementation does not understand, although implementations should provide warnings in such cases.  Most of these declarations specified here use "should", which means that implementations are allowed to silently ignore them if they are inappropriate.  Conflicts between declarations are resolved in an implementation-specific way.  Exceptions:

* Support for `reexport`, `export-all` and `specify-declarations` is required.

* Support for `notinline` is required for implementations that make the decision to inline code.

### Name control (required)

`(reexport `*import-set* ...`)`

Like `import`, but also exports all identifiers that were imported.  This allows a library to easily extend one or more other libraries.

`(export-all `*identifier* ...`)`

Exports all defined identifiers except for *identifiers*, if any.  Imported identifiers are not re-exported..  This allows a library to be easily extended.

### Optimization

In the list below, *importance* represents a symbol, one of `unimportant` (no importance), `neutral` (some importance, the default), `important` (substantial importance), or `extreme` (extreme importance).  It is an error to specify anything else.  These correspond to the Common Lisp importance values 0, 1, 2, and 3 respectively.

If no identifiers are named, then the library declaration applies to all identifiers defined in the library.  Implementations may (but should not) ignore the named identifiers and treat the declaration as applying to all identifiers.

`(speed `*importance* *identifier* ...`)`

The named *identifiers* should be processed so as to maximize execution speed to the extent specified by *importance*.

`(space `*importance* *identifier* ...`)`

The named *identifiers* should be processed so as to minimize the use of space at run time to the extent specified by *importance*.

`(safety `*importance* *identifier* ...`)`

The named *identifiers* should be processed so as to maximize safety at execution time to the extent specified by *importance*.

`(compilation-speed `*importance* *identifier* ...`)`

The named *identifiers* should be processed so as to maximize compilation speed to the extent specified by *importance*.

`(debuggability `*importance* *identifier* ...`)`

The named *identifiers* should be processed so as to maximize ease of debugging to the extent specified by *importance*.

### Errors

`(library-error `*literal-string*`)`

Signals an error when the library is first examined.  It is meant to be used within `cond-expand` library declarations to trigger an error if some feature or library is found not to be present.  Applications cannot count on being able to catch this error with exception handlers or guards.

### Compilation

If no identifiers are named, then the library declaration applies to all identifiers defined in the library.  Implementations may (but should not) ignore the named identifiers and treat the declaration as applying to all identifiers.

`(inline `*identifier* ...`)`

The procedures named by the *identifiers* should be inlined by the compiler, if there is one.

`(notinline `*identifier* ...`)`

The procedures named by the *identifiers* must not be inlined by the compiler, if there is one.

`(unused `*identifier* ...`)`

The implementation should not warn the user if the *identifiers* are defined but not used in the library.

`(report ` [`terse`|`silent`](`verbose`) *identifier* ...`)`

The implementation should process the definitions of the named identifiers verbosely, tersely, or silently.  Even in silent mode, however, the user should still be notified of errors.

### Semantics

If no identifiers are named, then the library declaration applies to all identifiers defined in the library.  Implementations may (but should not) ignore the named identifiers and treat the declaration as applying to all identifiers.

`(no-location `*identifier* ...`)`

The implementation may assume that the procedures bound to *identifier* have no location, so that neither `eq?` nor `eqv?` is required to return `#t` in any circumstances when passed a such a procedure as an argument.

## Declarations in main programs

To allow library declarations in main programs (not in files of code included by `include` or `include-ci`), the following new syntax is added:

`(specify-declarations `*declaration* ...`)`

Allows any library declaration except `export` and `reexport` to be included as part of a main program.   It is an error for `declare` to appear except at the beginning of a main program.  For backward compatibility with R7RS-small, programmers should avoid wrapping `import`, `include`, `include-ci`, or `begin` declarations in `specify-declarations`.

The more natural term `declare` is avoided because some implementations use it for related but implementation-specific uses.

## Import sets

`(drop-prefix `*import-set* *identifier*`)`

Import sets of this kind automatically renames identifiers in the given *import-set*: any of them which begin with the characters in *identifier* have those characters removed.

For example, if the library `(foo)` exports identifiers `foo-a`, `foo-b`, and `foo-c`, then `(import (drop-prefix (foo) foo-)` will import them under the names of `a`, `b`, and `c` respectively.  (Chibi Scheme already supports this type of import set.)

## Features

`(provide `*feature* ...`)`

As a consequence of loading this library, the specified *features* are made immediately available to future `cond-expand` library declarations, to `cond-expand` syntax, and to the `features` procedure.

## See also

See also [InterfacesCowan](InterfacesCowan.md).

