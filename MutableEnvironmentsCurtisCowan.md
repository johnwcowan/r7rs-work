This proposal provides mutable environments for R7RS-large.  It's based on [this proposal by Pavel Curtis](http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1988/msg00139.html), but he is not responsible for the use I have made of it.

## Introduction

The small language provides four procedures that return global environment specifiers for use by `eval`.  They are:

* `environment`, which returns an immutable environment specifier when passed on zero or more import-specs which contains the bindings made available by those import specs

* `scheme-report-environment`, which when passed the argument `5` returns a specifier for a copy of the R5RS environment, which may or may not be mutable

* `null-environment`, which when passed the argument `5` returns a specifier for a copy of the R5RS environment containing syntax keywords only, which may or may not be mutable

* `interaction-environment`, which returns a mutable environment specifier containing implementation-defined bindings, including at least those exported by the base library

## Procedures

The following procedures allow an application to generate, examine, and mutate environment specifiers which can be used like those obtained from the R7RS-small procedures above.

## Constructor

`(make-environment `*environment* ...`)`

Returns a newly allocated mutable environment specifier that has imported into it the bindings of the *environments*.

## Predicates

`(environment? `*obj*`)`

Returns `#t` if *obj* is an environment specifier, and `#f` otherwise.

`(mutable-environment? `*obj*`)`

Returns `#t` if *obj* is a mutable environment specifier, and `#f` otherwise.

`(environment-bound? `*environment symbol*`)`

Returns `#t` if *symbol* is bound in the environment specified by *environment*, and `#f` otherwise.

`(environment-syntax-keyword? `*environment symbol*`)`

Returns `#t` if *symbol* is bound as a syntax keyword in the environment specified by *environment*, and `#f` otherwise.

`(environment-assigned? `*environment symbol*`)`

Returns `#t` if *symbol* is bound as a variable and is assigned a value in the environment specified by *environment*, and `#f` otherwise.

## Accessors

`(environment-ref `*environment symbol*`)`

If *symbol* is bound as a variable that has been assigned a value in the environment specified by *environment*, returns the value bound to it.  If *symbol* is bound as a syntax keyword, returns an implementation-defined object which specifies whatever is bound to it such that it can be passed to `environment-set!`.  If *symbol* is not bound or not assigned, returns `#f`.

`(environment-imports `*environment*`)`

Returns a list of environments that have been imported into *environment*.  It is an error to mutate this list.

## Mutators

`(environment-bind `*mutable-environment symbol syntax-keyword?*`)`

Binds *symbol* in *mutable-environment* as a syntax keyword or variable.  The value is unassigned.

`(environment-import! `*mutable-environment environment* ...`)`

Modifies *mutable-environment* to import *environments*.  It is an error if any symbol is imported from more than one environment, either specified or already imported into *mutable-environment*.  Any symbols bound in *mutable-environment* shadow symbols in the imported environments.

`(environment-unimport! `*mutable-environment environment* ...`)`

Modifies *mutable-environment* to not import *environments*.  Unimporting an environment that is not imported has no effect.

`(environment-set! `*mutable-environment symbol value*`)`

In *mutable-environment *, assigns *symbol* (which must be bound as a variable or syntax keyword) to *value*.  It is an error if *symbol* is bound to a syntax keyword but *value* is not derived from a call on `environment-ref` passing an already existing syntax keyword.  Returns an unspecified value.

`(environment-remove! `*mutable-environment symbol*`)`

In the mutable environment specified by *environment*, removes any binding for *symbol* created by `environment-set!`, revealing any imported binding.  If there is no such binding, it does nothing.  Returns an unspecified value.

`(environment-freeze! `*environment*`)`

Causes *environment* to become an immutable environment.

## The whole environment

`(environment-for-each `*environment proc*`)`

Invokes *proc* on each identifier bound in the environment specified by *environment* whose value is not imported.  *Proc* is passed the identifier and the value (or an unspecified value if the identifier is bound as a syntax keyword or unassigned).  Note that imported identifiers when the environment was created are not passed to *proc* unless their bindings or values have been changed.