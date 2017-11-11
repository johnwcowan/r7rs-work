# Title

WG1 Module Proposal

# Authors =

Aaron W. Hsu [arcfide] <arcfide@sacrideo.us>

# Abstract =

A simple two-level module system is proposed that permits fine grained
binding control at any level of Scheme code as well as providing
for the creation of complete packaging units for distribution and code
management. The system extends the Scheme language without introducing
any specialized contexts. Rather the system promotes the free use and
rebinding of these forms anywhere in Scheme code, permitting very
expressive extensions to the Core Scheme language.

# Issues =

* Should the import specifications be extended or reduced?

# Rationale =

A simple but expressive module system is needed that enables the
extension of Scheme without undue complexity. The charter indicates
that a module system for WG1 should be defined to "promote
extensibility." Extensibility ranges in more than one direction, and a
module system ought to address both the issues of packaging as well as
the issues of binding visibility and scope at all levels of Scheme
code.

# Specification

This document proposes the following forms:

* library
* module
* import
* export
* co-export
* implicit-export
* include
* source-directories [parameter]

The following syntax describes their general use.

```
<library> := (library <library name> <export> <import> <expression> ...)
<library name> := (<library name element> <library name element> ...)
<library name element> := <symbol> | <exact integer>

<module> := (module [<identifier>] <expression> <expression> ...)

<export> := (export <export-spec> ...)
<export-spec> := <identifier> 
               | (rename (<idientifier_1> <identifier_2>) ...)

<co-export> := (co-export <identifier> <identifier> ...)
<implicit-export> := (implicit-export <identifier> <identifier> ...)

<import> := (import <import-spec> ...)
<import-spec> := <identifier>
               | <library name>
               | (library <library name>)
               | (only <import-spec> <identifier> ...)
               | (except <import-spec> <identifier> ...)
               | (prefix <import-spec> <identifier>)
               | (rename <import-spec>
                         (<identifier_1> <identifier_2>) ...)

<include> := (include <string>)
```

A `library` form may only occur at the top-level of a program. The
`library name` is the unique identifier of the library defined by the
`library` form. This proposal extends the R6RS naming convention to
allow for exact integers as well as symbols.

A module form may occur at any definition context. That is, it may
appear anywhere that a definition may appear. The module form
evaluates to an unspecified value. When a module form with the optional
name element is encountered, the given identifier is visible as a
valid import for all import forms that occur within the scope where
the module form occurs. That is, the identifier is bound to a module
entity that may be imported using an import form. If no optional
identifier is given, then the exports associated with the module form
are exported and made visible in the scope where the module form
occurs.

Macros may expand into both module and library forms. In the case of
libraries, the libraries are only legal at the top-level.

Both modules and libraries may have exports associated with them using
the `export` form. Each export spec indicates one or more identifiers
that are exports from the library or module. When the export spec is
simply an identifier, then that identifier is exported. When the
export spec is a rename form, the latter identifier of each pair is
exported.

Exports apply to the nearest module or library form where they are
encountered. That is, if a library form encapsulates a module form
wherein occurs an export form, that export form applies to the module
form, rather than the library form which wraps or contains the module
form.

Identifiers may be associated with a set of co-exports or implicit
exports. In the case of co-exports, when an identifier with co-exports
is exported by an `export` form, then all of the co-exports associated
with that identifier are also exported. In the case of
implicit-exports, if a macro is bound to an identifier with
implicit-exports, if that identifier is exported, then the macro may
expand into identifiers that are implicitly exported without it being
an unbound identifier error. This allows bindings to be implicitly
visible to the outside world without requiring the module or library
form to explicitly export them. Rather, a macro may expand into them,
instead. By default, if no `implicit-export` form indicates the set of
implicit identifiers associated with a given syntax, and that syntax is
exported, then all of the definitions and bindings visible to the macro
are implicitly exported (this corresponds to the R6RS behavior).
However, if an `implicit-export` form does exist, the set of identifiers
implicitly exported for that syntax is completely and fully specified by
the `implicit-export` form. It is an error to have more than one
`implicit-export` form attaching names to a single syntax export.

The `import` form makes bindings exported by libraries or named
modules visible at some scope. An import form may occur anywhere a
definition may occur. The exports associated with the libraries and
modules referenced by the import form are made visible in the scope
where the import form occurs. By default, all of the exports are made
visible. However, the exports may be limited or renamed using only,
prefix, except, and rename, according to the R6RS rules. The `library`
form allows for the import of libraries whose names class with those
of the operations just listed.

It is sometimes useful to separate the source code of a library or
module from the declaration of that module or library. This can be
achieved by the use of the 'include' form. The include form expects a
single string as its only argument. The include form reads in the
forms that it finds by resolving the string to some location, possibly
using the `source-directories` parameter to do so. It expands into the
forms that it reads in, with each form wrapped with as if they had
been entered where the include occured. That is, the include form
wraps the incoming forms with its own wraps before expanding into
those forms.

The `source-directories` function behaves as if it had parameter
semantics, whether or not actual parameters are used to implement this
procedure. While other values may be stored, the `source-directories`
parameter is expected to at least contain a list of strings.
Implementations must at least support some interpretation of this
source-directories parameter value, and it is permissible for them to
ignore it.

Multiple bindings of the same name may not be imported into the same
scope. Moreover, the bindings exported by a `library` form are
considered to be immuatable, whereas the bindings exported by a
`module` form are immutable or mutable only based on their original
definition. That is, by default, if a binding is mutable inside the
module form, then it is also mutable outside of the module form when
exported and subsequently imported.

All module forms are expanded and evaluated once. In this respect they
are similar to LET forms that export definitions from inside their
form, and return `(void)` as their value. Bindings are visible in the
same contexts that they were defined in.

On the other hand, `library` forms may be evaluated many times,
possibly zero times, if an implementation may determine that the
library is not needed. Additionally, for forwards compatibility, it is
expected that exports from a library may be made visible in arbitrary
contexts, though this has no effect on WG1.

# Design Decisions

This system is explicitly designed to take advantage of and encourage
the full range of expressive power from syntactic abstractions. To
this end, the imports and exports are separate forms, rather than
being tied directly to the library and module forms. Moreover, it is
recognized that different applications require different semantics for
their modules, and thus the library and modules forms are defined to
have clearly distinct semantics that serve two different purposes in
the language.

A number of decisions were taken to allow for the efficient
implementation and compilation of libraries and modules. Firstly, the
use of the implicit-export form allows for the optimization of modules
without requiring whole program analysis, which is not always possible.
This encourage more dynamic programming without the associated cost of
inefficient code. Additionally, the ability for a library to never be
evaluated is a feature. This allows a more sophisticated
implementation to determine that a given library will never be used,
and to elide it from the final program, potentially eliminating many
costly evaluations and saving on space. Even on implicit phasing
systems, this does not cause a problem because it is still possible
to force the visitation or invokation of a library.

In the end, this system was designed to be both expressive and
efficient, while remaining as compatible as possible with existing
systems, and not straying from already well documented and implemented
semantics. The differences are largely syntactic, and the author is
not aware of any new semantic work in these forms.

# Compatibility =

## R5RS ==

R5RS does not specify a module system, but some Scheme implementations
provided their own. The `module` form is similar in construction to
Chez Scheme's module form.

## R6RS ==

A subset of R6RS libraries will run without modification in this
system. In particular, those libraries which do not take advantage of
explicit phasing and/or versioning are expected to run without
incident.

## WG1

Currently, I am in support of R6RS-- as the appropriate subset of this proposal for
WG1.

## WG2

This proposal allows for the handling of phasing issues, which is not
currently being discussed in WG1. Additionally, it permits any sorts
of extensions to the forms by the creation of new forms in the same
manner that `export` and `import` are defined. This system is
compatible with both explicit and implicit phasing provided that the
system is properly specified and clarified for those semantics.
>
