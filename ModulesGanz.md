# Title

WG1 Module Proposal

# Authors =

Steven Ganz [steven_ganz] <steven.ganz@gmail.com>

# Abstract =

A module syntax is provided that is general enough to allow imports to be specified when a module is included into a context.  Modules have separate namespaces, and can import and export variables and macros with fine-grained control.  Restrictions (not fully specified here) can be imposed on the use of these forms in macros to allow for varying levels of static analysis.


# Rationale =

Modules should be true abstractions over syntax.  The charter indicates
that a module system for WG1 should be defined to "promote extensibility."  Extensibility is enhanced by the ability to reuse modules while varying their imports.

# Specification

This document proposes the following forms that can occur in any context where a definition is allowed (modulo any restrictions on their use in macro definitions) and that evaluate to an unspecified value:

* module
* include-module

The following syntax is provided:

```
<module> := (module [<module-name>] (import <import-spec> ...) (export <export-spec> ...)                 
               <expr> <expr> ...)

<import-spec> := (include-module <include-sets>)
               | <identifier>

<export-spec> := <identifier>
               | (<identifier> [[(indirect-export|<identifier> ...)]])

<include> := (include-module <include-sets>)

<include-sets> := <module-name>
                | (only <include-sets> <export-identifier> ...)
                | (except <include-sets> <export-identifier> ...)
                | (prefix <include-sets> <export-identifier> ...)
                | (rename <include-sets> (<old-identifier> <new-identifier>) ...)
```


A module has an optional name that, if specified, is added to the following namespace and that can be referenced from within an `include-spec`.  A module specifies identifiers and modules to import and identifiers to export.  It has at least one body expression.  The exported identifiers of a module must be defined in the module body.  The imported identifiers (both explicit and via an <include-sets>) may appear free in the module body.

Modules may be included in three ways:
* If no module name is given, then the exports associated with the module form are made visible to then namespace following the module form.
* They may be included into any definition context, in which case their exported identifiers become visible to the following namespace as bound in the body of the included module.
* As described above, they may be imported into another module, in which case their imported identifiers become imported identifiers of that module and their exported identifiers become visible to the body of that module as bound in the body of the included module.

In either case, the import identifiers (restricted and renamed as described below) must be bound in the current scope.  This implies that for a module definition at top level, imported modules should have no such import identifiers.  The export identifiers are then considered bound in the following scope to their definitions in the module body.

Upon inclusion of a module, its sets of imported and exported identifiers can be modified as follows:

* An `only` form produces a subset of the given import set,
> including only the listed identifiers (after any renaming).  It is a
> syntax error if any of the listed identifiers are not found in the
> original set.

* An `except` form produces a subset of the given import set,
> excluding the listed identifiers.  It is a syntax error if any of
> the listed identifiers are not found in the original set.

* A `rename` form modifies the given import set and/or export set, replacing each
> instance of <old-identifier> with <new-identifier>.  It is a syntax
> error if any of the listed <old-identifiers> are not found in either
> original set.

* A `prefix` form automatically renames all identifiers in the given
> <import-set, prefixing each with the specified identifier.

It is a syntax error if the same identifier is imported twice, from
any combination of modules or multiple `import` forms.


Macros may be restricted to preclude expanding into module and/or include-module forms.

Finally, there are two forms for exporting identifiers from a module.  The simple form just specifies the identifier.

The more complex form associates an identifier with a set of indirect exports.  If a macro is bound to an identifier with implicit-exports and that identifier is exported, then the macro may
expand into identifiers that are implicitly exported without it being
an unbound identifier error. This allows bindings to be implicitly
visible to the outside world without requiring the module or library
form to explicitly export them. Rather, a macro may expand into them,
instead. By default, if no `implicit-export` form indicates the set of
implicit identifiers associated with a given syntax, and that syntax is
exported, then all of the definitions and bindings visible to the macro
are implicitly exported (this corresponds to the R6RS behavior).
However, if an `indirect-export` form does exist, the set of identifiers
implicitly exported for that syntax is completely and fully specified by
the `indirect-export` form.


# Example Usage

```
(module monad-common (import star) (export mon-let)
   (define-syntax mon-let
      (syntax-rules ()
         [[(mon-let|[x rhs]] body)
          ((star (lambda (x) body)) rhs)])))

(module output-list-monad (import) (export unit mon-let)
   (define unit
      (lambda (x) `(,x . ())))
   (define star
      (lambda (recvr)
         (lambda (comp)r
            (let ([[a|(car comp)]]
                  [[d|(cdr comp)]])
               (let ([[pr|(recvr a)]])
                 `(,(car pr) . ,(append d (cdr pr))))))))
   (include-module monad-common))

(module env-monad (import) (export unit mon-let)
   (define unit
      (lambda (x)
         (lambda (env)
            x)))
   (define star
      (lambda (recvr)
         (lambda (comp)
            (lambda (env)
               (recvr (comp env))))))
   (include-module monad-common))

(include-module (prefix output-list-monad ol-))
(include-module (prefix env-monad env-))
...
```

Note that it is only necessary to define mon-let once and we can retrieve two versions.


# Other Issues

The following are not considered in this proposal, but could potentially be considered in extensions/refinements of this proposal:

* Mutability of bindings

* How many times module forms may be evaluated

* Import from files

Also, the precise restrictions on macros, if any, are not specified here.


# Compatibility =

The forms provided here are not unlike those of Chez's module system, but the flexibility they provide is more like units from PLT Scheme.  Although that flexibility is supported, the more commonly supported "hard-wiring" of modules is also supported here.  Much of the notation for modifying identifier sets is derived from R6RS.
