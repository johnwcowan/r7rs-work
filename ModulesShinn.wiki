== Title ==

WG1 Library Syntax

== Authors ==

Alex Shinn

== Abstract ==

A simple, static module syntax is provided which can easily be
implemented on top of existing module systems.  It's functionally
a subset of R6RS, equivalent to the R6RS-- option (i.e. R6RS
without versioning or phasing), differing by a minor syntactic
difference.

== Rationale ==

The charter explicitly requires a module system:

   To promote extensibility, the language developed by working group 1
   must include support for macros and modules in a way that is
   appropriate for the language's size and goals. Semantics compatible
   with interactive read/eval/print loops should be provided.

Module systems serve the purpose of encapsulating code and managing
namespaces.  Although `lambda` lets us manage local identifiers, it
does't allow us to encapsulate macros, and it wants for a friendlier
interface for typical "module-like" uses.

Anything beyond such a simple static definition of what a module
"looks like" can be difficult to impossible to implement in existing
module systems, much less integrate seamlessly with their own
modules.

== Specification ==

A module definition takes the following form:

{{{
  (library <module-name>
    <module-declarations>)
}}}

The keyword `library` is used for compatibility with R6RS - except for
this one keyword, this proposal will refer to modules (reserving the
option to change terminology at a later date, if the members so vote).

The module name is a list of symbols or exact integers specifying the
name of the module, for use when importing from other modules.  R6RS
does not allow non-symbol components, but as this is popular and
convenient for numeric concepts such as (srfi 1) and (rfc 821) it is
explicitly allowed.

The <module-declarations> can be any of:

  * (export <export-spec> ...)
  * (import <import-set> ...)
  * (include <resource-name> ...)
  * (body <module-body> ...)

Future standards may extend the syntax with additional forms.

`export` specifies a set of bindings to be exported.  Each
<export-spec> has one of the two forms:

  * <identifier>
  * (rename <local-identifier> <exported-identifer>)

where the former exports the identifier with the same name, and the
latter can be used to rename the identifier on export.

`import` specifies one or more modules to import into the current
module.  An <import-set> has one of the following forms:

  * <module-name>
  * (only <import-set> <identifier> ...)
  * (except <import-set> <identifier> ...)
  * (rename <import-set> (<old-identifier> <new-identifier>) ...)
  * (prefix <import-set> <prefix-identifier>)

By default, all of the bindings in a module's export clause are
imported with the same names (or the exported names if exported with a
`rename` form).  The additional <import-set> forms modify this as
identically to R6RS, as follows:

  * An `only` form produces a subset of the given <import-set>,
  including only the listed identifiers (after any renaming).  It is a
  syntax error if any of the listed identifiers are not found in the
  original set.

  * An `except` form produces a subset of the given <import-set>,
  excluding the listed identifiers.  It is a syntax error if any of
  the listed identifiers are not found in the original set.

  * A `rename` form modifies the given <import-set>, replacing each
  instance of <old-identifier> with <new-identifier>.  It is a syntax
  error if any of the listed <old-identifiers> are not found in the
  original set.

  * A `prefix` form automatically renames all identifiers in the given
  <import-set>, prefixing each with the specified identifier.

It is a syntax error if the same identifier is imported twice, from
any combination of modules or multiple `import` forms.

`body` and `include` declarations provide a means to specify the
Scheme code that makes up the body of the module.  `body` takes the
forms literally and splices them as a `begin`, whereas `include` takes
one or more file names, reads all the top-level forms in the files,
and splices the results.  The forms from all `body` and `include`
declarations are expanded in the order in which they occur in the
module declaration.

The bodies of an imported module may be evaluated multiple times, but
will be evaluated at least once before the body of any module which
imports it is evaluated.  It is a syntax error if any set of modules
form a cyclic import dependency.

== REPL Interaction ==

To load a module into
an interactive REPL, a REPL should extend the standard environment
with an import form that works as within a module declaration,
but importing the bindings directly into the REPL environment.

This has disadvantages as opposed to an out-of-bounds means
such as that provided by Scheme48, but is easy to remember,
allowing users to pick up new implementations quickly, and
also mirrors the top-level program syntax, allowing cut&paste
of scripts.

Multiple imports of the same identifier in the same `import` is
still a syntax error, however identifiers may later be rebound by
subsequent `import`s.

== Top-Level Programs ==

Top-level programs function the same as in R6RS - a top-level
import form is allowed as in the REPL, and the command-line
arguments are available with the `command-line` procedure.

== Implementation ==

This is a trivial syntactic variation of a subset of R6RS.  Any
static module system (including R6RS) would need to add support
for the varied syntax.  A syntactic module system could support
it with a wrapper macro expanding into the native module form.
A meta module system could support it with a new meta form
expanding into the native module form.

The syntactic variation was inspired by Scheme48, which removes
the ugliness of the completely fixed syntax in R6RS (both `export`
and `import` are required, and their order can't be changed),
allows room for extensions to be made easily, and even opens
the possibility of forwards compatible extensions by simply
ignoring unknown module forms.

== Compatibility ==

=== R5RS ===

The R5RS has no module system, so implementations all provide their own
incompatible module systems.  To work together and with WG1 standard
Scheme implementations, they should support the specified library syntax
in addition to (or in place of) their current syntax.

=== R6RS ===

The syntax is largely a subset of the R6RS `library` form, with the
exception that there is no implicit body.  To support WG1 library forms,
the core library (e.g. `(rnrs 7)`) should provide two macros,
`include` and `body`.  `body` can be defined simply as an alias for
`begin`:

{{{
  (define-syntax body
    (syntax-rules ()
      ((body x ...) (begin x ...))))
}}}

`include` can't be defined with `syntax-rules`, but can in a low-level
macro system.  The R6RS library specification gives a sample
definition of `include`.

=== WG2 ===

WG2 will likely need to address phasing issues, and provide the
equivalent of the R6RS `for` form.

== Example ==

The stack library from the R6RS specification:

{{{
  (library (stack)
    (export make push! pop! empty!)
    (import (rnrs))
    (body
     (define (make) (list ’()))
     (define (push! s v) (set-car! s (cons v (car s))))
     (define (pop! s)
       (let ((v (caar s)))
         (set-car! s (cdar s))
         v))
     (define (empty! s) (set-car! s ’()))))
}}}

== Issues ==

  * Phasing is irrelevant in the absense of low-level macros, and is thus
  ignored.

  * Versioning is left out entirely.  This is an important feature for
  practical application development, not to be ignored lightly, but the
  author felt that experimentation with it in current Scheme
  implementations is insufficient for standardization at the current
  time.

  * The keywords used are open for debate - `library` is chosen for
  compatibility with R6RS, but I'm happy to take a vote for switching
  this or any others.

  * The body of the library when placed in the same file is indented a
  single extra character as compared to library forms with implicit
  bodies.  A separator syntax `---` could remove this extra space as
  described in ModuleSyntax.

  * Forwards compatibility of non-essential forms (declarations,
  optimizations and other meta-info) could be achieved by specifying
  that any unknown library form simply be ignored.  However this can
  cause difficulties for extensions that _are_ essential.

  * We still need to choose the default module hierarchy for the WG1 core.

  * With either R5RS or R6RS identifier syntax, it is possible for both
  a prefix to be a valid identifier, and an export to be a valid
  identifier, but the resulting concatenation invalid.  For example,
  `-` is a valid prefix, but prepended to `foo` results in `-foo`
  which is not a valid literal.  We need to note this in the
  description of prefix, or modify the symbol syntax so that this
  can't occur.  This is especially tricky because `i` with a prefix of
  `-` results in `-i`, a number.

== References ==

  * [[http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_chap_7|R6RS Modules]]
  * [[http://community.schemewiki.org/?scheme48-module-system|Scheme48 Modules]]
  * [[http://www.scheme.com/csug/syntax.html|Chez Modules]]
  * [[http://practical-scheme.net/gauche/man/gauche-refe_32.html|Gauche Modules]]
