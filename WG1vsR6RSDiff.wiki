== R5RS Incompatibilities ==

The one instance where we deliberately broke compatibility with R5RS
in favor of R6RS is by defaulting the reader to case-sensitivity.
We also use the R6RS `#!fold-case` and `#!no-fold-case` reader extensions
for changing the case-sensitivity, and have structured the module
system to allow using existing case-insensitive R5RS source
unmodified.

== R6RS Base Incompatibilities ==

The module system does not support phase distinctions, which are
unnecessary in the absense of low-level macros (see below), nor
versioning, which we feel is an important feature but deserves more
experimentation before standardizing.

In addition, the syntax of the module system was deliberately chosen
to be syntactically different from R6RS, using `define-library` instead of
`library` and putting an extra level of indirection around the body,
for the following reasons:

  * Allows easy disambiguation between R6RS and R7RS modules.

  * Makes it easier to `include` separate files, optionally with the
    `include-ci` form to case-insensitive files.

  * Provides the `cond-expand` form from SRFI 0, allowing for a more
    deterministic alternative to the R6RS ".impl.sls" file naming
    convention.

  * Allows room for extensibility.  The R6RS syntax provides two
    positional forms which must be present and must have the correct
    keywords, `export` and `import`, which does not allow for
    unambiguous extensions.  We think extensibility is important
    (we've already added three forms in the points above), and so
    chose a syntax which provides a clear separation between the
    module declarations and the Scheme code which makes up the body.

Since the system is simple, it is expected that R6RS implementations
will be able to support the `module` syntax in addition to their
`library` syntax.

The modularization of standardized identifiers is different from
the R6RS system.  In particular, procedures which are optional either
expressly or by implication in R5RS have been removed from the base
module.

Identifier syntax is not provided.  We feel this is a useful feature
in some situations, but the existence of such macros means that
neither programmers nor
other macros can look at an identifier in an evaluated position and
know it is a reference -- this in a sense makes all macros slightly
weaker.  We'd like to see individual implementations continue
experimenting with this and other extensions before standardizing.

Internal syntax definitions are allowed, but all references to syntax
must follow the definition -- the even/odd example given is R6RS is not
allowed.

The R6RS exception system was incorporated as-is, but the condition types
have been left unspecified.  Specific errors that were required to be
signalled in R6RS remain "an error" in R7RS, allowing implementations
to provide their own extensions.  The condition system and stricter
semantics may reappear in the large language or a later SRFI or
standard.  There is no discussion of "safety."

We do not require full Unicode support, but requiring implementations
to be consistent with Unicode in the characters that they do support.
Case conversions are described in terms of the Unicode
locale-independent mappings, and instead of explicit normalization
forms we provide `string-ni=?` etc. for an implementation-defined
normalization form (which may be the identity transformation).
Character comparisons are defined by Unicode, but string comparisons
are implementation-dependent, and therefore need not be the lexicographic
mapping of the corresponding character comparisons (an incompatibility
with R5RS).  Non-Unicode characters are permitted.

The full numeric tower is optional as in R5RS, but support for IEEE
infinities, NaN, and -0.0 were adopted from R6RS.  Most clarifications on
numeric results were also adopted, but the new procedures
`real-valued?`, `rational-valued?`, and `integer-valued?` were not.
The R5RS names `inexact->exact` for `exact` and `exact->inexact` for
`inexact` were retained, with a note indicating that their names are
historical.

The division operators `div`, `mod`, `div-and-mod`, `div0`, `mod0` and
`div-and-mod0` have been replaced with a full set of 18 operators
describing 6 rounding semantics.

When a result is unspecified, it is still required to be a
single value, in the interests of R5RS compatibility.  However,
non-final expressions in a body may return any number of values.

In the interest of the widespread SRFI 1 support and extensive code
using it, the semantics of `map` and `for-each` have been changed to
use the SRFI 1 early termination behavior instead of an error.
Likewise `assoc` and `member` take an optional `equal?` argument as in
SRFI 1, instead of the separate `assp` and `memp` procedures from
R6RS.

We adopted the R6RS `quasiquote` clarifications, but have not seen
convincing enough examples to allow multiple-argument `unquote` and
`unquote-splicing`.

The `case` macro has been extended with a `=>` syntax analogous to
that in `cond`.

We've kept R5RS `#` inexact "placeholder" digits in the
interest of backwards compatibility, but not adopted the R6RS
mantissa widths.

== R6RS Library Incompatibilities ==

The low-level macro system and syntax-case were not adopted.  There
are two general families of macro systems in widespread use -- the
syntax-case family and the syntactic-closures family -- and they have
neither been shown to be equivalent nor capable of implementing each
other.  Given this situation we feel we cannot choose one over the
other, and so leave low-level macros to the large language.

The new I/O system from R6RS was not adopted.  We feel a completely
new system deserves a period of usage before being standardized, and
were unhappy with the backwards-compatibility "simple I/O" which
introduced a redundant API and relegated R5RS code to being a
second-class citizen.  Instead we added support for binary ports as
disjoint from character ports, but in the same style as existing ports
which is what most R5RS implementations currently do.

Our string ports are compatible with SRFI 6 rather than R6RS;
analogous bytevector ports are also provided.

We felt the R6RS records system was overly complex, and the two layers
poorly integrated.  We spent a lot of time debating this, but in the
end decided to simply use a generative version of SRFI 9, which has
near-universal support among implementations.  We hope to provide a
more powerful records system in the large language.

We have not included enumerations in the small language.

We add R6RS bytevectors, providing only the "u8" procedures in the
small language, but keeping the SRFI 4 #u8(...) syntax, acknowledging
that with a module system it's easier to change names than reader
syntax.

We provide an interface to the current system time in terms of TAI.

The utility macros `when` and `unless` are provided, but since it
would be meaningless to try to use their result we leave the result
unspecified.

We could not agree on many issues with hash tables and have left them
for the large language.  We've also left sorting and bitwise
arithmetic to WG2.

We did not relegate pair and string mutators to separate modules.

== Certain Additions (Technical Incompatibilities) to R6RS ==

We've added support for CL-style #<n>=(... #<n># ...) reader labels.
`write` is required to detect cycles and use these labels in this
case.  The new `write-simple` (name pending) is added for when the
programmer does not want to output reader labels.

We extend `syntax-rules` with `_` as a general wildcard as in R6RS,
but make explicit that both `_` and `...` can be used in the literals
list in which case their usual behavior is overridden.  We allow the
same pattern language extensions, as well as the SRFI 46 ellipsis
specifier syntax.

We provide a new `syntax-error` form for signalling friendly
compile-time errors from syntax-rules macros.

We added parameters compatible with R6RS (and SRFI 39), but in the
interest of thread-safety and conflicting behavior among existing
implementations left the semantics of mutating a parameter (passing it
an argument) unspecified.  Parameters in the small language can be
altered only with the `parameterize` form.

`equal?` is required to always terminate, and to be true if `eqv?` is true.
