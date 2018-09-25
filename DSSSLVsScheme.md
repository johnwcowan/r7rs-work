[DSSSL](http://en.wikipedia.org/wiki/Document_Style_Semantics_and_Specification_Language), the Document Style Semantics and Specification Language, is a language for specifying stylesheets for SGML documents, and is semantically an ancestor of XSLT.  It includes as part of itself something called the [expression language](http://vrici.lojban.org/~cowan/dsssl/), a large subset (with extensions) of R4RS Scheme.  This page tells you in a coarse way (see the previous link for details) what the differences are between the two.

Here's the list provided at the top of Clause 8:

* The expression language uses only the functional, side-effect free subset of Scheme. Features of Scheme that are not useful in the absence of side-effects have been removed (for example, `begin, set!, eqv?`)
* The vector data type is not provided.
* A character object is uniquely identified by its name and properties.
* The number data type is a subtype of a more general quantity data type that adds the concept of dimension to a number.
* `call-with-current-continuation` is not provided.
* Some optional features of R4RS are not provided.
* The `gcd` and `lcm` procedures are not provided.
* Keyword arguments are provided.

The syntax keywords and procedures provided by the expression language and its defined subset, the core expression language, are enumerated [here](http://tinyurl.com/feature-groups).

Here are finer details:

* The list of syntactic keywords is not extensible.
* You may not redefine a defined identifier.
* Improper lambda-lists are not allowed.
* A definition may refer to identifiers that are defined in later definitions, not only within `lambda`-expressions, but anywhere.
* Internal definitions are equivalent to `letrec`.
* You may redefine a built-in identifier such as `car`, and such redefinitions are pervasive.
* `Case` uses `equal?` to match keys, since `eqv?` does not exist.
* Numbers are restricted to exact integers and inexact rationals.
* Exact integers must include the range -2^31^-1 to 2^31^-1 and may be larger.
* `#i` and `#e` syntax are not supported.

DSSSL extensions:

* Complex `lambda`-lists are provided, with flags `#!optional, #!rest, #!key`.  These three are self-defining named constants when used outside `lambda`-lists.
* Keywords are runtime objects, with procedures `keyword?, keyword->string, string->keyword`.
* Quantities are a superset of numbers: the primitive unit is meters, written `m` (e.g. `1m` for 1 meter).  Standard derived units are `cm mm in pt pica`, and new derived units (but not new primitive units) may be added with `define-unit`.
* Numeric literals with a decimal point, an exponent, or a unit are inexact; all others are exact. 
* The procedures `exact?, inexact?, =, <, >, <=, >=, zero?, positive?, negative?, odd?, even?, max, min, +, -, *, /, abs atan` (with two arguments), `sqrt` apply to quantities as well as numbers.
* `quantity->number` strips the unit from a quantity.
* `format-number` converts a number to a string containing ordinary numbers, numbers with leading zeros, letters, or Roman numerals.  `format-number-list` applies to a
* Characters have (immutable) property lists, where the properties are keywords: one property is `numeric-equiv:`.
* Language objects specify collation and case conversion; procedures are `language?, current-language, with-language`.
* Non-Scheme procedures can be accessed with `external-procedure`, which returns a corresponding Scheme procedure, which should be pure functional.
* Date and time procedures are provided: `time` returns the current time since the 1970 Epoch as an integer; `time->string, time<?, time>?, time<=?, time>=` generate and compare ISO 8601 timestamp strings.
* `error` signals an error; it has one argument, a string. 
