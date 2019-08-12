## Unicode Character Database

The Unicode Character Database (UCD) is a set of properties defined by the Unicode Standard
and applicable to characters of the Unicode repertoire.
The exact list of properties varies from version to version of the UCD,
so they are not enumerated in this SRFI.
Instead, each property supported by a particular implementation of this package
is represented by a library named`(srfi FIXME propname)`.
All the libraries except `(srfi FIXME meta`) export the same procedures, so if you want
more than one of them, import each one with a distinct prefix.

(Insert exact mapping from Unicode property names to library names here:
coerce to lower case, change underscores to hyphens, etc.)

The fundamental procedure `get-value` from a given property library returns
the value of that property when applied to that character.

Implementations may implement whatever subset of the UCD properties they choose.

Returned values must be treated as immutable by callers.

## Meta library procedures

`(ucd-version)`

Returns a list of three exact integers specifying the version of the UCD
that this implementation provides.
There is no mechanism for providing more than one version.
If the UCD version is 5.0.0, the value of `(ucd-version)` is `(5 0 0)`.

`(ucd-properties)`

Returns a list of the names of all properties known to the implementation.
This primarily exists for documentation purposes.

## Per-property library procedures

These procedures exist with the same names in each library other than the meta library.

`(property-name)`

Returns a string which is the canonical name of *prop*.

`(property-type)`

Returns one of the symbols `numeric`, `string`, `boolean`, `enumerated` or `catalog`
to specify the type of this property

`(property-aliases)`

Returns a list of strings which are the aliases (including the canonical name) of *prop*.
Names that merely differ in case or underscores from any of the others are not included.

`(default-value)`

Returns the Unicode-defined default value for *prop*, or `#f` if there is none.

`(property-syntax)`

Returns a string whose value is a regular expression characterizing the valid syntax
of all the values of the the property, or `#f` if no syntax is available.

### Property predicates

There is a group of predicates whose semantics is specified by the Unicode Standard
specifying standard characteristics that a Unicode property may have.
They have names of the form `*-property?`, where `*` may be any of:

```
obsolete deprecated stabilized numeric string binary enumerated catalogued
miscellaneous irg mapping dictionary-index reading dictionary-like radical-stroke
variant normative informative contributory provisional
```

In all cases the return value is `#t` or `#f`.

## Enumerated property values

Property values which are booleans, numbers, or plain strings constitute no special problem.
Enumerated and catalogue properties, with the exception of character name properties,
have canonical forms and aliases and therefore
some special procedures associated with them.

## Enumerated and catalog procedure values

`(canonicalize-value `*string-or-integer*`)`

Returns the canonical name of a string or integer that serves as a property value,
given either an alias or an alternative form differing only in case or underscores.
Normally strings are used as both input and output to this procedure.
However, The property named `Canonical_Combining_Class` has canonical integer property values
with string aliases for some of them,
so in this case either a string or an integer may appear as the argument, and
an integer will be returned.

`(canonical-value? `*obj*`)`

Returns `#t` if *obj* is a canonical property value, and `#f` otherwise.

## Retrieving property values

`(get-value `*codepoint*`)`

Return the boolean, string (in canonical form if it has one), or number
that represents the value of the property at *codepoint*, which can be
specified with a character or an exact integer.

## Advanced UCD

See [AdvancedUcdCowan](AdvancedUcdCowan.md)
for the block, standardized variant, named sequence, CJK radical, and emoji source properties.
