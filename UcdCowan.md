== Unicode Character Database ==

The Unicode Character Database (UCD) is a set of properties defined by the Unicode Standard and applicable to characters of the Unicode repertoire.  The exact list of properties varies from version to version of the UCD, so they are not enumerated here.  Instead, each property supported by a particular implementation of this package is represented by a ''property'' object belonging to a unique type.  Given a property (which can be retrieved in a variety of ways) and a character, the fundamental procedure `ucd-get-property-value` returns the value of that property when applied to that character.

Implementations may implement whatever subset of the UCD properties they choose.

Returned values must be treated as immutable by callers.

== Version procedure ==

`(ucd-version)`

Returns a list of three exact integers specifying the version of the UCD that this implementation provides.  There is no mechanism for providing more than one version.  If the UCD version is 5.0.0, the value of `(ucd-version)` is `(5 0 0)`.

== Properties ==

Properties can be thought of as analogous to symbols, but with multiple names.  Every property has a canonical name as well as zero or more aliases.  Unlike symbol names, property names are case-insensitive, and in addition the presence or absence of an underscore character in a name is not meaningful.  Because there is no way to construct a new property, property objects may be compared with `eqv?`.

Properties are typed in a way that reflects the type of the values for that property.  Some properties are numeric, some are string, some are boolean, and some are enumerated or catalog properties (the difference is that a catalog property typically gains new values in new UCD versions, whereas an enumerated property has a fairly closed set of values).

== Property procedures ==

`(ucd-find-property `''string''`)`

Returns the property object one of whose names is ''string'', or `#f` if there is no such property known to the implementation.

`(ucd-properties)`

Returns a list of all properties known to the implementation.

`(ucd-property? `''obj''`)`

Returns `#t` if ''obj'' is a property, and `#f` otherwise.

`(ucd-property-name `''prop''`)`

Returns a string which is the canonical name of ''prop''.

`(ucd-property-type `''prop''`)`

Returns one of the symbols `numeric`, `string`, `boolean`, `enumerated` or `catalog` to specify the type of ''prop''.

`(ucd-property-aliases `''prop''`)`

Returns a list of strings which are the aliases (including the canonical name) of ''prop''.  Names that merely differ in case or underscores from any of the others are not included.

`(ucd-default-value `''prop''`)`

Returns the Unicode-defined default value for ''prop'', or `#f` if there is none.

`(ucd-property-syntax `''prop''`)`

Returns a string whose value is a regular expression characterizing the valid syntax of all the values of the the property, or `#f` if no syntax is available.

=== Property predicates ===

There is a group of predicates whose semantics is specified by the Unicode Standard that specify a set of standard characteristics that a Unicode property may have.  They have names of the form `ucd-*-property?`, where `*` may be any of `obsolete deprecated stabilized numeric string binary enumerated catalogued  miscellaneous irg mapping dictionary-index reading dictionary-like radical-stroke variant normative informative contributory provisional`.  In all cases the return value is `#t` or `#f`.

== Enumerated property values ==

Property values which are booleans, numbers, or strings constitute no special problem.  Enumerated and catalogued property values, however, have canonical names and aliases and are subject to the same casing and underscore rules as properties.  With the exception of Unicode character names, therefore, they are represented by a disjoint object type called ''UCD-enums'', with procedures analogous to those for properties.  Property value names are not unique across properties.  Like properties, enums may be compared with `eqv?`.

UCD-enums associated with the `Character_Name` property have different name matching rules from other UCD-enums.

== Enum procedures ==

`(ucd-find-enum `''property''` `''string-or-integer''`)`

Returns the ucd-enum object associated with ''property'' one of whose names is ''string'', or `#f` if there is no such property known to the implementation.  The property named "Canonical_Combining_Class" has integer property values, but there are enumerated aliases for some of them, so in this case either a string or an integer may appear as the second argument.

`(ucd-enums `''prop''`)`

Returns a list of all UCD-enums associated with ''property''.

`(ucd-enum? `''obj''`)`

Returns `#t` if ''obj'' is a UCD-enum, and `#f` otherwise.

`(ucd-enum-name `''ucd-enum''`)`

Returns a string or integer which is the canonical name of ''ucd-enum''.

`(ucd-enum-property `''ucd-enum''`)`

Returns the property which is associated with ''ucd-enum''.

`(ucd-enum-aliases `''ucd-enum''`)`

Returns a list of strings (with a possible integer) which are the aliases (including the canonical name) of ''ucd-enum''.  Names that merely differ in case or underscores from any of the other names are not included.

== Retrieving property values ==

`(ucd-get-property-value `''codepoint''` `''prop''`)`

Return the boolean, string, number, or enum which represents the value of ''prop'' at ''codepoint'', which can be a character or an exact integer.

== Advanced UCD ==

See AdvancedUcdCowan for blocks, standardized variants, named sequences, CJK radicals, and emoji sources.
