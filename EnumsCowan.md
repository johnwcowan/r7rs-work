## Rationale

Many procedures in many libraries accept arguments from a finite set (usually a fairly small one),
or subsets of a finite set to describe one or more modes of operation.
Offering a mechanism for dealing with such values fosters portable and readable code,
much as records do for compound values, or multiple values for procedures computing several results.

In Lisp-family languages, it is traditional to use symbols and lists of symbols for this purpose.
Symbols have at least two disadvantages:
they are not "type-safe", in the sense that a single symbol may be used in more than one
logically independent universe of flags; and in Scheme symbols do not have associated values.
In C-family languages, enumerations have names and numeric values, by default consecutive values,
but often powers of two or something externally dictated.
However, the name is not accessible at runtime, and enumeration types
are not really disjoint from integer types (in C++ they are statically distinct).

This SRFI instead provides something related to the *enums* of Java version 5 and later.
These are objects of a type disjoint from all others that are grouped into *enumeration types*
(called *enum classes* in Java).  In Java, each enumeration type is allowed to declare
the number and types of values associated with each object, but in this SRFI an enumeration
object has exactly one value; this is useful when translating from C to record the numeric value,
but has other uses as well.  The universes of R6RS correspond to enum types, but they are not reified.

In this SRFI, each enum has four properties:  the enum type to which it belongs, its name (a symbol),
its ordinal (an exact integer), and its value (any object).
An enum type provides access to all the enums that belong to it by name or ordinal.

*Enumeration sets* are used to represent multiple enums that belong to the same type.  They provide
a subset of the operations provided by [SRFI 113](http://srfi.schemers.org/srfi-113/srfi-113.html)
general sets.

Specialized mappings from enums to arbitrary values will be described
in a future SRFI.  Meanwhile, general-purpose hash tables from
[SRFI 125](http://srfi.schemers.org/srfi-125/srfi-125.html) or elsewhere, or
[SRFI 146](http://srfi.schemers.org/srfi-146/srfi-146.html) mappings can be used instead.

## Specification

### Enum type constructor

`(make-enum-type `*list*`)`

Returns a newly allocated enum type containing a fixed set of newly allocated enums.
Both enums and enum types are immutable, and it is not possible to create an enum
except as part of creating an enum type.

The elements of *list* are either symbols or two-element lists, where each
list has a symbol as the first element and any value as the second element.
Each list element causes a single enum to be generated, whose name is specified by the symbol.
It is an error unless all the symbols are distinct within an enum type.
The position of the element in *list* is the ordinal of the corresponding enum,
so ordinals within an enum type are also distinct.  If a value is given,
it becomes the value of the enum; otherwise the enum's value is the same as the ordinal.

### Enum accessors

`(enum-type `*enum*`)`

Returns the enum type to which *enum* belongs.

`(enum-name `*enum*`)`

Returns the name (symbol) associated with *enum*.

`(enum-ordinal `*enum*`)`

Returns the ordinal (exact integer) associated with *enum*.

`(enum-value `*enum*`)`

Returns the value associated with *enum*.

### Enum finders

`(enum-name->enum `*enum-type symbol*`)`

If there exists an enum belonging to *enum-type* named *symbol*, return it; otherwise return `#f`.

`(enum-ordinal->enum `*enum-type exact-integer*`)`

If there exists an enum belonging to *enum-type* whose ordinal is *exact-integer*, return it;
otherwise return `#f`.

Note: There is no way to find an enum by its value, since values need not be unique.

The following convenience procedures provide enum-finding followed by access to a property.

`(enum-name->ordinal `*enum-set symbol*`)`

Return the ordinal of the enum belonging to *enum-type* whose name is *symbol*.
It is an error if there is no such enum.

`(enum-name->value`*enum-set symbol*`)`

Return the value of the enum belonging to *enum-type* whose name is *symbol*.
It is an error if there is no such enum.

`(enum-ordinal->name `*enum-set exact-integer*`)`

Return the name of the enum belonging to *enum-type* whose ordinal is *exact-integer*.
It is an error if there is no such enum.

`(enum-ordinal->value `*enum-set exact-integer*`)`

Return the value of the enum belonging to *enum-type*  whose ordinal is *exact-integer*.
It is an error if there is no such enum.

### Enumeration types

`(enum-type-size `*enum-type*`)`

Returns an exact integer equal to the number of enums in *enum-type*.

`(enum-min `*enum-type*`)`

Returns the enum belonging to *enum-type* whose ordinal is 0.

`(enum-max `*enum-type*`)`

Returns the enum belonging to *enum-type* whose ordinal is equal to the number of enums
in the enum type minus 1.

`(enum-type-enums `*enum-type*`)`

Returns a list of the enums belonging to *enum-type* ordered by increasing ordinal.

`(enum-type-names `*enum-type*`)`

Returns a list of the names of the enums belonging to *enum-type* ordered by increasing ordinal.

`(enum-type-values `*enum-type*`)`

Returns a list of the values of the enums belonging to *enum-type* ordered by increasing ordinal.

### Enum objects

`(enum-next `*enum*`)`

Returns the enum that belongs to the same enum-type as *enum* and has an ordinal one greater than *enum*.
Returns `#f` if there is no such enum.

`(enum-prev `*enum*`)`

Returns the enum that belongs to the same enum-type as *enum* and has an ordinal one less than *enum*.
Returns `#f` if there is no such enum.

### Enum predicates

`(enum-type? `*obj*`)`

Returns `#t` if *obj* is an enum type, and `#f` otherwise.

`(enum? `*obj*`)`

Returns `#t` if *obj* is an enum, and `#f` otherwise.

`(enum-type-contains? `*enum-type enum*`)`

Returns `#t` if *enum* belongs to *enum-type*, and `#f` otherwise.

`(enum=? `*enum* ...`)`

Returns `#t` if all the arguments are the same enum, and `#f` otherwise.
It is an error to apply `enum=?` to enums belonging to different enum types.

`(enum<? `*enum* ...`)`

`(enum>? `*enum* ...`)`

`(enum<=? `*enum* ...`)`

`(enum>=? `*enum* ...`)`

These predicates return `#t` if their arguments are enums whose ordinals are in
increasing, decreasing, non-decreasing, and non-increasing order respectively, and `#f` otherwise.
It is an error unless all of the arguments belong to the same enum type.

### Comparators

`(make-enum-comparator `*enum-type*`)`

Returns a [SRFI 128](http://srfi.schemers.org/srfi-128/srfi-128.html)
comparator suitable for comparing enums that belong to *enum-type*.
The comparator contains both an ordering predicate and a hash function, and orders enums based on their ordinal values.

### Enum set constructors

`(enum-type->enum-set `*enum-type*`)`

Returns an enum set containing all the enums that belong to *enum-type*.

`(enum-set `*enum* ...`)`

Returns an enum-set containing the *enums*.  It is an error unless
all of them belong to the same enum type.

`(list->enum-set `*list*`)`

Returns an enum-set containing the members of *list*.  It is an error
unless all the members are enums belonging to the same enum type.

`(enum-set-project `*enum-type enum-set*`)`

Returns an enum set containing the enums belonging to *enum-type* that
have the same names as the members of *enum-set*, whose enum type
typically is not the same as *enum-type*.

`(enum-set-copy `*enum-set*`)`

Returns a copy of *enum-set*`)`.

### Enum set predicates

`(enum-set? `*obj*`)`

Returns `#t` if *obj* is an enum set and `#f` otherwise.

`(enum-set-contains? `*enum-set enum*`)`

Returns `#t` if *enum* is a member of *enum-set*.  It is
an error if *enum* does not belong to the same enum type
as the members of *enum-set*.

`(enum-set=? `*enum-set-1 enum-set-2*`)`

Returns `#t` if *enum-set-1* and *enum-set-2* have
the same members.  It is an error if the members of the
enum sets do not belong to the same type.

### Enum set mutators

These procedures are linear-update:
that is, they may or may not modify their *enum-set* argument,
and any existing references to it are invalidated.

`(enum-set-adjoin! `*enum-set enum* ...`)`

Returns an enum-set that contains the members of
*enum-set* and the *enums*.  It is an error if
the members of the result do not all belong to the same
enum type.

`(enum-set-delete! `*enum-set enum* ...`)`

Returns an enum-set that contains the members of
*enum-set* excluding the *enums*.  It is an error if
the members of the result do not all belong to the same
enum type.

`(enum-set-delete-all! `*enum-set list* ...`)`

Returns an enum-set that contains the members of
*enum-set* excluding the members of *list*.  It is an error if
the members of the result do not all belong to the same
enum type.

### Enum set operations

`(enum-set-size `*enum-set*`)`

Returns the number of elements in *enum-set*.

`(enum-set->list `*enum-set*`)`

Returns a list containing the members of *enum-set*.

`(enum-set-collect `*proc enum-set*`)`

Invokes *proc* on each member of *enum-set* in increasing
ordinal order.  The results are made into a list and returned.

`(enum-set-for-each `*proc enum-set*`)`

Invokes *proc* on each member of *enum-set* in increasing
ordinal order and discards the rest.  The result is
an unspecified value.

`(enum-set-fold `*proc nil enum-set*`)`

The current state is initialized to *nil*, and *proc*
is invoked on the current state and each element
of *enum-set* in increasing ordinal order, setting the
current state to the result.  The algorithm is repeated
until all the elements of *enum-set* have been processed.
Then the current state is returned.

`(enum-set-project `*enum-set-1 enum-set-2*`)`

Returns an enum-set containing the elements of *enum-set-1*
that have the same name as an element of *enum-set-2*.
It is permitted for the *enum-sets* to belong to different
enum types.

### Enum set logical operations

`(enum-set-union! `*enum-set-1 enum-set-2*`)`

Returns an enum-set containing all the elements of either
*enum-set-1* or *enum-set-2*.  It is an error if all
the elements of the result do not belong to the same
enum type.  The contents of *enum-set-1* may be
destroyed in the process.

`(enum-set-intersection! `*enum-set-1 enum-set-2*`)`

Returns an enum-set containing all the elements that
appear in both *enum-set-1* and *enum-set-2*.  It is an error if all
the elements of the result do not belong to the same
enum type.  The contents of *enum-set-1* may be
destroyed in the process.

`(enum-set-difference! `*enum-set-1 enum-set-2*`)`

Returns an enum-set containing the elements of
*enum-set-1* but not *enum-set-2*.  It is an error if all
the elements of the result do not belong to the same
enum type.  The contents of *enum-set-1* may be
destroyed in the process.

`(enum-set-xor! `*enum-set-1 enum-set-2*`)`

Returns an enum-set containing all the elements of either
*enum-set-1* or *enum-set-2* but not both.  It is an error if all
the elements of the result do not belong to the same
enum type.  The contents of *enum-set-1* may be
destroyed in the process.

