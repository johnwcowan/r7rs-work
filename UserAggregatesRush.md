## Title

WG1 User Defined Aggregate Types

## Authors

David Rush

## Abstract

There is a significant fraction of the Scheme programming community which is looking for more expressive tools for data abstraction. When this desire is brought into contrast with the minimalist facilities of the existing Scheme Reports it can be difficult to reconcile the expectations many programmers have from more mainstream languages. SRFI-9 provides a facility which occupies a low-energy "sweet spot" in terms of semantic complexity and expressiveness, although it is unfortunately still somewhat primitive - lacking primarily in extensibility and integration with other Scheme facilities, most notably READ and WRITE.

Since most type-checking in Scheme is performed at run-time through the evaluation of predicate functions, the issues of type extensibility and integration ultimately come down to the the relationships expressible through those type predicate functions. This leads to an examination of the type structures within Scheme and a slight reorganization of the type predicates to enable a richer data abstraction facility.

## Issues

* generative vs named
* opacity of ports
* relationship to {{{VALUES}}}
* EQ? vs EQV? and extensibility

## Rationale

Scheme provides a variety of primitive types from which program values are all taken. The majority (how many?) of them are atomic types, with little or no internal structure. The remaining primitive types aggregate multiple atomically typed values. Prior to SRFI-9 and R6RS there was no further type information available concerning Scheme values; however those documents introduced quasi-visible mutability types to certain classes of values. R5RS also allowed (but did not require) implementations to make literal constant aggregates (in particular, with cons cells) immutable, but provided no way to determine if a cons cell was actually mutable prior to actually attempting to change its value. This proposal takes the minimalist and REPL-friendly approach and provides no guarantees of immutability to aggregates by adopting the approach that all aggregate types are essentially specializations of vectors.

It is worth noting that lists are not an aggregate type which is addressed by this proposal, as lists are a recursive data type defined in terms of the primitive {{{pair?}}} aggregate type.

The primary goal of this proposal is to ease the writing of type predicates. To that end we provide a minimal algebra on <type-id>s. The algebra only allows sub-typing, as Scheme's top-level union type transparently handles all issues of union or recursive types without need for further machinery. This proposal provides almost no policy and minimal machinery for the use of <type-id>s withing Scheme programs. In effect, <type-id>s are merely a specialized form of GENSYM, but they provide a significant aid to accurately expressing and manipulating the type relationships within Scheme programs.

## Specification

* {{{atom?}}} => <boolean>
* {{{aggregate?}}} => <boolean>

These predicates split the type space of Scheme into two essential categories: types which are fully opaque, and types which are composed of multiple atomic objects. There is no guarantee that any accessors for an {{{aggregate?}}} object will be in scope.

* {{{make-aggregate-type}}} [<name>] => <type-id>
* {{{extend-aggregate-type}}} <type-id> [<name>] => <type-id>

These functions provide access to type identifiers within the implementation. If the <name> parameter is specified, the implementation will provide a unique <type-id> associated with the <name>. When a <name> is not specified the implementation will generate a new unique <type-id>.

There is effectively a separate name space for any <name>s associated with <type-id>s. The type name space primarily facilitates exploratory programming via a REPL, allowing data structures to remain valid across multiple reloads of the same source code. Generative usage of the type API is also supported by explicitly binding un-named <type-id>s to lexically visible names within Scheme code via the usual mechanisms.

* {{{type<=?}}} <type-id> <type-id> => <bool>
* {{{type>=?}}} <type-id> <type-id> => <bool>

These functions allow programs to easily establish the relationship between two <type-id>s. A <type-id> is considered greater than another <type-id> if it is more specialized (via {{{extend-aggregate-type}}}). Equality of <type-id>s is determined through the usual Scheme predicates.

* {{{make-aggregate}}} <type-id> <slots> => <aggregate>
* {{{aggregate-values}}} <type-id> [...](<value>) => <aggregate>

Create a new {{{aggregate?}}} value. The {{{make-aggregate}}} form creates a value where there are the number of slots (specified by <slots>) initialized with {{{#undefined}}}, while the {{{aggregate-values}}} form has a number of slots equal to the number of <value>s provided in the call. The values given zero-relative indices from left to right for access via {{{aggregate-ref}}} and {{{aggregate-set!}}}.

* {{{aggregate-length}}} <aggregate> => <exact integer>

Returns the number of slots that may be accessed in an aggregate by {{{aggregate-ref}}} and {{{aggregate-set!}}}.

* {{{aggregate-ref}}} <aggregate> <slot> => <value>
* {{{aggregate-set!}}} <aggregate> <slot> <value> => <value>

{{{aggregate-set!}}} return the value that the <slot> held prior to the mutation.

* {{{aggregate-type}}} <pair> => <pair-type-id>
* {{{aggregate-type}}} <vector> => <vector-type-id>
* {{{aggregate-type}}} <string> => <string-type-id>
* {{{aggregate-type}}} <aggregate> => <type-id>
* {{{aggregate-type}}} <name> => <type-id>
* {{{aggregate-type}}} <values> => <values-type-id>

## Top-Level Programs

## Implementation

## Compatibility

### R5RS

### R6RS

### WG2

