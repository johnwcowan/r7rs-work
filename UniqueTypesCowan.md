Unique run-time types

## Abstract

This SRFI is intended to standardize a primitive run-time mechanism to create disjoint types.

## Rationale

This SRFI provides a simple hook to create new data types at run time that are disjoint from all existing types.  allowing portable libraries to implement SRFI 9, SRFI 99, SRFI 131, SRFI 135, R6RS records, Chicken records, CLOS, persistent databases, remote access to data on servers, and the like on top of it.  It is also portably implementable and usable entirely separately from any of these.

Note that there is no concept of a type object here: a type is simply a name for a group of closely linked procedures that allow the creation and manipulation of type instances (which are objects) and subtypes.  This SRFI exposes no ambient authority, and relies entirely on module exports for access control.  It is based on a less radical proposal by Alaric Snell-Pym, [UniqueTypesSnellPym](UniqueTypesSnellPym.md).

## Specification

### Make-type

`(make-type ` *type-payload*`)` → *type-accessor constructor predicate accessor make-subtype*

Calling `make-type` on *type-payload*, which can be any Scheme object, returns five values, all of which are procedures.  They are distinct (in the sense of `eqv?`) from each other and from any other procedures returned by other calls to `make-type`.  In brief, the five functions:

* return *type-payload*

* return newly allocated objects of a disjoint type known as *instances*, each associated with an *instance payload*

* return `#t` iff an object is an instance of this type

* return the instance payload

* return five more procedures associated with a subtype of this type

Details are given for a sample type in the next section.  The type payload might contain metadata (such as field names or class variables) associated with the type as a whole.

## Sample procedures for a type

For the purposes of this section, we will suppose that `(define-values (reia-metadata make-reia reia? reia-ref make-reia-subtype) (make-type 'reia))` has been evaluated, and document each of the five variables that it binds.  "Reia" is an acronym for "remarkably 'evil' in appearance", and has no particular significance.  Fnord!

`(reia-metadata)` → *object*

Returns the symbol `reia`.

`(make-reia `*instance-payload*`)`→ *reia*

Returns a newly allocated instance associated with *instance-payload*.  This association is single and immutable, but it is possible to make use of an appropriate container payload in order to effectively associate the instance with more than one value.  To make the association effectively mutable, use a mutable payload such as a box, list or vector.  Instances belong to a type that is disjoint from any existing Scheme type, including types created by other calls to `make-type`.

`(reia? `*object*`)`→ *boolean*

Returns `#t`  iff *object* was returned by a call to `make-reia` or any constructor created as part of a direct or indirect subtype of the `reia` type.

`(reia-ref `*reia*`)`→ *object*

Returns the instance payload of *reia*.  It is an error if *reia* does not satisfy `reia?`.

`(make-reia-subtype `*type-payload*`)`→ *type-accessor constructor predicate accessor make-subtype*

Returns five new procedures with the same semantics as `make-type`, such that the objects returned by *constructor* satisfy `reia?`.

## Implementation

TBD

