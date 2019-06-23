## Issues

The natural representation of types is as SRFI 128 comparators.  However, when
communicating with the outside world, we need type names instead, as comparators
contain opaque procedures that cannot be serialized.   Where should the relationship
between comparators and names be kept?  Does it make sense to have a single namespace
for all named comparators?

Similarly, when executing restrict or extend operations remotely, we cannot in general
pass an opaque Scheme closure; we need an S-expression that can be compiled to
the remote system's native expressions.

Do we want relations with computed rather than explicitly stored bodies?  If so, how?

## Editorial

relation-all?, relation-any?

Count, fold, unfold, copy/materialize, for-each, partition (?)

Maybe types?

Constraints: domains, keys, foreign keys.

Read and write relations from external systems.

## Tuples

A tuple is a mapping from symbolic attribute names to arbitrary values.
[Need to figure out best representation.]
The order of the attributes has no meaning.  It is an error to mutate a tuple.
Duplicate attribute names are not allowed.  Tuples also exist as parts of relations,
but their representation in a relation is not necessarily the same as that
of a "freestanding" tuple.

(tuple? obj)

Returns #t if obj is a tuple according to the above rules and #f otherwise.

(tuple-degree tuple)

Returns the number of attribute-value pairs in tuple as an exact integer.

(tuple-ref tuple name)

Returns the value associated with the attribute name in tuple.

(tuple-project tuple namelist)

Returns a smaller tuple with only the elements whose names appear in namelist.

(tuple-remove tuple namelist)

Returns a smaller tuple with all elements except those whose names appear in namelist.

(tuple-extend tuple name proc)

Passes tuple to proc and adds a new element with the specified name and the value
returned by proc.

(tuple-rename tuple alist)

Alist is an alist mapping attribute names in relation to new attribute names.
Returns a tuple in which all attributes whose names appear as keys in alist
are replaced by attributes whose names appear as the corresponding values.

## Headings

A heading is also an alist mapping symbolic attribute names to symbolic type names.
Duplicate attribute names are not allowed.

## Relations

A relation has a heading that designates attribute names and their corresponding type names
and a set of tuples that have the same attribute names as those in the heading.
Relations are immutable and opaque.

### Constants

relation-dee

A relation with no attributes and a single empty tuple.

relation-dum

A relation with no attributes and no tuples.

### Constructor

(make-relation heading list)

Returns a relation whose heading is heading and whose body consists of the tuples in list.

### Predicates

(relation? obj)

Returns #t if obj is a relation and #f otherwise.

(relation-contains relation tuple)

Returns #t if tuple is equal to a tuple in relation and #f otherwise.

### Accessors

(relation-heading relation)

Returns the heading of relation.

(relation-degree relation)

Returns an exact integer which is the number of attributes in relation.

(relation-size relation)

Returns an exact integer which is the number of tuples in relation.

(relation-body relation)

Returns the body of relation in the form of a list of tuples.

### Relational algebraic operations

For all these operations, any duplicate tuples that would otherwise exist
in the result are quietly removed.

(relation-project relation list)

Returns a relation with only the attribute names given in list.

(relation-remove relation list)

Returns a relation with all the attribute names that do not appear in list.

(relation-rename relation alist)

Alist is an alist mapping attribute names in relation to new attribute names.
This procedure returns a relation in which all attribute names appearing
as the keys in alist
have been replaced with attribute names appearing as the corresponding values.
The names of any other attributes are left unchanged.

(relation-restrict relation pred)

Returns a relation with the same attributes as relation and a subset of the
tuples, those that satisfy pred.  The object passed to pred represents a tuple;
it is an error to access this tuple outside the dynamic extent of pred.

(relation-extend relation proc list)

Returns a relation with the same attributes as relation plus additional
attributes whose names appear in list.  Each object passed to pred represents a tuple;
it is an error to access this tuple outside the dynamic extent of pred.
The result of pred is a tuple mapping the new attribute names to their values.

(relation-join relation1 relation2)

Returns a relation with one attribute for each non-overlapping attribute
of relation1 and relation2 and one for each overlapping attribute.
The tuples are generated as if the following algorithm is used: create a
tuple-like object by appending each tuple from relation1 with each tuple
from relation2 (a variant of a Cartesian product).  Then eliminate all
tuple-like objects which differ in any of the overlappingly named attributes.

This procedure provides the intersection of its arguments if all attributes overlap, the
modified Cartesian product if none overlap, and the natural join otherwise.

(relation-semijoin relation1 relation2)

Joins relation1 and relation2 and removes all attributes unique to relation2.

(relation-antijoin relation1 relation2)

Joins relation1 and relation2 and removes all overlapping attributes.
Also known as compose.

(relation-join-all relation-list)

Returns the join of all relations in relation-list.
Note that join is associative and commutative.
Joining a single relation returns it; joining no relations returns relation-dee.
This is not the set-theoretically correct answer in the case of intersection.

(relation-union relation1 relation2)

Returns a relation with the same heading as relation1 and relation2 and
all the tuples from both relations.  If relation1 and relation2
have different headings, an error satisfying relation-error? is signaled.

(relation-union-all relation-list)

Returns the union of all relations in relation-list.
Note that union is associative and commutative.
Unioning a single relation returns it; unioning no relations is an error.

(relation-difference relation1 relation2)

Returns a relation with the same heading as relation1 and relation2 and
all the tuples in relation1 but not relation2.  If relation1 and relation2
have different headings, an error satisfying relation-error? is signaled.

(relation-semidifference relation1 relation2)

TBD

(relation-map proc relation)

Accepts a relation and returns a relation with the same heading, where
each tuple of the input has been passed to relation-map and the result
placed in the output relation.  The object passed to pred represents a tuple;
it is an error to access this tuple outside the dynamic extent of pred.

(relation-group relation list name)

TBD

(relation-ungroup relation name)

(relation-summarize ...)

TBD

(relation-tclose ...)

TBD

(relation-quota ...)

TBD

### Set predicates

Note that the following three predicates do not obey the trichotomy law.

(relation=? relation1 relation2)

Returns #t if relation1 and relation2 contain the same tuples and #f otherwise.

(relation<? relation1 relation2)

Returns #t if relation1's tuples are a proper subset of relation2's tuples
and #f otherwise.

(relation>? relation1 relation2

Returns #t if relation1's tuples are a proper subset of relation2's tuples
and #f otherwise.

relation<=? relation1 relation2)

Returns #t if relation1's tuples are a subset of relation2's tuples
and #f otherwise.

(relation>=? relation1 relation2

Returns #t if relation1's tuples are a subset of relation2's tuples
and #f otherwise.

## Constraints

[explain them]

## Key operations

(relation-valid? relation alist)

Alist is a mapping from names to SRFI 128 comparators.
Uses the type-valid procedure associated with an attribute
name to check the values associated with the named attribute
[blah blah explain better]

(relation-has-key? relation keylist)

Returns #t if the names in keylist constitute a key of relation,
and #f otherwise.

(relation-has-foreign-key? relation1 keylist1 relation2 keylist2)

Returns #t if the names in keylist1 constitute a key of relation1, the
names in keylist2 constitute a key of relation2, and the join of
relation1 and relation2, after appropriate renaming so that their
key attributes have the same names, has the same cardinality as relation1.

(### Dee functions to investigate

Probably not needed.

def validateHeading(heading):  
def relationFromCondition(f):  
def relationFromExtension(f):  


