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

Difference, semidifference (?), group and ungroup, summarize, tclose, quota for relations.

Remove, project, extend, rename for tuples?  Maybe not since they are not opaque.

Wrap and unwrap for both tuples and relations.

The usual aggregates plus ALL and ANY.

Count, fold, unfold, copy/materialize for sure: what other collection APIs.

Maybe types.

Read and write relvars from external systems.

## Tuples

A tuple is an alist mapping symbolic attribute names to values.
No types are associated with them.
The order of elements of the alist has no meaning.  It is an error to mutate a tuple.
Duplicate attribute names are not allowed.  Tuples also exist as parts of relations,
but their representation in a relation is not necessarily an alist.

(tuple? obj)

Returns #t if obj is a tuple and #f otherwise.

(tuple-ref tuple name)

Returns the value associated with the attribute name in tuple.

## Headings

A heading is also an alist mapping symbolic attribute names to symbolic type names.
Duplicate attribute names are not allowed.

## Relations

A relation has a heading that designates attribute names and their corresponding type names
and a set of tuples that have the same attribute names as those in the heading.
Relations are immutable and opaque.

These procedures also accept relvars in place of relations.

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

(relation-project relation list)

Returns a relation with only the attribute names given in list.
Duplicate tuples are removed.

(relation-remove relation list)

Returns a relation with all the attribute names that do not appear in list.
Duplicate tuples are removed.

(relation-rename relation alist)

List is an alist mapping attribute names in relation to new attribute names.
This procedure returns a relation in which all attribute names appearing
as the keys in alist
have been replaced with attribute names appearing as the corresponding values.
The names of any other attributes are left unchanged.

(relation-restrict relation pred)

Returns a relation with the same attributes as relation and a subset of the
tuples, those that satisfy pred.  The object passed to pred represents a tuple:
it may be an alist or a lightweight opaque object that can be passed
to tuple-ref.  In the latter case, it is only valid for the dynamic extent
of pred.

(relation-extend relation proc list)

Returns a relation with the same attributes as relation plus additional
attributes whose names appear in list.  Each tuple is passed individually
in no particular order to proc
which returns a list of the corresponding values of the named attributes.
The object passed to pred represents a tuple:
it may be an alist or a lightweight opaque object that can be passed
to tuple-ref.  In the latter case, it is only valid for the dynamic extent
of proc.

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

(relation-join-all relation-list)

Returns the join of all relations in relation-list.
Note that join is associative and commutative.
Joining a single relation returns it; joining no relations returns relation-dee.
This is not the set-theoretically correct answer in the case of intersection.

(relation-union relation1 relation2)

Returns a relation with the same heading as relation1 and relation2 and
all the unique tuples from both relations.

(relation-union-all relation-list)

Returns the union of all relations in relation-list.
Note that union is associative and commutative.
Unioning a single relation returns it; unioning no relations is an error.

### Set predicates

(relation=? relation1 relation2)

(relation<? relation1 relation2)

(relation>? relation1 relation2

(relation<=? relation1 relation2)

(relation>=? relation1 relation2

## Relvars

A relvar is a mutable object that holds a heading, which is fixed, the body of a relation,
which can be replaced by a different body, a set of relation keys, and
a set of foreign keys.  It is an error unless all keys and foreign keys have disjoint names.

As noted above, all procedures that accept relations also accept relvars.

(relvar? obj)

Returns #t if obj is a relvar and #f otherwise.

(make-relvar relation)

Return a newly allocated relvar whose heading and body are the heading and body of relation.
Thre are no keys or foreign keys.

(relvar-ref relvar)

Returns the relation stored in relvar.

(relvar-set! relvar relation)

Check that relvar and relation have the same heading.  If not, signal an error satisfying
rel-error?.  If so, replace the body of relvar with the body of relation.

## Key operations

(relvar-add-key! relvar key list)

Adds a new candidate key named key to relvar (or supersedes one that is there)
consisting of the attribute names in list.

(relvar-add-foreign-key! relvar key list other-relvar other-key)

Adds a new foreign key named key to relvar (or supersedes one that is there)
consisting of the attribute names in list, and links it to the key named
other-key in other-relvar (which can be the same as relvar).

(relvar-keys relvar)

Return a list of the names of the keys of relvar.  It is an error to mutate this list.

(relvar-foreign-keys relvar)

Return a list of the names of the foreign keys of relvar.  It is an error to mutate this list.

(relvar-key relvar key)

Return a list of attribute names that constitute the key named key.

(relvar-key-other-relvar relvar key)

Returns the relvar linked to the foreign key named key.

(relvar-key-other-key relvar key)

Returns the key linked to the foreign key named key.

(relvar-delete-key! relvar key)

Deletes the key or foreign key named key from relvar.

(relvar-foreign-key relvar key)

Return a list of attribute names that constitute the foreign key named key.

### Dee functions to investigate

Probably not needed.

def dictToTuple(heading, d):  
def validateHeading(heading):  
def constraintFromCandidateKeyFactory(r, Hk=None, scope={}):  
def constraintFromForeignKeyFactory(r1, (r2, map), scope={}):  
def constraintFromLambdaFactory(r, f, scope={}):  
def relationFromCondition(f):  
def relationFromExtension(f):  


