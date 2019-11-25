## Abstract

A dictionary interface (''dictint'') is an object of a disjoint type that allows
a procedure to manipulate an object that maps keys to values
without having to know exactly how the object is implemented.
Such an object is called a dictionary (''dict'') in this SRFI,
and there is one dictint for each type of dict.

This SRFI provides
a number of accessors, mutators, and other procedures for dicts,
each of which takes a dictint and a dict argument in that order.
If the dictint argument is omitted, the default dictint is assumed.
(This is always safe, because dictints are disjoint from dicts.)

There are also constructors for dictints,
but no contructors for dicts: a dict must be constructed with
a type-specific constructor.

Finally, there are some precomposed dictints, a default dictint,
and a way to register new dictints as part of the default system.

## Mutability

All mutation procedures in this SRFI are in effect linear-update:
they return a dict which might either be newly allocated
or the same dict that was passed to the procedure.

## Basic dict procedures

The following procedures are fundamental to all dicts
and must be provided when constructing a new dictint.

(dict? [dictint] obj)

Answers #t if obj is a dict of the type specified by dictint.

(dict-contains? dictint dict key)

Returns #t if one of the keys of dict is key, and #f otherwise.

(dict-ref dictint dict key [failure [success] ])

If key is a key of dict, then invokes success on the corresponding value
and returns the result.
If key is not a key of dict, then invokes the thunk failure and
returns the result.

(dict-set dictint dict obj ...)

Returns a dict that contains all the associations of dict
plus those specified by objs, which alternate between keys and values.
If a key to be added already exists in dict, the new value prevails.

(dict-delete dictint dict key ...)

Returns a dict that contains all the associations of dict except those
whose keys are keys.

(dict-size dictint dict)

Returns an exact integer representing the number of associations in dict.

(dict-for-each proc dictint dict)

Invokes proc on each key of the dict and its corresponding value in that order.
Returns an unspecified value.

(dict-copy dictint dict)

Returns a copy of dict.  If dict is immutable, may return dict itself.

## Convenience dict procedures

These procedures can be built on top of the basic procedures
and provide many useful further abilities.  If they are not
specified to the dictint constructor, procedures will be provided
for each of them.

(dict-empty? dictint dict)

Returns #t if dict is empty and #f if it is not.

(dict-ref/default dictint dict key default)

If key is a key of dict, then returns the corresponding value.
If key is not a key of dict, then returns default.

(dict-adjoin dictint dict obj ...)

Returns a dict that contains all the associations of dict
plus those spsecified by objs, which alternate between keys and values.
If a key to be added already exists in dict, the old value prevails.

(dict-delete-all dictint dict keylist)

Returns a dict with all the associations of dict except those whose
keys are listed in keylist.

(dict-replace dictint dict key value)

Returns a dict that uses the same dictint and other properties of dict
and contains all the associations of dict except as follows:
If key is equal (in the sense of the comparator of dict) to an existing key of dict,
then the association for that key is omitted and replaced by the association
defined by the pair key and value. If there is no such key in dict,
then dict is returned unchanged.

(dict-intern dictinct dict key failure)

Extracts the value associated to key in dict, and returns two values, dict and the value.
If key is not contained in dict, failure is invoked on no arguments.
The procedure then returns two values, a dict that contains all the associations of dict
and in addition a new association that maps key to the result of invoking failure, 
and the result of invoking failure.

(dict-update dictint dict key updater [failure [success] ])

Semantically equivalent to, but may be more efficient than, the following code:

  (dict-set dictint dict key (updater (dict-ref dictint dict key failure success)))

The obvious semantics hold when success (and failure) are omitted (see dict-ref).

(dict-update/default dictint dict key updater default)

Semantically equivalent to, but may be more efficient than, the following code:

  (dict-set dictint dict key (updater (dict-ref/default dictint dict key default)))

(dict-pop dictint dict [failure])

Chooses an arbitrary association from dict and returns three values:
a dict that contains all associations of dict except the chosen one,
and the key and the value of the chosen association. 
If dict contains no associations and failure is supplied,
then the thunk failure is invoked and its values returned.
Otherwise, it is an error.

(dict-search dictint dict key failure success)

The dict dict is searched for an association with key key.
If it is not found, then the failure procedure is tail-called
with two continuation arguments, insert and ignore,
and is expected to tail-call one of them.

If an association with key key is found, then the success procedure is tail-called
with the matching key of dict, the associated value,
and two continuations, update and remove, and is expected to tail-call one of them.

It is an error if the continuation arguments are invoked,
but not in tail position in the failure and success procedures.
It is also an error if the failure and success procedures
return to their implicit continuation without invoking
one of their continuation arguments.

The effects of the continuations are as follows
(where obj is any Scheme object):

Invoking (insert value obj) returns a dict that
contains all the associations of dict,
and in addition a new association that maps key to value.

Invoking (ignore obj) has no effects.

Invoking (update new-key new-value obj) returns a dict that
contains all the associations of dict,
except for the association with key key,
which is replaced by a new association that maps new-key to new-value.

Invoking (remove obj) returns a dict that
contains all the associations of dict,
except for the association with key key.

In all cases, two values are returned:
the dict and obj.

