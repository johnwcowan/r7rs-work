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
and in addition a new association mapping key to the result of invoking failure, 
and the result of invoking failure.




