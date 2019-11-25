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

(dict-count pred dictint dict)

Passes each association of dict as two arguments to pred
and returns an exact integer that counts the number of times
that pred returned true.

(dict-any? pred dictint dict)

Passes each association of dict as two arguments to pred
and returns true when one of the calls to pred returns true.
If all calls return false, dict-any? returns false.

(dict-every? pred dictint dict)

Passes each association of dict as two arguments to pred
and returns #f when any of the calls to pred return false.
If all calls return true, dict-every? returns true.

(dict-keys dictint dict)

Returns a list of the keys of dict.

(dict-values dictint dict)

Returns a list of the values of dict.  The results returned
by dict-keys and dict-values are ordered consistently.

(dict-entries dictint dict)

Returns two values, the result of calling dict-keys and the
result of calling dict-values.

(dict-map proc dictint dict)

Returns a dict containing the keys of dict and the values that result
from invoking proc on the keys and corresponding values of dict.

(dict-fold proc nil dictint dict)

Invokes proc on each association of dict with three arguments:
the key of the association, the value of the association,
and an accumulated result of the previous invocation. 
For the first invocation, nil is used as the third argument.
Returns the result of the last invocation,
or nil if there was no invocation.

(dict-map->list proc dictint dict)

Returns a list of values that result from invoking proc
on the keys and corresponding values of dict.

(dict-filter pred dictint dict)  
(dict-remove pred dictint dict)

Returns a similar dict that contains just the associations of dict
that do /do not satisfy pred.

(dict->alist dictint dict)

Returns an alist whose keys and values are the keys and values of dict.

## Possible additional procedures

These functions will take O(n²) time when applied to O(n) dicts.
Issue: should we have them?

Subset functions:  dict=? dict<? dict>? dict<=? dict>=?

Set theory operations: dict-union, dict-intersection, dict-difference, dict-xor

## Dictionary interface constructors

All the following procedures return dictints that provide
all the dict procedures of this SRFI.

(make-basic-dictint dict? dict-contains? dict-ref dict-set
                    dict-delete dict-size dict-for-each dict-copy)

Returns a dictint object that provides access to the eight procedures
shown above and through them to the other procedures of this SRFI.
However, the native procedures of the type of dictionary which this
dictint provides may be more efficient than the synthesized procedures
that using this constructor will provide.

(make-dictint dictint dict)

Returns a dictint object that provides access to all the procedures
provided in dict, which maps the names of the procedures of this SRFI
to suitable type-specific procedures.  The eight procedures listed
in the description of make-basic-dictint are required: the others
are optional.

(make-alist-dictint pred)

Returns a dictint object that allows alists to be treated as dict,
searching them using pred.
Mutations are pushed onto the front of the alist.

## Standard dictionary interface objects

eq-alist-dictint

A dictint object that allows alists to be treated as dicts,
using eq? to search them.
Mutations are pushed onto the front of the alist.

eqv-alist-dictint

A dictint object that allows alists to be treated as dicts,
using eqv? to search them.
Mutations are pushed onto the front of the alist.

equal-alist-dictint

A dictint object that allows alists to be treated as dicts,
using equal? to search them.
Mutations are pushed onto the front of the alist.

plist-dictint

A dictint object that allows property lists (lists with alternating keys and values,
and where all the values are symbols) to be treated as dicts.
Mutations directly mutate the list.

hash-table-dictint

A dictint object that allows SRFI 125 hash tables to be treated as dicts.
Mutations directly mutate the hash table.

mapping-dict

A dictint object that allows SRFI 146 mappings to be treated as dicts.
Mutations return new mappings that share storage with the underlying mapping.

hashmap-dict

A dictint that allows SRFI 146 hashmaps to be treated as dicts.
Mutations return new mappings that share storage with the underlying mapping.
