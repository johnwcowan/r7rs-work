## Abstract

The procedures of this SRFI allow
the manipulation of an object that maps keys to values
without having to know exactly how the object is implemented.
Such an object is called a dictionary in this SRFI.

## Rationale

Until recently there were only two standard mechanisms for managing
key-value pairs: alists and hash tables, and hash tables do not have
a standard interface.  Now, however, the number of such objects is
growing.  We have SRFI 125 and SRFI 126 as well as R6RS hash tables,
persistent ordered and hashed mappings from SRFI 146,
and ordered key-value stores (often on a disk or a remote machine)
from SRFI 167.

It's inconvenient for users if other SRFIs or libraries have to
insist on accepting only one kind of dictionary.
By using the procedures of this SRFI, a procedure can take a dictionary
as an argument and manipulate it without knowing its type.

## Specification

In order for the system to know that an object is a dictionary,
a predicate must be defined that recognizes that type of dictionary.
Then the predicate must be registered along with procedures that know
how to manipulate the dictionary.
This SRFI exposes
a number of accessors, mutators, and other procedures for dictionaries
that can be called on any dictionary with a registered predicate,
and directly or indirectly invokes the procedures associated with that
predicate.

We call a specific key-value pair an *association*.  This is
why an alist, or association list, is called that; it is a list
of associations represented as pairs.

## Basic dictionary procedures

The following procedures provide basic operations on dictionaries.
All other dictionary operations can be implemented on top of them.

(dictionary? obj)

Answers #t if obj is a dictionary
that answers #t to some registered predicate.

`(make-similar-dict `*dict*`)`

There is not and cannot be a single constructor for all dictionaries,
since the necessary setup is different for each type of constructor.
Therefore, a dictionary must be created using a type-specific constructor.

However, `make-similar-dict` is a constructor that given a dictionary returns a
*similar* dictionary with no associations.
Dictionaries are similar if they satisfy the same predicate and
have the same equality predicate as well as the same ordering predicate,
hash function, or both.

(dict-size dictionary)

Returns an exact integer representing the number of associations in dictionary.

(dict-search dictionary key failure success)

This procedure is a workhorse for dictionary lookup, insert, and delete.
The dictionary dictionary is searched for an association with key key.
If it is not found, then the failure procedure is tail-called
with two continuation arguments, insert and ignore,
and is expected to tail-call one of them.

If an association with key key is found, then the success procedure is tail-called
with the matching key of dictionary, the associated value,
and two continuations, update and remove, and is expected to tail-call one of them.

It is an error if the continuation arguments are invoked,
but not in tail position in the failure and success procedures.
It is also an error if the failure and success procedures
return to their implicit continuation without invoking
one of their continuation arguments.

The effects of the continuations are as follows
(where obj is any Scheme object):

Invoking (insert value obj) returns a dictionary that
contains all the associations of dictionary,
and in addition a new association that maps key to value.

Invoking (ignore obj) has no effects.

Invoking (update new-key new-value obj) returns a dictionary that
contains all the associations of dictionary,
except for the association with key key,
which is replaced by a new association that maps new-key to new-value.

Invoking (remove obj) returns a dictionary that
contains all the associations of dictionary,
except for the association with key key.

In all cases, two values are returned:
the dictionary and obj.

(dict-for-each proc dictionary)

Invokes proc on each key of the dictionary and its corresponding value in that order.
This procedure is used for doing operations on the whole dictionary.
Returns an unspecified value.

## Convenience dictionary procedures

These procedures can be built on top of the basic procedures
and provide many useful further abilities.  

## Predicates

(dict-empty? dictionary)

Returns #t if dictionary contains no associations and #f if it does contain associations.

(dict-contains? dictionary key)

Returns #t if one of the keys of dictionary is key, and #f otherwise.

## Lookup

(dict-ref dictionary key [failure [success] ])

If key is a key of dictionary, then invokes success on the corresponding value
and returns the result.
If key is not a key of dictionary, then invokes the thunk failure and
returns the result.

(dict-ref/default dictionary key default)

If key is a key of dictionary, then returns the corresponding value.
If key is not a key of dictionary, then returns default.

## Mutation

All these procedures as well as `dict-search` are in effect linear-update:
they return a dictionary which might either be newly allocated
or the same dictionary that was passed to the procedure.
Any previously existing references to the dictionary are not valid
and should not be used.

(dict-set dictionary obj ...)

Returns a dictionary that contains all the associations of dictionary
plus those specified by objs, which alternate between keys and values.
If a key to be added already exists in dictionary, the new value prevails.

(dict-adjoin dictionary obj ...)

Returns a dictionary that contains all the associations of dictionary
plus those spsecified by objs, which alternate between keys and values.
If a key to be added already exists in dictionary, the old value prevails.

(dict-delete dictionary key ...)

Returns a dictionary that contains all the associations of dictionary except those
whose keys are keys.

(dict-delete-all dictionary keylist)

Returns a dictionary with all the associations of dictionary except those whose
keys are listed in keylist.

(dict-replace dictionary key value)

Returns a dictionary that uses the same dictint and other properties of dictionary
and contains all the associations of dictionary except as follows:
If key is equal (in the sense of the comparator of dictionary) to an existing key of dictionary,
then the association for that key is omitted and replaced by the association
defined by the pair key and value. If there is no such key in dictionary,
then dictionary is returned unchanged.

(dict-intern dictinct dictionary key failure)

Extracts the value associated to key in dictionary, and returns two values, dictionary and the value.
If key is not contained in dictionary, failure is invoked on no arguments.
The procedure then returns two values, a dictionary that contains all the associations of dictionary
and in addition a new association that maps key to the result of invoking failure, 
and the result of invoking failure.

(dict-update dictionary key updater [failure [success] ])

Semantically equivalent to, but may be more efficient than, the following code:

&nbsp;&nbsp;&nbsp;&nbsp;(dict-set dictionary key (updater (dictionary-ref dictint dictionary key failure success)))

The obvious semantics hold when success (and failure) are omitted (see dictionary-ref).

(dict-update/default dictionary key updater default)

Semantically equivalent to, but may be more efficient than, the following code:

&nbsp;&nbsp;&nbsp;&nbsp;(dict-set dictionary key (updater (dictionary-ref/default dictint dictionary key default)))

(dict-pop dictionary [failure])

Chooses an arbitrary association from dictionary and returns three values:
a dictionary that contains all associations of dictionary except the chosen one,
and the key and the value of the chosen association. 
If dictionary contains no associations and failure is supplied,
then the thunk failure is invoked and its values returned.
Otherwise, it is an error.

## The whole dictionary

(dict-count pred dictionary)

Passes each association of dictionary as two arguments to pred
and returns an exact integer that counts the number of times
that pred returned true.

(dict-any? pred dictionary)

Passes each association of dictionary as two arguments to pred
and returns true when one of the calls to pred returns true.
If all calls return false, dictionary-any? returns false.

(dict-every? pred dictionary)

Passes each association of dictionary as two arguments to pred
and returns #f when any of the calls to pred return false.
If all calls return true, dictionary-every? returns true.

(dict-keys dictionary)

Returns a list of the keys of dictionary.

(dict-values dictionary)

Returns a list of the values of dictionary.  The results returned
by dictionary-keys and dictionary-values are ordered consistently.

(dict-entries dictionary)

Returns two values, the result of calling dictionary-keys and the
result of calling dictionary-values.

(dict-map proc dictionary)

Returns a dictionary containing the keys of dictionary and the values that result
from invoking proc on the keys and corresponding values of dictionary.

(dict-fold proc nil dictionary)

Invokes proc on each association of dictionary with three arguments:
the key of the association, the value of the association,
and an accumulated result of the previous invocation. 
For the first invocation, nil is used as the third argument.
Returns the result of the last invocation,
or nil if there was no invocation.

(dict-map->list proc dictionary)

Returns a list of values that result from invoking proc
on the keys and corresponding values of dictionary.

(dict-filter pred dictionary)  
(dict-remove pred dictionary)

Returns a similar dictionary that contains just the associations of dictionary
that do /do not satisfy pred.

(dict->alist dictionary)

Returns an alist whose keys and values are the keys and values of dictionary.

## Registering dictionary types

The following procedures register new dictionary types.
It is an error to register a dictionary type whose
instances return `#t` to any predicate already registered.

(simple-register-dictionary! pred make-similar-dict dict-size dict-search dict-for-each)

Registers *pred* as a predicate and associates the four basic procedures
shown above and through them to the other procedures of this SRFI.
However, the native procedures of the type of dictionary which
satisfy pred may be more efficient than the synthesized procedures
that using this registration procedure will provide.

(register-dictionary! pred dictionary)

Registers pred and provides procedures that allow more efficient
manipulation of dictionaries that satisfy pred.
The dictionary argument
maps the names of the procedures of this SRFI
to suitable type-specific procedures.  The four procedures listed
in the description of simple-register-dictionary! are required:
the others are optional.

## Lists as dictionaries

The exact list of pre-registered dictionaries depends on their
availability in a given implementation.  However, lists are
supported as dictionaries using the following specification:

If the car of a list is a symbol, it is assumed to be a property
list, alternating keys (which must be symbols) with values.
Searching is done using the `eq?` predicate.
Mutation operations actually mutate the property list whenever possible.

If the list is empty, or its car is a pair, ten the list is assumed
to be an alist.  Searching is done using the `equal?` predicate.
New values are added to the beginning of an alist
non-destructively, but deletions are destructive.

In all other cases, lists are not treated as dictionaries.
