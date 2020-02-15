## Abstract

The procedures of this SRFI allow
the manipulation of an object that maps keys to values
without the caller needing to know exactly how the object is implemented.
Such an object is called a *dictionary* in this SRFI.

## Rationale

Until recently there was only one fully standard mechanism for managing
key-value pairs: alists.  Most Schemes also support hash tables, but
until R6RS there was no standard interface to them, and many
implementations do not provide that interface.

Now, however, the number of such objects is
growing.  In addition to hash tables, there are
persistent ordered and hashed mappings from SRFI 146
and ordered key-value stores (often on a disk or a remote machine)
from SRFI 167.

It's inconvenient for users if SRFIs or other libraries have to
insist on accepting only a specific type of dictionary.
This SRFI exposes
a number of accessors, mutators, and other procedures that can be
called on any dictionary, provided that its type has been registered
with an implementation of this SRFI.
By using these procedures, a procedure can take a dictionary
as an argument and make use of it without knowing its exact type.

## Specification

In order for the system to know that an object is a dictionary,
a predicate must be defined that recognizes that type of dictionary.
Then the predicate must be registered along with procedures that know
how to manipulate the dictionary.  At least the five basic dictionary procedures
(see below) must be registered, but more may be provided at registration time.

We call a specific key-value pair an *association*.  This is
why an alist, or association list, is called that; it is a list
of associations represented as pairs.

When a key argument is said to be the same as some key of the dictionary,
it means that they are the same in the sense of the dictionary's equality predicate.
It is assumed that no dictionary contains two keys that are the same in this sense.

Dictionaries are said in this SRFI to be *similar* if they are of the same
type and have the same [SRFI 128](http://srfi.schemers.org/srfi-128/srfi-128.html)
comparator.

The basic procedure `dict-search` and the procedures in the mutation section
are linear-update in the sense of SRFI 1.
They return a dictionary which might either be newly allocated
or the same dictionary that was passed to the procedure.
Any previously existing references to the dictionary are no longer valid
and should not be used.


## Dictionary predicate

`(dictionary? `*obj*`)`

Returns `#t` if *obj* answers `#t` to some registered predicate,
and `#f` otherwise.

## Basic dictionary procedures

The procedures of this section provide basic operations on dictionaries.
All other procedures in this SRFI can be implemented on top of them,
so every dictionary type must implement them.

This does not mean that calling one of these procedures always succeeds
independently of the dictionary type.  It is still possible for a dictionary
operation to fail by signaling an error, or for it simply to be an error
to invoke it on that dictionary or dictionary type.

`(dict-size `*dictionary*`)`

Returns an exact integer representing the number of associations in *dictionary*.

`(dict-search `*dictionary key failure success*`)`

This procedure is a workhorse for dictionary lookup, insert, and delete.
The dictionary *dictionary* is searched
for an association whose key is the same as *key*
in the sense of *dictionary*'s comparator.
If one is not found, then the *failure* procedure is tail-called
with two continuation arguments, *insert* and *ignore*,
and is expected to tail-call one of them.

However, if such an association is found,
then the *success* procedure is tail-called
with the matching key of *dictionary*, the associated value,
and two continuation arguments, *update* and *remove*,
and is expected to tail-call one of them.

It is an error if the continuation arguments are invoked other than
in tail position in the *failure* and *success* procedures.
It is also an error if the *failure* and *success* procedures
return to their implicit continuation without invoking
one of their continuation arguments.

The behaviors of the continuations are as follows
(where *obj* is any Scheme object, which in all cases
is returned as a second value):

Invoking `(`*insert value obj*`)` returns a dictionary that
contains all the associations of *dictionary*,
and in addition a new association that maps *key* to *value*.

Invoking `(`*ignore obj*`)` has no effects and returns *dictionary*
unchanged.

Invoking `(`*update new-key new-value obj*`)` returns a dictionary that
contains all the associations of *dictionary*,
except for the association whose key is the same as *key*,
which is replaced by a new association that maps *new-key* to *new-value*.

Invoking `(`*remove obj*`)` returns a dictionary that
contains all the associations of *dictionary*,
except for the association with key key.

`(dict-map `*proc dictionary*`)`

Returns a dictionary similar to *dictionary* that maps each key of *dictionary*
to the value that results
from invoking *proc* on the corresponding key and value of *dictionary*.

`(dict-filter `*pred dictionary*`)`  

Returns a dictionary similar to *dictionary* that contains just the associations of *dictionary*
that satisfy *pred* when it is invoked on the key and value of the association.

`(dict-for-each `*proc dictionary*`)`

Invokes *proc* on each key of *dictionary* and its corresponding value in that order.
This procedure is used for doing operations on the whole dictionary.
Returns an unspecified value.

## Predicates

`(dict-empty? `*dictionary*`)`

Returns `#t` if *dictionary* contains no associations and `#f` if it does contain associations.

`(dict-contains? `*dictionary key*`)`

Returns `#t `if one of the keys of *dictionary* is the same as *key*
and `#f` otherwise.

## Lookup

`(dict-ref 	`*dictionary key* [*failure* [*success*] ]`)`

If *key* is the same as
some key of *dictionary*, then invokes *success* on the corresponding value
and returns its result.
If *key* is not a key of *dictionary*, then invokes the thunk *failure* and
returns its result.

`(dict-ref/default `*dictionary key default`)`

If *key* is the same as some key of *dictionary*, then returns the corresponding value.
If not, then returns *default*.

## Mutation

`(dict-set `*dictionary obj* ...`)`

Returns a dictionary that contains all the associations of *dictionary*
plus those specified by *objs*, which alternate between keys and values.
If a key to be added already exists in *dictionary*, the new value prevails.

`(dict-adjoin `dictionary obj ...`)`

Returns a dictionary that contains all the associations of *dictionary*
plus those spsecified by *objs*, which alternate between keys and values.
If a key to be added already exists in *dictionary*, the old value prevails.

`(dict-delete `*dictionary key* ...`)`

Returns a dictionary that contains all the associations of *dictionary* except those
whose keys are the same as one of the *keys*.

`(dict-delete-all `*dictionary keylist*`)`

Returns a dictionary with all the associations of *dictionary* except those whose
keys are the same as some member of *keylist*.

`(dict-replace `*dictionary key value*`)`

Returns a dictionary that
contains all the associations of *dictionary* except as follows:
If *key* is the same as a key of *dictionary*,
then the association for that key is omitted and replaced by the association
defined by the pair *key* and *value*.
If there is no such key in *dictionary*,
then dictionary is returned unchanged.

`(dict-intern `dictionary key failure`)`

Extracts the value associated with the key in *dictionary* that is the same as *key*,
and returns two values, *dictionary* and the value.
If *key* is not the same as any key in *dictionary*, *failure* is invoked on no arguments.

The procedure then returns two values,
a dictionary that contains all the associations of *dictionary*
and in addition a new association that maps *key* to the result of invoking *failure*, 
and the result of invoking *failure*.

`(dict-update `*dictionary key updater* [*failure* [*success*] ]`)`

Retrieves the value of *key* as if by `dict-ref`,
invokes *updater* on it, and sets the value of *key* to be
the result of calling *updater* as if by `dict-set`,
but may do so more efficiently.  Returns the updated dictionary.

`(dict-update/default `*dictionary key updater default*`)`

Retrieves the value of *key* as if by `dict-ref/default`,
invokes *updater* on it, and sets the value of *key* to be
the result of calling *updater* as if by `dict-set`,
but may do so more efficiently.  Returns the updated dictionary.


`(dict-pop `*dictionary* [*failure*]`)`

Chooses an arbitrary association from *dictionary* and returns three values:
a dictionary that contains all associations of *dictionary* except the chosen one,
and the key and the value of the chosen association. 
If dictionary contains no associations and *failure* is supplied,
then the thunk *failure* is invoked and its values returned.
Otherwise, it is an error.

## The whole dictionary

`(dict-count `*pred dictionary*`)`

Passes each association of dictionary as two arguments to *pred*
and returns an exact integer that counts the number of times
that *pred* returned true.

`(dict-any? `*pred dictionary*`)`

Passes each association of *dictionary* as two arguments to *pred*
and returns true when one of the calls to *pred* returns true.
If all calls return false, `dict-any?` returns false.

`(dict-every? `*pred dictionary*`)`

Passes each association of *dictionary* as two arguments to *pred*
and returns `#f` when any of the calls to *pred* return false.
If all calls return true, `dict-every?` returns true.

`(dict-keys `*dictionary*`)`

Returns a list of the keys of *dictionary* in an arbitrary but consistent order.
The order may change when new elements are added to *dictionary*.

`(dict-values `*dictionary*`)`

Returns a list of the values of *dictionary*.  The results returned
by `dict-keys` and `dict-values` are ordered consistently.

`(dict-entries `*dictionary*`)`

Returns two values, the result of calling `dict-keys` and the
result of calling `dict-values` on *dictionary*.

`(dict-fold `*proc knil dictionary*`)`

Invokes *proc* on each association of *dictionary* with three arguments:
the key of the association, the value of the association,
and an accumulated result of the previous invocation. 
For the first invocation, *knil* is used as the third argument.
Returns the result of the last invocation,
or *knil* if there was no invocation.

`(dict-map->list `*proc dictionary*`)`

Returns a list of values that result from invoking *proc*
on the keys and corresponding values of *dictionary*.

`(dict-remove `*dictionary pred*`)`

Returns a dictionary that contains all the associations of *dictionary*
except those that satisfy *pred* when called on the key and value.

`(dict->alist `*dictionary*`)`

Returns an alist whose keys and values are the keys and values of *dictionary*.

## Registering dictionary types

The following procedure registers new dictionary types.
It is an error to register a dictionary type whose
instances return `#t` to any predicate already registered.

`(register-dictionary! `*pred dictionary*`)`

Registers *pred* and provides procedures that allow
manipulation of dictionaries that satisfy *pred*.
The *dictionary* argument
maps the names of the procedures of this SRFI
to suitable type-specific procedures.  Entries in *dictionary* for
`dict-size`, `dict-search`, `dict-map`, `dict-filter`, and `dict-for-each` are required.
The others are optional, but if provided may be more efficient than the versions provided by
the implementation of this SRFI.

To break the recursion whereby a new type is registered using a
dictionary of an existing type, dictionaries can always be registered
using an alist or property list, as explained in the next section).

## Lists as dictionaries

The exact set of pre-registered dictionaries depends on their
availability in a given implementation.  However, lists are  
supported as dictionaries using the specification in this section.
If two keys are the same (in the sense of the specified equality predicate),
then all but the first are treated as if they did not exist.

If the car of a list is a symbol, then the list is assumed to be a property
list, alternating keys (which must be symbols) with values.
Mutation operations actually mutate the property list whenever possible.
The equality predicate of this kind of dictionary is `eq?`.

If the list is empty, or its car is a pair, then the list is assumed
to be an alist.  New values are added to the beginning of an alist
non-destructively, but it is an error to attempt deletion.
The equality predicate of this type of dictionary is `equal?`.

In all other cases, lists are not treated as dictionaries.
