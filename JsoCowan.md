This library allows the creation and use of *JavaScript-style objects
(JSOs)*, which are chained mappings that map keys (which are symbols)
to arbitrary values.  JSOs are represented using a variant of alists,
but they are not compatible with alist procedures.

When you create a JSO, you can specify a *prototype JSO*. The recursive
sequence of a JSO's prototype, the prototype of its prototype, and so on,
is called the *prototype chain*. If a JSO doesn't contain a particular
key, the prototype chain will be searched for it. Modifications,
however, are only applied to the JSO proper, never to anything along
the prototype chain. A JSO without a prototype chain is called a *free JSO*.

An attempt to look up an unknown key returns (by default) a unique
value known as *the undefined value*, which can be tested with
`jso-undef?`.  Note that this value is not itself a JSO.
This value is not to be confused with the general Scheme concept
of returning an undefined value from a procedure, which may be any value
and need not be consistent between two invocations of the same procedure.

A JSO is represented as a Scheme pair whose car is the symbol `@` and
whose cdr is the concatenation of zero or more two-element lists with
another JSO or the empty list.  The first element of each two-element list
is a symbol known as the *key* and the second element is an arbitrary
Scheme object known as the *value*.  Keys are tested for equality with
`eqv?`.  It is no accident that the attribute list of an SXML element
has the same representation as a JSO.

## Constructors

`(make-jso` *prototype obj* ...`)`

Returns a newly allocated JSO with the specified *prototype*.  If 
*prototype* is the empty list, the JSO is free.  Additional 
arguments are alternating keys and values of the JSO.

`(alist->jso` *alist prototype*`)`

Returns a newly allocated JSO with the specified *prototype* (or a free 
JSO if *prototype* is the undefined value).  The key-value pairs of 
*alist* become the corresponding keys and values of the JSO.  It is an error 
if the alist has duplicate keys.

`(jso-unfold` *stop? mapper successor seed*`)`

Create a new JSO.  If the result of applying the predicate *stop?* to
*seed* is true, return the JSO.  Otherwise, apply the procedure *mapper*
to the current seed, which is initialized to *seed*.  *Mapper* returns
two values, which are inserted into the JSO as the key and the value
respectively.  Then get a new seed by applying the procedure *successor*
to the current seed and repeat this algorithm.

## Predicates

`(jso-undef?` *obj*`)`

Returns `#t` if *obj* is the undefined value, and `#f` otherwise.  The 
undefined value is distinct in the sense of `eqv?` from every other 
Scheme object.

`(jso?` *obj*`)`

Returns `#t` if the car of *jso* is the symbol `@`, and `#f` otherwise. 
The rest of the JSO is left unexamined.

`(jso-empty?` *jso*`)`

Returns `#t` if the JSO has no keys, and `#f` otherwise.  The prototype 
chain is left unexamined.

`(jso-contains?` *jso key*`)`

Returns `#t` if *jso* contains *key*, and `#f` otherwise.  The 
prototype chain is left unexamined.

## Accessors

`(jso-ref` *jso key* [ *failure* [ *success* ] ]`)`

`(jso-ref/default` *jso key default*`)`

Searches *jso*, including the prototype chain, for the key *key*, 
invokes the procedure *success* on it, and returns the result.  If 
*key* is not found, invokes the thunk *failure* and returns the result. 
If *success* is not specified, it is the identity function; if *failure* 
is not specified, it is a procedure that returns the undefined value.

The `jso-ref/default` procedure is the same, but returns *default*
if the *key* is not found.

`(jso-local-ref` *jso key* [ *failure* [ *success* ] ]`)`

`(jso-local-ref/default` *jso key* default*`)`

The same as `jso-ref` and `jso-ref/default`, except that the prototype
chain is not searched.

`(jso-size` *jso*`)`

Returns the number of keys in *jso* as an exact integer.  The prototype 
chain is left unexamined.

`(jso-prototype` *jso*`)`

Returns the prototype of *jso*, or the undefined value if *jso* is free.

## Conversions

`(jso-keys` *jso*`)`

Returns a list of the keys of *jso*.  The prototype chain is left 
unexamined.

`(jso-values` *jso*`)`

Returns a list of the values of *jso* in the same order as `jso-keys` 
returns the keys.  The prototype chain is left unexamined.

`(jso->alist` *jso*)

Creates a newly allocated alist whose key-value pairs are the keys and 
values of the JSO. The prototype chain is left unexamined.

`(full-jso->alist` *jso*)

Creates a newly allocated alist whose key-value pairs are the keys and 
values of the JSO, including its prototype chain.

`(alist->jso` *alist*`)`

Creates a free JSO whose keys and values are the key-value pairs of the 
alist.  If any key is not a symbol, an error is signaled.

## Copying, mapping, and folding

`(jso-copy` *jso*`)`

Returns a copy of *jso*, sharing the same prototype chain.  The keys 
and values are shared, but not the structural pairs.

`(jso-free-copy` *jso*`)`

Returns a copy of *jso* that is free.  The keys and values are shared, 
but not the structural pairs.

`(jso-full-copy` *jso*`)`

Returns a copy of *jso*, including the prototype chain.  The keys and 
values are shared, but not the structural pairs.

`(jso-map` *proc jso*`)`

Returns a newly allocated JSO after applying *proc* to each key and
value in *jso*; *proc* returns the new key and value.  The prototype
chains are shared.

`(jso-for-each` *proc jso*`)`

Applies *proc* to each key and value.  The prototype chain is left 
unexamined.  Returns an undefined value (not necessarily *the* 
undefined value).

`(jso-fold` *proc seed jso*`)`

Calls *proc* for every association in *jso* with three arguments: the
key of the association, the value of the association, and an accumulated
value *val*. *Val* is *seed* for the first invocation of *proc*, and for
subsequent invocations of *proc*, the returned value of the previous
invocation. The value returned by `jso-fold` is the return value
of the last invocation of *proc*.  The prototype chain is not examined.

`(jso-filter` *pred jso*`)`

Returns a JSO containing the associations of *jso* whose
key and value satisfy *pred*.  The prototype chain is unexamined
and is shared with the result.

`(jso-remove` *pred jso*`)`

Returns a JSO containing the associations of *jso*, except those whose
key and value do not satisfy *pred*.  The prototype chain is unexamined
and is shared with the result.

`(jso-partition` *pred jso*`)`

Returns two values, both JSOs.  The first JSO contains the
associations of *jso* whose key and value satisfy *pred*, and
the second JSO contains those that do not.  The prototype chain is unexamined
and is shared with the results.

## Mutators

These procedures return an undefined value (not necessarily *the*
undefined value).

`(jso-set!` *jso key value*`)`

Searches *jso* for *key*, and if found changes its value to *value*.  
If *key* is not found, *key* and *value* are added as the last key 
before the prototype chain. The prototype chain is left unexamined.

`(jso-delete!` *jso key* ...`)`

Searches *jso* and removes all keys (and their values) that are equal 
to any of the *keys*. The prototype chain is left unexamined. Unknown 
keys are ignored.  

`(jso-delete-all!` *jso key-list*`)`

Does the same as` jso-delete!`, except that the keys to be deleted are 
specified as a list.

`(jso-set-prototype!` *jso prototype*`)`

Replaces the prototype of *jso* with *prototype*.  If *prototype* is 
the undefined value, *jso* becomes free.  

`(jso-map!` *proc jso*`)`

Modifies *jso* by applying *proc* to each key and value in *jso*;
*proc* returns the new value.  The prototype chain is not examined.

## Method calls

`(jso-apply` *jso key argument* ...`)`

Applies the result of `(jso-ref ` *jso key*`)` to *jso* and the 
*arguments*, returning the result(s).  If the result does not exist or 
is not a procedure, it is an error.

`(jso-apply/fallback` *jso key fallback argument* ...`)`

Obtains the result of `(jso-ref `*jso key*`)`.  If the result is not a procedure, it 
is an error.  If the result is a procedure, it is applied to
to *jso* and the *arguments*, returning the result(s).
If the result does not exist, the procedure *fallback* is applied instead.

## JSON

`(json-value?` *obj*`)`

Returns `#t` if *obj* is a JSON value: that is, a finite real number, a 
string, a boolean, the Scheme symbol `null`,
a proper list whose elements (if any) are JSON 
values, or a JSO whose values are JSON values (the prototype chains are 
left unexamined).  The procedure verifies that any JSO's keys are 
symbols.

Note that when discriminating between JSON values represented by lists,
they should be tested in the following order:  the empty list representing
an empty JSON array, any JSO
representing a JSON object, and any other list representing a non-empty
JSON array.

`(json-escape-string` *string ascii?*`)`

Inserts JSON escapes into a plain Scheme string to make it a valid JSON 
string.  In particular, all characters that cannot appear directly in a 
JSON string, or that have single-character escape sequences, are 
escaped.  As JSON requires, characters larger than `#\xFFFF` are 
encoded as two consecutive `\u` sequences.

If the *ascii?* argument is true, all non-ASCII characters are also 
escaped.

`(json-unescape-string` *string*`)`

Interprets all escape sequences in a valid JSON string and returns the
result.  An error satisfying `json-error?` is signaled if a malformed
escape sequence is found.

`(json-write` *obj [ *options* [ *port* ] ]`)`

Output *obj* to *port* (which defaults to the value of 
`(current-output-port)`) in [JSON 
format](https://tools.ietf.org/html/rfc7159).  Exact rationals other 
than integers are converted to inexact numbers before being output.  
The symbol `null` is output as the keyword `null`.  If *obj* is not a JSON 
value, an error satisfying `json-error?` is signaled before any output is done.

The *options* argument is an optional list of symbols.  The symbol `ascii` causes 
all non-ASCII characters in strings to be escaped.  The symbol `pretty` 
may cause the JSON to be pretty-printed.  The meanings of all other symbols are 
implementation-dependent.

`(json-read` [ *prototype* [ *port* ] ]`)`

Reads the [JSON representation](https://tools.ietf.org/html/rfc7159) of 
a JSON value from *port* (which defaults to the value of 
`(current-input-port)`) and returns the appropriate value.  Any leading 
whitespace is skipped.  The keyword `null` is read as the symbol `null`.
Integers are returned as exact numbers in accordance with Scheme rules; 
all other JSON numbers are returned as inexact numbers.
Any JSOs returned are created with *prototype*, or as free JSOs if 
*prototype* is the empty list or omitted.

If the representation is not 
syntactically correct JSON, an error is signaled and *port* is left in
an undefined state.  If an end of file is 
read from *port*, then if nothing has been read yet an end-of-file 
object is returned; otherwise, an error is signaled.



`(json-error?` *obj*`)`

Returns true if *obj* is an error specified by any of the errors of
the above procedures.
