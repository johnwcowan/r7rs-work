This WG2 proposal extends Scheme symbols in a variety of traditional Lisp ways: it provides uninterned symbols and property lists.  It can be implemented portably on top of standard Scheme symbols, provided that the `symbol?`, `symbol=?`, and `symbol->string` procedures are redefined to deal with uninterned symbols as well as standard symbols.  For a high-quality implementation, a record facility is needed for uninterned symbols, and a hash table or similar lookup table to associate standard symbols with their property lists.

## Uninterned symbols

Uninterned symbols are like standard symbols, except that they have two names, a unique name and a human-readable name (which need not be unique).  Implementations need not guarantee that the unique name is truly unique, but should make a best effort to do so, such as by using a large pseudo-random string of characters as the name.

`(symbol? `*obj*`)`

Returns `#t` if *obj* is either a standard symbol or an uninterned symbol, and `#f` otherwise.

`(standard-symbol? `*obj*`)`

Returns `#t` if *obj* is a standard symbol, and `#f` otherwise.

`(uninterned-symbol? `*obj*`)`

Returns `#t` if *obj* is an uninterned symbol, and `#f` otherwise.

`(symbol=? `*symbol1 symbol2* ... `)`

Returns `#t` if all the *symbol*s have the same unique name in the sense of `string=?`, and `#f` otherwise.

`(symbol->string `*symbol*`)`

Returns the name of *symbol* as a string if it is a standard symbol, or its human-readable name if it is an uninterned symbol.  It is an error to mutate this string.

`(string->symbol `*string*`)`

Returns the standard symbol whose name is *string*.  (Same as the standard version of this procedure.)

`(string->uninterned-symbol `*string*`)`

Returns a newly allocated uninterned symbol whose human-readable name is *string*, and whose unique name is chosen by the implementation.

`(symbol-unique-name `*symbol*`)`

Return the name of *symbol* if it is a standard symbol, or its unique name if it is an uninterned symbol.

`(gensym ` [#|*string* ]] `)`

Returns a newly allocated uninterned symbol whose human-readable name is distinct from that of any symbol returned by a previous call to `gensym`.  If *string* is present, it is used as a prefix of the human-readable name.

## Property lists

A property list (plist) is a list that can be bound to a symbol.  The original value of a symbol's plist is implementation-dependent.  The structure of a plist is a sequence of symbols known as *indicators*, some of which may be followed by an arbitrary object known as the indicator's *value*.  Note that plists are a list of objects, unlike alists which are a list of pairs.

`(get-property `*symbol indicator* [#|*obj* ]] `)`

If a symbol that is `symbol=?` to *indicator* is found on the list, the next object on the plist of *symbol* is returned; it is an error if there is no next object.  If there is not, *obj* is returned if it is present, and if absent, `#f` is returned.

`(get-property-list `*symbol indicator*`)`

If a symbol that is `symbol=?` to *indicator* is found on the plist of *symbol*, the rest of the plist is returned.  If there is not, `#f` is returned.

`(set-property! `*symbol indicator obj*`)`

If a symbol that is `symbol=?` to *indicator* is found on the plist of *symbol*, the next object on the plist is destructively replaced with *obj*; it is an error if there is no next object.  If there is not, *indicator* and *obj* are consed on to the beginning of the plist.   An unspecified value is returned.

`(remove-property! `*symbol indicator*`)`

If a symbol that is `symbol=?` to *indicator* is found on the plist of *symbol*, the indicator and the next object on the list are destructively removed from the plist and `#t` is returned; it is an error if there is no next object.  If there is not, nothing is done, and `#f` is returned.

`(symbol-property-list `*symbol*`)`

Returns the plist associated with *symbol* without copying it.  The caller is free to mutate the returned list.

`(symbol-property-list-set! `*symbol list*`)`

The plist associated with *symbol* is replaced with *list*, which is not copied.  An unspecified value is returned.

`(symbol-add-indicators! `*symbol-list indicator*`)`

*Symbol-list* is a list of symbols.  If a symbol that is `symbol=?` to *indicator* is found on the plist of *symbol*, nothing is done.  If not, *indicator* is consed on to the beginning of the plist.  An unspecified value is returned.

`(symbol-remove-indicators! `*symbol-list indicator*`)`

*Symbol-list* is a list of symbols.  If a symbol that is `symbol=?` to *indicator* is found on the plist of *symbol*, the indicator is destructively removed from the plist.  If not, *indicator* is consed on to the beginning of the plist.  An unspecified value is returned.

## Symbol utilities

`(symbol-append ` *symbol* ... `)`

Returns a standard symbol whose name is the concatenation (as if by `string-append`) of the names of the *symbol*s.  This may be used to construct pseudo-structured symbol names; for example, `(symbol-append prefix ":" name)`.
