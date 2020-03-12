> I am not sure whether "Hash List" is the correct term for what I have in mind, but by "Hash List"
> I mean a hash table whose values are used to link the entries to a Scheme-style list.

Python calls this an OrderedDict, and I think "sorted dictionary" is a reasonably Schemey name,
though it should probably be abbreviated for use as a prefix to procedure names.
"sd" may be too short, but I'll use it here.

In order to efficiently implement `push!` as you have described it,
it would be necessary to keep a doubly linked list of the keys
so that when an existing key is pushed it can be spliced out of its current location.
Given that, it makes sense to be able to push and pop at either end,
and in fact Python allows this, making this data structure also a random access deque.

The usual approach to doubly linked lists is to use triples with car, cdr, and cbr elements
("b" stands for "backwards"), but fully integrating such lists into Scheme
would require a SRFI almost twice the size of SRFI 1.
Therefore, I suggest a different approach:

A sorted hash table contains three elements: an ordinary hash table, an alist of key-value pairs
in forward order, and the same alist in backward order,
with corresponding accessors `sd-dict`, `sd-forward`, `sd-backward`.
It is an error to mutate any of the results of these procedures;
however, all other SRFI 1 or SRFI 125 procedures just work,
which hugely reduces the API surface.
Only the various mutators have to be reimplemented.

The hash table, rather than mapping keys to user-specified values, maps keys
to pairs: the car and cdr are the
corresponding pairs in the forward and backward alists respectively.
To get the user-level value of a key, look at the caar of either of the corresponding pairs.

The data structure invariant is that for every key of a sorted dictionary *sd*,
the key,
`(caar (hash-table-ref/default (sd-dict sd) key))`,
and `(cadr (hash-table-ref/default (sd-dict sd) key))`,
are all equal in the sense of the comparator of `(sd-dict sd)`.

By making all these accessors user-visible,
users can navigate around the data structure with ordinary Scheme operations:
given a key, you can find the next key in an sd with
`(caadr (hash-table-ref/default (sd-table sd) #f)))`.
This needs bulletproofing for the cases of a nonexistent key or no next key,
so `sd-ref`, `sd-ref/default`, `sd-next-key`, and `sd-previous-key` are worth
adding as convenience procedures.

> The interface would contain procedures like:
> 
> `(make-hash-list)`
  
Per my design above, this would take an existing empty dictionary and wrap an sd around it.
That way you can get the advantage of the arguments to `make-hash-table` without duplicating them.

> `(hash-list-contains? hl key)`

Use `(hash-table-contains? (sd-table sd)) (hash-list-push! hl key)`.

> `(hash-list-push! hl key)`
> 
> `(hash-list-pop! hl key)`

These are the main new mutators needed.
They need an indication of which end to push/pop, or else four procedures rather than two
(I favor the latter approach).

Other mutators needed would be `sd-delete!`, `sd-delete-all!`, `sd-intern!`,
`sd-replace!`, `sd-update!`, `sd-update/default!`, and `sd-clear!`

> `(hash-list-fold proc seed hl)`
> 
> `(hash-list-fold-right proc seed hl)`

Use SRFI 1 fold over the forward or backward list, calling `sd-triple-value` as needed.

> `(hash-list-reverse! hl)`

This procedure would swap the forward and backward lists in the sd.

Such a data type could be easily implemented on top of SRFI 125.
It could also be implemented over an abstract dictionary type:
see [Dictionaries](https://bitbucket.org/cowan/r7rs-wg1-infra/src/default/Dictionaries.md)
for a pre-SRFI.  Note that sd's are mutable, even if the underlying dictionary is
linear-update.