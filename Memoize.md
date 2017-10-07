This isn't a proposal, just some ideas and discussion points.

The basic interface to memoizing is a variant of `lambda`, perhaps called `lambda/memo` as in Racket, or perhaps something else.  It returns a procedure that ''memoizes'' the results of calls on it in a ''store'', and then pulls the results out of the store on later calls with the same arguments.  (Obviously you don't want to memoize impure procedures.)  Plausible stores are a-lists, hash tables, mutable or immutable sets, and other data structures.  It's easy to see how `define/memo` would work as a more convenient version of this.

The store can be abstracted into a record with four fields: a store creation procedure, a getter, a setter, and a [[http://srfi.schemers.org/srfi-114/srfi-114.html|SRFI 114]] comparator which defines what counts as "the same arguments" and is passed to the store creation procedure.  We need some standard stores, though unless memoization is tightly coupled to something like hash tables or trees, they won't be very efficient ones.  On the other hand, if it happens that the range of the argument is 0 to a small exact integer, a vector store would be much better than a hash table store.

== Issues ==

 * How do we associate the store object with a particular lambda?  Dynamic parameters seem like the Wrong Thing; the store object should be statically bound to the memoized procedure.  Adding extra arguments to `lambda/memo` beyond the lambda-list and the body doesn't seem like a win either: how do you know if the extra arguments are arguments, or part of the body?.

 * If multiple arguments and/or multiple returns are allowed, how are they packaged up in the store?  Vectors?  Lists?  Something else?

 * Should there be some way to let the function decide which return values to memoize?