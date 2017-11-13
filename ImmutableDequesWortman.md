## Introduction

This SRFI defines immutable deques. A structure is immutable when all its operations leave the structure unchanged. Note that none of the procedures specified here ends with a `!`.

## Rationale

A double-ended queue, or *deque* (pronounced "deck") is a sequential data structure which allows elements to be added or removed from either end efficiently.  It is a generalization of both a queue and a stack, and can be used as either by disregarding the irrelevant procedures.

This SRFI describes immutable deques, or *ideques*.  Immutable structures are sometimes called *persistent* and are closely related to *pure functional* (a.k.a. *pure*) structures. The availability of immutable data structures facilitates writing efficient programs in the pure-functional style.  Unlike the immutable lists of [SRFI 116](http://srfi.schemers.org/srfi-116/srfi-116.html), it is efficient to produce modified versions of a deque; unlike the list queues of [SRFI 117](http://srfi.schemers.org/srfi-117/srfi-117.html), it is possible to efficiently return an updated version of a deque without mutating any earlier versions of it.

## Specification

We specify required time efficiency upper bounds using big-O notation. We note when, in some cases, there is "slack" between the required bound and the theoretically optimal bound for an operation. Implementations may use data structures with amortized time bounds, but should document which bounds hold in only an amortized sense. The use of randomized data structures with expected time bounds is discouraged.

Deques are disjoint from all other Scheme types.

### Constructors

`(ideque `[[|*element* ...]]`)`

Returns a deque containing the *elements*. The leftmost element (if any) will be at the front of the deque and the rightmost element (if any) will be at the back. Takes O(n) time, where *n* is the number of elements.

`(ideque-unfold `*stop? mapper successor seed*`)`

Invokes the predicate *stop?* on *seed*.  If it returns false, generate the next result by applying *mapper* to *seed*, generate the next seed by applying *successor* to *seed*, and repeat this algorithm with the new seed.  If *stop?* returns true, return a deque containing the results in order of accumulation.  Takes O(n) time.

`(ideque-unfold-right `*stop? mapper successor seed*`)`

Invokes the predicate *stop?* on *seed*.  If it returns false, generate the next result by applying *mapper* to *seed*, generate the next seed by applying *successor* to *seed*, and repeat the algorithm with the new seed.  If *stop?* returns true, return a deque containing the results in reverse order of accumulation.  Takes O(n) time.

### Predicates

`(ideque? `*x*`)`

Returns `#t` if *x* is a deque, and `#f` otherwise.  Takes O(1) time.

`(ideque-empty? `*deque*`)`

Returns `#t` if *deque* contains zero elements, and `#f` otherwise.  Takes O(1) time.

### Queue operations

`(ideque-front `*deque*`)`

`(ideque-back `*deque*`)`

Returns the front/back element of *deque*. It is an error for *deque* to be empty. Takes O(1) time.

`(ideque-remove-front `*deque*`)`

`(ideque-remove-back `*deque*`)`

Returns a deque with the front/back element of *deque* removed. It is an error for *deque* to be empty. Takes O(1) time.

`(ideque-add-front `*deque obj*`)`

`(ideque-add-back `*deque obj*`)`

Returns a deque with *obj* pushed to the front/back of *deque*. Takes O(1) time.

### Other accessors

`(ideque-take `*deque n*`)`

`(ideque-take-right `*deque n*`)`

Returns a deque containing the first/last *n* elements of *deque*.  Takes O(n) time.

`(ideque-drop `*deque n*`)`

`(ideque-drop-right `*deque n*`)`

Returns a deque containing all but the first/last *n* elements of *deque*.  Takes O(n) time.

`(ideque-split-at `*deque n*`)`

Returns two values, the results of `(ideque-take `*deque n*`)` and `(ideque-drop `*deque n*`)` respectively, but may be more efficient.  Takes O(n) time.

### The whole deque

`(ideque-length `*deque*`)`

Returns the length of *deque* as an exact integer.  May take O(n) time, though O(1) is optimal.

`(ideque-append `*deque* ...`)`

Returns a deque with the contents of the *deque* followed by the others, or an empty deque if there are none. Takes O(kn) time, where k is the number of deques and n is the number of elements involved, though O(k log n) is possible.

`(ideque-concatenate `*list-of-deques*`)`

Returns a deque with the contents of the first deque in *list-of-deques* followed by the others. This is provided for Schemes in which the number of arguments which can be passed to `apply` is limited.  Takes O(kn) time, where k is the number of deques and n is the number of elements involved, though O(k log n) is possible.

`(ideque-reverse `*deque*`)`

Returns a deque containing the elements of *deque* in reverse order.  Takes O(n) time.

`(ideque-count `*pred deque*`)`

Returns the number of elements of *deque* which satisfy *pred* as an exact integer.  Takes O(n) time.

`(ideque-zip `*deque* ...`)`

Returns a deque of lists (not deques) each of which contains the corresponding elements of the argument deques in the order specified.  Processing stops when all the elements of any deque have been seen. Takes O(kn) time, where *k* is the number of deques and *n* is the number of elements involved.

### Mapping

`(ideque-map `*proc deque* ...`)`

Applies *proc* to the corresponding elements of *deques* and returns a deque containing the results in order.  Terminates when any deque is finished.  Takes O(n) time.

`(ideque-for-each `*proc deque* ...`)`

Applies *proc* to the corresponding elements of *deques* in order and returns an unspecified result.  Terminates when any deque is finished.  Takes O(n) time.

`(ideque-fold `*proc nil deque* ...`)`

`(ideque-fold-right `*proc nil deque* ...`)`

Invokes *proc* on the corresponding elements of *deques* in forward/reverse order, passing the result of the previous invocation as a second argument. For the first invocation, *nil* is used as the second argument. Returns the result of the last invocation, or *nil* if there was no invocation.  Terminates when any deque is finished.  Takes O(n) time.

`(ideque-append-map `*proc deque* ...`)`

Applies *proc* to the corresponding elements of *deques*.  It is an error if the result is not a list.  Returns a deque containing the elements of the lists in order.  Terminates when any deque is finished.  Takes O(n) time.

### Filtering

`(ideque-filter `*pred deque*`)`

`(ideque-remove `*pred deque*`)`

Returns a deque which contains the elements of *deque* that do/do not satisfy *pred*.  Takes O(n) time.

`(ideque-partition `*proc deque*`)`

Returns two values, the results of `(ideque-filter `*pred deque*`)` and `(ideque-remove `*pred deque*`)` respectively, but may be more efficient.  Takes O(n) time.

### Searching

`(ideque-find `*pred deque failure*`)`

`(ideque-find-right `*pred deque failure*`)`

Returns the first/last element of *deque* that satisfies *pred*.  If there is no such element, returns the result of invoking the thunk *failure* .  Takes O(n) time.

`(ideque-take-while `*pred deque*`)`

`(ideque-take-while-right `*pred deque*`)`

Returns a deque containing the longest initial/final prefix of elements in *deque* all of which satisfy *pred*.  Takes O(n) time.

`(ideque-drop-while `*pred deque*`)`

`(ideque-drop-while-right `*pred deque*`)`

Returns a deque which omits the longest initial/final prefix of elements in *deque* all of which satisfy *pred*, but includes all other elements of *deque*.  Takes O(n) time.

`(ideque-span `*pred deque*`)`

`(ideque-break `*pred deque*`)`

Returns two values, the initial prefix of the elements of *deque* which do/do not satisfy *pred*, and the remaining elements.  Takes O(n) time.

`(ideque-any `*pred deque*`)`

`(ideque-every `*pred deque*`)`

Invokes *pred* on the elements of *deque* in order until one of them returns a true/false value, which is then returned.  If there are no such elements, returns `#f`/`#t`.  Takes O(n) time.

### Conversion

`(list->ideque `*list*`)`

`(ideque->list `*deque*`)`

Conversion between deque and list structures. FIFO order is preserved, so the front of a list corresponds to the front of a deque. Each operation takes O(n) time.

`(generator->ideque `*generator*`)

`(ideque->generator `*ideque*`)`

Converts a [SRFI 121](http://srfi.schemers.org/srfi-121/srfi-121.html) generator to and from a deque.
