
# From Jeff Dalton

[Reference](http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1996/msg00162.html)

Perhaps I misunderstood what people meant by "opaque".  I
thought it meant the fields of the record could be accessed
(and modified) only via the proper accessors (and mutators).
With a non-opaque record, OTOH, there would be some other
way to "look inside" (a metaphorical way of speaking that
I thought would fit with the metaphor behind "opaque").
For instance, maybe (record-length rec) tells me the number
of slots, (record-ref rec i) gives me the contents of the
i-th slot, and (record-set! rec i value) puts a new value
in the i-th slot.  (At least that looks like the kind of
abstraction-breaking mechanism someone might object to.)

Now, in most cases no one is going to use record-ref and friends.
It's not like ADT implementations would normally use them all over
the place instead of calling the proper accessors (and mutators).
Nor would someone writing client code be likely to use them.
Neither of these things would happen except in fairly unusual
cases.

### R6RS

In an R6RS context, opaqueness means that you can neither identify the record instance as a record, nor discover its type, nor retrieve its constructors, predicate, accessors, and mutators.  Such opaque records might be used to implement bignums or even conses.

