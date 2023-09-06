## Authors

R. Kent Dybvig (specification author) and John Cowan (shepherd)

## Abstract

Guardians allow programs to protect objects from deallocation by the
garbage collector and to determine where objects would otherwise have
been deallocated.  When the object has associated non-memory resources,
a program can register it with a guardian.  The GC will mark inaccessible
objects but will not collect them; at the program's convenience,
inaccessible objects are removed from the guardian and their non-memory
resources are disposed of.

Guardians allow objects to be saved from deallocation
indefinitely so that they can be reused or so that clean-up or other
actions can be performed using the data stored within the objects.
Guardians avoid the problems associated with classical finalizers
detailed in the Rationale section.

## Issues

None at present.

## Rationale

The difficulties with classical finalizers are:
</p>

 * When run directly by the garbage collector, they must
   be executed within a critical section, because the collector may
   interrupt the rest of the program at arbitrary points.

 * When cyclic and shared structures must be finalized, it may be essential
   to control the order in which finalization is done.

 * They do not always allow the full range of language features
   to be employed.  For example, allocation may not be possible from within
   the garbage collector, as the collector may not be re-entrant.  If an
   error is signaled while the collector is running, it is not always clear
   how to recover.

 * It may or may not be appropriate to examine the object being finalized
   in order to determine what to do with it.  In particular, it may be
   the Right Thing to protect the object from reclamation by placing it in
   a variable or a container, with or without re-registering it for
   finalization.

Guardians solve all of these problems at the expense of manual reclamation.
Objects are preserved by a guardian; periodically, the
guardian mut be invoked to allow them to be reclaimed by the collector
or to preserve them.

## Specification

Guardians are procedures that encapsulate groups of objects registered
for preservation. When a guardian is created, the group of registered
objects is empty. An object is registered with a guardian by passing
the object as an argument to the guardian, as well as a <i>representative
object</i> to be returned when an object is retrieved from the guardian.

The group of registered objects associated with a guardian is
logically subdivided into two disjoint subgroups: a subgroup referred
to as "accessible" objects, and one referred to "inaccessible"
objects. Inaccessible objects are objects that have been proven to be
inaccessible (except through the guardian mechanism itself or through a
weak reference), and accessible objects are objects that have not been
proven so.  Objects may be registered in more than onen guardian, and a guardian
may be registered in another guardian.

The word "proven" is important here: it may be that some
objects in the accessible subgroup are indeed inaccessible but that this
has not yet been proven. This proof may not be made in some cases until
long after the object actually becomes inaccessible, typically when
the collector is run.

Objects registered with a guardian are initially placed in the accessible
group and are moved into the inaccessible group at some point after they
become inaccessible. The representative of an object in the inaccessible
group is retrieved by invoking the guardian without arguments.
If there are no objects in the inaccessible group, the guardian returns `#f`.

Although an object registered without a representative and returned from a guardian
has been proven otherwise inaccessible (except via a weak pointer), 
it has not yet been reclaimed by the storage management system
and will not be reclaimed until after the last non-weak pointer to it
within or outside of the guardian system has been dropped.
In fact, objects that have been retrieved from a guardian have no special status
in this or in any other regard.

This feature circumvents the problems that might otherwise arise with shared or cyclic structure.
A shared or cyclic structure consisting of inaccessible objects is preserved in its entirety,
and each piece registered for preservation with any guardian is placed in the inaccessible set
for that guardian. The programmer then has complete control over the order in which pieces
of the structure are processed.

An object may be registered with a guardian more than once,
in which case it will be retrievable more than once.

### Procedures

`(make-guardian)`

Returns a newly allocated guardian with no registered objects.

`(guardian? `*obj*`)`

Returns `#t` if *obj* is a guardian and `#f` otherwise.

`(`*guardian obj* [ *rep* ]`)`

Registers *obj* with *guardian*, specifying *rep* as the representative object.
If *rep* is omitted, the representative object is *obj*.

`(`*guardian*`)`

Returns the representative of a single object from *guardian*
and removes the object.

`(unregister-guardian `*guardian*`)`

Unregisters all the accessible objects currently registered with the guardian,
with one caveat.  Objects registered by other threads than the current
thread are not necessarily removed from the guardian.  To ensure that all
objects are unregistered in a multithreaded application, a single thread
can be used both to register and unregister objects. Alternatively, an
application can arrange to define a handler that calls
`unregister-guardian` after it calls the collector.

In any case, `unregister-guardian` returns a list containing
the representative objects corresponding to the objects
that it unregisters,
with duplicates as appropriate if the same object is registered more
than once with the guardian. Objects already resurrected but not yet
retrieved from the guardian are not included in the list but remain
retrievable from the guardian.

<h2 id="implementation">Implementation</h2>

A portable implementation of guardians is not possible.  Chez Scheme
provides guardians as explained in this SRFI; Guile provides them as well,
but without `guardian-unregister`.

## Acknowledgements

Thanks to the members of the SRFI discussion group.</p>

## Copyright

This SRFI is © 2023 John Cowan.</p>

This SRFI is a derivative work of Section 13.2 of
<i>Chez Scheme User's Guide</i> version 9, which is licensed under the
[Apache License Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)
, © Cisco Systems, Inc. 2022.
The SRFI as a whole is licensed as follows:

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation files
 (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge,
 publish, distribute, sublicense, and/or sell copies of the Software,
 and to permit persons to whom the Software is furnished to do so,
 subject to the following conditions:

 This section (including the next paragraph) shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
