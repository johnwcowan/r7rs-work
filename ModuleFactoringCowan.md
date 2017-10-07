Here's my counterproposal to ModuleFactoringShinn, with rationales:

 * Move `(scheme io)` back to the base.
   * It provides string-ports and the shared function of all ports, and there is no reason why all Schemes shouldn't provide these.  Note that `(scheme io)` was never actually proposed or voted on, and its presence in the first draft is an error by the editors.
   * Failing this, I prefer the name `(scheme ports)` from ModuleFactoringMedernach to `(scheme io)`, since by themselves these routines don't actually provide input to or output from the program.
 * Don't provide an umbrella module for I/O.
   * Files, reading Scheme objects, and writing Scheme objects are orthogonal.  Demanding that your implementation provide all of them as a matter of course, across the whole range of Schemes, doesn't make much sense.  We already have a ticket allowing Schemes to pre-import whichever modules they want in the REPL.
 * Leave `interaction-environment` in the `(scheme repl)` module.
   * Alex wants to move this to `(scheme eval)`, but the essence of `(scheme repl)` is that it provides not only evaluation of expressions, but a mutable global environment.  There is no reason why a Scheme can't provide the first without the second.
 * Leave `case-lambda` in the base module.
   * It captures a useful pattern and can be optimized by a slick compiler or by hand.
 * Move `(scheme multiple-values)` back to the base.
   * Multiple values are easy to hack an implementation for (lists, or a list wrapped in a record if you insist), and at least `exact-integer-sqrt` depends on them.  Alex wants to move this procedure to a separate module so it can depend on `(scheme multiple-values)`, but I think that's [[http://dictionary.reference.com/browse/pelion|piling Pelion on Ossa]].
 * Rename `(scheme unicode)` to `(scheme char)`.
   * Alex and I agree on this one.  The essence of this module is that it does things which, on a full-Unicode system, require a big table.  Making it a module means that you can have full Unicode without the big table unless specific code requires it.
 * Move `char-alphabetic?`, `char-numeric?`, `char-upper-case?`, `char-lower-case?` and `char-whitespace?` to `(scheme char)`.
   * Alex and I agree on this one; not doing it in the first place was an oversight on my part.
 * Move the normalization procedures to `(scheme char normalization)`.
   * I still prefer to keep the names of WG1 modules simple, but okay, fine.

== Not part of the proposal ==

Alex says that depending on discussion, he would consider moving
the following out of the core:

 * `syntax-rules`
 * `define-record-type`
 * blob procedures
 * `string-ref` and `string-set!` (but possibly he means `string-set!` and `string-fill`?)

The first three are IMO important parts of Scheme and cheap to provide directly on all reasonable systems.  There is no reason why all Schemes can't support them, which I consider the most important factor in moving things to modules.  It doesn't make sense for all Schemes to provide inexact arithmetic (which is mostly about having floating-point instructions available) or files, so they belong in modules.

I am no fan of mutable strings, but reluctantly I conclude that programs depend on them and we can't get rid of them, so all Schemes might as well provide them.  Hence no module.
