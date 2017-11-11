This is my counterproposal to [ModuleFactoringCowan](ModuleFactoringCowan.md), which is in turn a counterproposal to [ModuleFactoringShinn](ModuleFactoringShinn.md).  I've copied some text directly with only minor edits.

* Leave `(scheme io)` unchanged.
* It's better to be explicit than to make a combined module here.  For example, if we added a fifth submodule later, would that automatically be included in (scheme io)?  Furthermore, it's better not to move it to the base.  Some embedded Schemes may really not want to include this kind of I/O in some situations, and leaving these procedures in a module makes that easier.
* Leave `interaction-environment` in the `(scheme repl)` module.
* Same as [ModuleFactoringCowan](ModuleFactoringCowan.md).
* Move `case-lambda' to a new `(scheme case-lambda)` module.
* Same as [ModuleFactoringShinn](ModuleFactoringShinn.md).  There's no reason that this has to be provided in the base.
* Move `(scheme multiple-values)` back to the base.
* Same as [ModuleFactoringCowan](ModuleFactoringCowan.md).  It's too late to remove multiple values from the base language.
* Rename `(scheme unicode)` to `(scheme char)`.
* Same as [ModuleFactoringCowan](ModuleFactoringCowan.md).
* Move `char-alphabetic?`, `char-numeric?`, `char-upper-case?`, `char-lower-case?` and `char-whitespace?` to `(scheme char)`.
* Same as [ModuleFactoringCowan](ModuleFactoringCowan.md).
* Move the normalization procedures to `(scheme char normalization)`.
* Same as [ModuleFactoringShinn](ModuleFactoringShinn.md), as opposed to [ModuleFactoringCowan](ModuleFactoringCowan.md)'s `(scheme normalization)`.  "Normalization" is too broad a term to use without qualification, and there's no reason that three symbols is too many.
