Things that R7RS-small implementations explicitly *should* do:

* Produce exact results from exact arguments passed to rational procedures
* Produce inexact results if exact results are not possible
* Use at least IEEE precision for transcendental operations
* Cause `(exit #t)` and `(exit #f)` to communicate success and failure to the operating system
* Use compactly represented integers as values of `(current-jiffy)`
* Document the mapping between library names and file names
* Permit redefinition or mutation of imported bindings (in the REPL only)
