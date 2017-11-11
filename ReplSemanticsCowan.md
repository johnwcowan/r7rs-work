## REPL semantics

WG1 Scheme implementations SHOULD provide a REPL.

At startup, a REPL MUST import the core module and MAY import an implementation-defined list of other modules.  Implementations SHOULD allow this list to be changed in advance.

A REPL MUST allow other modules to be imported using the same syntax as is used when one module imports another.  Imports may be performed at any time during the life of the REPL.  It is undefined how many times an imported module is instantiated.

Definitions and expressions to be evaluated MAY be intermixed freely.  Identifiers and syntax keywords MAY be redefined, subject to the following rules:

* If the definition of *id1* refers to *id2* other than inside a lambda abstraction, redefining *id2* does not affect the value of that expression.

* If *id* appears in the procedure position of an invocation that is inside a lambda abstraction, applying that abstraction will invoke the procedure bound to the most recent definition of *id*.  (No inlining of procedure calls at the REPL.)

* If an expression uses a syntax keyword, redefinition of that keyword will not affect the value of the expression.  (Syntax keywords are effectively inlined.)

A REPL MUST provide the syntax `(include `*filename*`)`, which causes the declarations and expressions in *filename* to be processed exactly as if they had been input directly to the REPL.

A REPL MAY provide a means of loading modules or include files that have been compiled by a compiler associated with the implementation.
