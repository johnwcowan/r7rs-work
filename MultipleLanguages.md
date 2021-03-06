This is a very preliminary idea for Scheme systems to support multiple languages by translation.
I don't know if it will ever be proposed for R7RS-large.

At the top of a file containing non-Scheme or a different variant of Scheme
there is a line beginning with #!lang
and followed on the same line by the name of a module such as
`(picrin javascript)`, `(chibi lua)`, `(chicken awk)` or `(johnwcowan algol60)`.
This line appears after any shebang line but before any source code.

The Scheme system imports this module, which must export the procedure `transpile`.
This procedure is invoked on three arguments:
an input port, the filename of the source,
and a line number representing the first line of source code,
normally 3 if there is a shebang line and 2 if there is not.

`Transpile` then reads source code from the port in whatever language or languages it understands,
known here as the source language, and returns a list of Scheme definitions and expressions
that represent a translation of the source code into Scheme,
which the Scheme system then processes like any other code.
If the language in question is a Lispy one, `transpile` may be able to use `read`,
but if not it will have to have its own parser.

If an element of the output is an exact integer
(which would not do anything useful as a top-level expression),
it indicates the line of the original source code;
A string indicates a filename from which source is now being read, either the original filename or one
due to an `include` analogue in the source language.

Syntax or static semantic errors in the source code are raised as exceptions
that satisfy the predicate `transpiler-error?`.
Accessors pull out the filename and line number of the error
as well as a message indicating what is wrong.

Implementations of `transpile` should themeselves attempt to be as reusable as possible
by being written in and generating Scheme that conforms to some standard
and makes a minimal use of implementation-specific features.  It is good if the output
of `transpile` allows reasonable access to the procedures of the source language from Scheme code, and
even better if it permits calling Scheme procedures from the source language as if they
were external procedures in the source language.
Of course this interoperability depends on the particular source language.

The only prior art for this is Racket's `#lang` directive, which works similarly
except that the output of the front end is Racket syntax objects
(which carry source file and line information) rather than Scheme source code,
which makes for greater flexibility but also more complexity and less portability.

