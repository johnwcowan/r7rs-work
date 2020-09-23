The following Schemes can produce standalone executables:

Bigloo, Chicken, Loko, Gambit, Owl Lisp, Racket, Rhizome/pi, RScheme, Scheme->C, Stalin

Most of these compile to C, with the following exceptions:

Racket packages up the necessary materials
to make something that can be run from the command line,
though with no speed improvements over Racket's conventional JIT.
The externally maintained program `chez-exe` provides the same
facility for Chez.

Loko compiles directly to x86_64 executables, either hosted under Linux
or stand-alone on bare metal.
