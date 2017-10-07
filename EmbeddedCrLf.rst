I ran a set of existing implementations under Linux
with input redirected to a file
containing:

{{{
(write (string-length "abc
def"))
}}}

where the line break is CR+LF.

Racket, Gauche, Chicken, Bigloo,
Scheme48/scsh, Guile, SISC, Chibi, STklos, !SigScheme, Scheme 9 report 8.

MIT, Kawa, Chez, SCM, Ikarus, Larceny, Ypsilon, Mosh, !IronScheme, Sagittarius report 7.