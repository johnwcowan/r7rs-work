
= From R. Kent Dybvig =
[[http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1996/msg00101.html|Reference]]

One value of opacity is that it simplifies program analysis, both by
the user and by the compiler.  When programming, it's nice to be able
to scan the code where a record type declaration is visible and know
whether a field is used, whether it is assigned, and what type of value
it holds.  If any part of the program to which we pass the object can
obtain a field accessor or mutator, we've lost this ability. 

