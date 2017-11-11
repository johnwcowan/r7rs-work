It's important to be able to build development tools that operate on
all types of records, not just one in particular.  For example, if I
want to build a debugger that is able to print out the contents of
record structures in a uniform way, I need a standard way to ask any
record for its contents, for its RTD, etc.  The debugger should not
have to be extended every time a new record type is defined.  While
most programs may not make use of this facility, its controlled use is
powerful and useful.
