
# From R. Kent Dybvig
[Reference](http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1996/msg00101.html)

I object to the procedural interface and prefer a syntactic
interface for two reasons.  First, the procedural interface is verbose,
so that portable code that uses it either will be ugly or will have to
carry with it the (possibly sizable) definition of a syntactic
extension.  Short snippets of expository code involving records either
won't be short or, if they employ a syntactic extension without
defining it, won't be well-specified.

Second, the procedural interface is difficult to compile efficiently
because it is too unstructured and too general.  The result will be
that each implementation that bothers to try to generate good code will
recognize only certain stylized uses (those produced by the local
define-record-type syntactic extension), so that code won't be portable
with any reasonable degree of efficiency.  By standardizing on a
syntactic construct, we stylize all code and introduce restrictions,
e.g., that all accessors and mutators be created together, that will
allow even simple compilers to generate good code.

