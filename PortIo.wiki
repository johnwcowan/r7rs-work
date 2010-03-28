= Port I/O =

A port is a type of abstraction to a data source or sink,
fundamentally sequential in nature though in some cases a
limited amount of random access is allowed.  The data
typically corresponds to files or other devices, such as
network sockets, but may also be entirely virtual in nature.

== Port Types ==

Ports may have a wide range of properties, but are usually
classified along two orthogonal categories, the port
direction and the port type.  The direction corresponds to
whether the port is an input-port (data source) or
output-port (data sink).  In the R5RS these are disjoint,
though some Scheme implementations allow a single port to
act as both input and output.

The type refers to the type of data the port acts on - the
two most common types of ports are binary and textual.  A
binary port is conceptually a stream of octets, and is
necessary for working with any binary data formats,
including database formats and most forms of compression and
encryption.  A textual port is conceptually a stream of
characters, which is often layered on top of a binary port
representing some specific character encoding system (CES),
but may also be represented directly as in a
[[http://srfi.schemers.org/srfi-6/srfi-6.html|string port]].

There can be other types of ports corresponding to streams
of different types of data - the Pascal language provides
record I/O, and Gambit Scheme provides object ports - but
this discussion is only concerned with binary and textual
ports.

The operations performed on binary ports are relatively
simple, dealing with reading individual octets or chunks of
octets, possibly interpreting these as various machine
integers.  In database-like binary formats random-access is
often essential, though for efficiency on most devices its
use should be minimized.  When working with compression
formats it is often necessary to work at the bit-level,
which can be done with higher-level abstractions on top of a
binary port or with a specialized bit port.

Textual ports typically come with a much wider range of
operations.  Input textual ports have libraries for reading
at the level of char, grapheme, token, word, line, sentence
or paragraph (of either natural or programming languages),
and parsing or searching for text in a multitude of formats.
Output textual ports have libraries for writing at the same
levels, and various formatting approaches such as
[[http://srfi.schemers.org/srfi-48/srfi-48.html|templated formatting]],
combinator formatting and logging.

Binary and textual ports may be distinguished both at the
language level by what operations are permitted, and at the
operating system level by how newlines are handled - Windows
converts crlf sequences in ports to a single newline on
input, and vice-versa on output.

Nonetheless one can still provide an interface that allows
mixing binary and textual I/O on the same port, and a
frequent debate is whether to allow this - are binary and
textual ports disjoint?  If they are not disjoint and both
types of operations can be mixed, then it is effectively
impossible to perform efficient buffering (or at least no
one has yet shown an approach to the contrary), which would
make Scheme unsuitable to many types of systems or scripting
applications.  Another problem would be how to handle binary
I/O on a textual port not backed by a binary port (e.g. a
string port).  In lieu of these issues the R6RS made these
two port types disjoint.

Assuming binary and textual ports are disjoint, the next
question that arises is what type are the standard input and
output ports, and can they be changed?  Usually they are
textual, but there is not always a means to change this -
C++ for instance
[[http://www.parashift.com/c++-faq-lite/input-output.html#faq-15.13|has no standard way to make stdin binary.]]

The R6RS provides `current-input-port` etc. as textual
ports, and also `standard-input-port` etc. to create a
"fresh" binary input port connected to standard input.
Presumably these are both separate buffered ports using the
same file descriptor backend, which would result in
inconsistent behavior if both were used.  Thus safe usage
should always look like:

  (close-port (current-input-port))
  (define in (standard-input-port))
  ...

and an API which enforces this might be worth considering.

Other ports as created with `open-input-file` and other
existing R5RS procedures can be assumed to be textual, for
backwards compatibility reasons.  Alternate procedures may
be used for creating binary ports, or the binary distinction
may be included as part of other encoding and meta
information supplied when opening the file (see
`file-options` in the R6RS).

== Port Extensions ==

    * PortEncodings
    * PortRandomAccess
