# Data formats

Procedures for handling CSV, delimiter-separated values (DSV), and INI files.
JSON has been removed, as it is defined elsewhere.

Input ports default to `(current-input-port)`, output ports to
`(current-output-port)`.

## Comma-separated values.

The external representation of a CSV (comma-separated values) stream
is defined by [RFC 4180](https://tools.ietf.org/html/rfc4180), with
the following deviations:

  *  CR characters are ignored on input (so LF is a sufficent line break) but generated on output.
  
  *  No special treatment is given to the first line.  It is up to applications to decide if it is a header line or not.
  
  *  No attempt is made to ensure that all records have the same number of fields, either on input or on output.
  
  * Processing is done in terms of characters, not bytes.
  
  The internal representation of each record is a list of strings
  that represent the fields of that record.  There is no representation
  of whole files.
  
`(make-csv-generator `[*port*]`)`

Returns a [SRFI 121](http://srfi.schemers.org/srfi-121/srfi-121.html) generator
which when invoked reads a record from *port* and returns it in the
internal representation.  When *port* returns an end of file object,
the generator does the same.

`(make-csv-accumulator `[*zero-hack*] [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
which when invoked with one argument that is the internal representation of a record
writes the corresponding external representation to `port`.  When invoked on an
end-of-file object, no action is taken.  In either case, an unspecified value
is returned.

## Delimiter-separated values

Delimiter-separated values (DSVs) are a variant of comma-separated values,
and the internal representation is the same.
However, the delimiter may be any character, and the end of a record is
always a line break and vice versa.  When a backslash
or the delimiter itself is part of a field, it is preceded with a backslash.
A line break that is part of a field is represented using the two-character sequence `"\n"`.
Similarly, a tab that is part of a field is represented using the two-character sequence `"\t"`.
All other backslashes are treated literally.

`(make-dsv-generator `*delim* [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generator
which when invoked reads a record from *port* and returns it in the
internal representation, using *delim* (a character) to delimit fields.
If *delim* is a single space, any number of either spaces or tabs are
jointly treated as the delimiter, so that both spaces and tabs that are
to be interpreted as part of a field need to be escaped.
When *port* returns an end of file object, the generator does the same.

`(make-dsv-accumulator `*delim* [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
which when invoked with one argument that is the internal representation of a record
writes the corresponding external representation to `port`,
using *delim* (a character) to delimit fields.  When invoked on an
end-of-file object, no action is taken.  In either case, an unspecified value
is returned.

