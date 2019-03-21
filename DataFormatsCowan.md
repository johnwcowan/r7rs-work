# Data formats

Procedures for handling JSON, CSV, and delimiter-separated values (DSV).

Input ports default to `(current-input-port)`, output ports to
`(current-output-port)`.

## JSON

 The external representation of JSON values is defined by
 [RFC 7159](https://tools.ietf.org/html/rfc7159).
 The internal representation of JSON values in Scheme is as follows:

  *  JSON numbers are represented as Scheme numbers.  Integers may be represented as exact integers; all other numbers are represented as inexact real numbers.
  
  *  JSON strings are represented as Scheme strings.
  
  *  The JSON values `true`, `false`, and `null` are represented as `#t`, `#f`, and the symbol `null` respectively.
  
  *  JSON arrays are represented as Scheme vectors.
  
  *  JSON objects are represented as Scheme alists that map keys (strings) to values (any value).
  
`(read-json `[*port*]`)`

Reads the external representation of a JSON value from *port* and returns the
internal representation.

`(write-json `*obj* [*port*]`)`

Writes *obj* to *port* as an external representation,
returning an unspecified value.
If *obj* is not an internal representation, an error
satisfying `data-format-error?` is signaled.

`(check-json `*obj*`)`

Returns `#t` if *obj* is an internal representation of a JSON value, and
`#f` otherwise.

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

If the *zero-hack* value is present, it is an error if it is not a boolean.  If
it is present and `#t`, then any field consisting solely of digits and whose
first character is `0` is written with `="` before it and `"` after it.  This
will cause the leading zeros to be preserved by spreadsheet programs, which
attempt to identify the type of a value based on the presence or absence of
quotation marks surrounding it.

## Delimiter-separated values

Delimiter-separated values (DSVs) are a variant of comma-separated values.
However, the delimiter may be any character, and the end of a record is
always a line breakand vice versa.  To include a double quote, a backslash,
or the delimiter itself as part of a field, precede it with a backslash.
To include a line break in a field, use a backslash followed by `n`.

`(make-dsv-generator `*delim* [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generator
which when invoked reads a record from *port* and returns it in the
internal representation, using *delim* (a character) to delimit fields.
If *delim* is a single space, any number of either spaces or tabs are
jointly treated as the delimiter, so that both spaces and tabs that are
part of a field need to be escaped with a backslash.
When *port* returns an end of file object, the generator does the same.

`(make-csv-accumulator `[*zero-hack*] [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
which when invoked with one argument that is the internal representation of a record
writes the corresponding external representation to `port`,
using *delim* (a character) to delimit fields.  When invoked on an
end-of-file object, no action is taken.  In either case, an unspecified value
is returned.

## Auxiliary routines

The following routines are provided to ease the interoperation
of generators with ports.

`(reader->generator `*proc arg* ...`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generator
which when invoked calls *proc* on the *args*, and returns whatever *proc*
returns.  For example, invoking the generator  returned by
`(reader->generator read-string 10)` will read a string of 10 characters
from the current input port and return it.  If 10 characters are not
available, a string containing as many as are available is returned;
if none are available, an end of file object is returned.

`(writer->accumulator `*proc arg* ...`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
which when invoked with one argument calls *proc* on the that argument followed by
the *args*, and returns whatever *proc* returns.  For example, invoking the
accumulator returned by
`(writer->accumulator write-string port 0 10)` with a string argument
will cause the first 10 characters of the string to be written to the port.
When such an accumulator is invoked on an end-of-file object, no action is taken.

`(accumulate-generated-values `*accumulator generator*`)`

Invokes *generator* repeatedly and passes the value as an argument
to *accumulator*.  If *generator* returns an end of file object,
it is also passed to *accumulator*, and `accumulate-generated-values`
returns whatever *accumulator* returned.