# Data formats

Procedures for handling CSV and delimiter-separated values (DSV).
For INI file support, see [IniFilesCowan](IniFilesCowan.md).
For JSON support, see [SRFI 180](http://srfi.schemers.org/srfi-180/srfi-180.html).

Input ports default to `(current-input-port)`, output ports to
`(current-output-port)`.

## Comma-separated values.

The external representation of a CSV (comma-separated values) stream
is defined by [RFC 4180](https://tools.ietf.org/html/rfc4180), with
the following deviations:

  *  CR characters are ignored on input (so LF is a sufficient line break)
     but generated on output.
  
  *  No special treatment is given to the first line.
     It is up to applications to decide if it is a header line or not.
  
  *  No attempt is made to ensure that all records have the same number of fields,
     either on input or on output.
  
  * Processing is done in terms of characters, not bytes.
  
  The internal representation of each record is a list of strings
  that represent the fields of that record.  There is no representation
  of whole files.
  
`(make-csv-generator `[*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generator
which when invoked reads a record from *port* and returns it in the
internal representation.  When *port* returns an end of file object,
the generator closes *port* and returns the object.

`(make-csv-accumulator `[*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
which when invoked with one argument that is the internal representation of a record
writes the corresponding external representation to `port`, returning an unspecified value.
When invoked on an end-of-file object, the generator closes *port* and returns the end of file object.

## Delimiter-separated values

Delimiter-separated values (DSVs) are a variant of comma-separated values,
and the internal representation is the same.
However, the delimiter may be any character, and the end of a record is
always a newline and vice versa.  The following characters are escaped
by a backslash sequence:

  * The delimiter is escaped by preceding it with a backslash
  * A blackslash is escaped as `\\`
  * A newline is escaped as `\n`
  * A tab is escaped as `\t`

All other backslashes are treated literally.

`(make-dsv-generator `*delim* [*port*]`)`

The semantics are the same as the semantics of `make-csv-generator`,
except that *delim* when unescaped delimits fields.
As a special case, if *delim* is a single space,
any consecutive sequence of either spaces or tabs jointly treated as the delimiter.

`(make-dsv-accumulator `*delim* [*port*]`)`

The semantics are the same as the semantics of `make-csv-accumulator`,
except that *delim* is used to delimit fields.

