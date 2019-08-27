# Data formats

Procedures for handling CSV, delimiter-separated values (DSV), and maybe INI files.
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

If the *zero-hack* value is present, it is an error if it is not a boolean.  If
it is present and `#t`, then any field with the syntax of a number whose
first character is `0` is written with `="` before it and `"` after it.  This
will cause the leading zeros to be preserved by spreadsheet programs, which
attempt to identify the type of a value based on the presence or absence of
quotation marks surrounding it.

## Delimiter-separated values

Delimiter-separated values (DSVs) are a variant of comma-separated values,
and the internal representation is the same.
However, the delimiter may be any character, and the end of a record is
always a line break and vice versa.  When a backslash
or the delimiter itself is part of a field, it is preceded with a backslash.
A line break that is part of a field is represented using the two-character sequence `"\n"`.
Similarly, a tab that is part of a field is represented using the two-character sequence `"\t"`.

`(make-dsv-generator `*delim* [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) generator
which when invoked reads a record from *port* and returns it in the
internal representation, using *delim* (a character) to delimit fields.
If *delim* is a single space, any number of either spaces or tabs are
jointly treated as the delimiter, so that both spaces and tabs that are
part of a field need to be escaped with a backslash.
When *port* returns an end of file object, the generator does the same.

`(make-dsv-accumulator `*delim* [*port*]`)`

Returns a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
which when invoked with one argument that is the internal representation of a record
writes the corresponding external representation to `port`,
using *delim* (a character) to delimit fields.  When invoked on an
end-of-file object, no action is taken.  In either case, an unspecified value
is returned.

## INI files (maybe)

An INI file is a simple line-based configuration format.  There are many variations;
this SRFI requires support for at least the following:

  *  Comments begin with ';' and are removed from lines.
     
  *  Blank lines and leading and trailing whitespace are ignored.
  
  *  The beginning of a section is marked by a line beginning with `[` and ending with `]`.
     Sections do not nest.  The name of a section is the characters between the brackets.
     
  *  Other lines containing `=` are treated as key-value pairs within the current section or, if
     they precede any section line, as key-value pairs belonging to an unnamed section.
     Whitespace immediately before or after the `=` is ignored.
     
  *  Otherwise unrecognizable lines are treated as keys whose value is the empty string.
  
`(read-ini-file `[ *port* ]`)`

Read lines from *port* (default is the value of `(current-input-port)`) until end of file
and interpret them as above.  The result is an alist mapping the section names (treated as
symbols) to subordinate alists whose keys are symbols and whose values are strings.  The
order of sections and keys in the file is preserved; no merging between identical section
names or keys is performed.  The unnamed section if it exists has a key of `#f`.

`(write-ini-file `*alist* [ *port* ]`)`

Writes an alist in the format above to *port* (default is the value of `(current-output-port)`).
The format is checked for validity before any writing is done.



