## INI files

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
  
`(make-ini-file-generator `*inport*`)`

Returns a generator that reads one or more lines from *inport* and returns a list of three strings:
the current section name, the key, and the value.  If no section names have been read, the
section name is the empty string.  When the port returns an end of file object, it is closed and
the generator returns an end of file object.

`(make-ini-file-accumulator `*outport*`)`

Returns an accumulator that writes to outport.  If the argument passed to the accumulator
is a list of three strings, a key-value line, preceded if necessary by a section line,
is written.  If the argument is a single string, it is prefixed by `"# "` and written out.
In either case, the accumulator returns an unspecified value.
If the argument is an end of file object, *outport* is closed and the end of file object returned.