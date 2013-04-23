== Ports ==

'''This proposal is obsolete.  Most of it other than settings lists was adopted by WG1.  Settings lists have now become a WG2 proposal at SettingsListsCowan.'''

Here's my proposal for WG1 Scheme ports and I/O facilities.  It's fully upward compatible with R5RS, but takes ideas from R6RS, SRFI 91 and SRFI 6.  Many of the concepts are present in R6RS under other names.

The new features beyond R5RS are:

 * partial control of buffering, character encoding, newline translation, and Scheme case sensitivity
  * Rejected by WG1, see #29 and [[:WG1Ballot2Results|WG1Ballot2Results]]
 * string ports (SRFI-6-compatible)
  * Accepted by WG1, see #30 and [[:WG1Ballot1Results|WG1Ballot1Results]]
 * binary ports
  * Accepted by WG1, see #28 and [[:WG1Ballot2Results|WG1Ballot2Results]]
 * blob ports (the binary version of string ports)
  * Accepted by WG1, see #149 and [[:WG1Ballot3Results|WG1Ballot3Results]]
 * `current-error-port`
  * Accepted by WG1, see #59 and [[:WG1Ballot1Results|WG1Ballot1Results]]
 * `flush-output-port`
  * Accepted by WG1, see #134 and [[:WG1Ballot3Results|WG1Ballot3Results]]
 * `read-line`
  * Accepted by WG1, see #133 and [[:WG1Ballot3Results|WG1Ballot3Results]]
 * `delete-file`, and `file-exists?` from R6RS
  * Accepted by WG1, see #60 and [[:WG1Ballot1Results|WG1Ballot1Results]]

== Port model ==

In this proposal, there are two kinds of ports, binary ports and character ports. Unusually, ''every binary port is automatically a character port'', though not vice versa.  Character ports therefore support character I/O operations, and binary ports support both character I/O operations and binary I/O operations.  Indeed, all I/O operations which are valid on a character port are also valid on a binary port.

This implies that some character encoding must be associated with each binary port so that character I/O can be performed on it.  The only encoding that implementations MUST support is ASCII, so this is relatively cost-free in the simplest case, since ASCII encoding does not require a separate character buffer or encoding translation table, only a few bits for newline translation and case sensitivity.

However, not all character ports are binary ports.  For example, string ports have no concept of character-to-byte encoding, because they only deal with sequences of characters.  Although not specified in this proposal, a further generalization is ''object ports'', whose fundamental I/O unit is the Scheme object. All character ports are object ports, because there is a standard encoding of (most) Scheme objects to characters.

This proposal does not specify any way to create a bidirectional port, but allows for their possible existence in an implementation.  Sockets, terminals, and pipes are all possible examples.

== Filename model ==

In this proposal, a filename may be specified either as a string or as a ''settings list'', which is a list of alternating ''keys'' and ''values'' where every key is a symbol.  Specifying a string is equivalent to specifying the settings list `(path `''string''`)`.  Implementations MUST support the following key:

 `path`::

 Specifies the filename.  The interpretation of filenames is implementation-dependent.  There is no default value, but implementations MAY accept other keys in lieu of this one for opening files or file-like objects that don't have string names.  In particular, filenames on Posix are really u8-vectors with some u8 values excluded, and filenames on Windows are really u16-vectors with some u16 values disallowed, and while most names of actual files are representable as strings, some may not be.

Implementations SHOULD support the following additional keys (if not, then the implementation-dependent default cannot be changed):

 `buffering`::

 Specifies what kind of buffering is present.  The value `#f` means no buffering is employed; `binary` means that there is a binary buffer but no character buffer; `#t` means there are both character and binary buffering.  Other values MAY be specified by an implementation.  Buffer sizes are implementation-dependent.  The default value is implementation-dependent.

 `encoding`::

 Specifies what character encoding to use on a binary port.  The (case insensitive) value `US-ASCII` MUST be supported.  The values `ISO-8859-1` and `UTF-8` SHOULD be supported if the implementation contains the appropriate repertoire of characters.  Other values MAY be supported, which SHOULD appear in the [[http://www.iana.org/assignments/character-sets|IANA list of encodings]].  The default value is implementation-dependent.

 If a BOM (Byte Order Mark, U+FEFF) is present at the beginning of input on a port encoded as UTF-8, it is skipped.  A BOM is not automatically written on output.  Implementations MAY provide a way around this.

 `newline`::

 Specifies how to translate newlines.  The value `#f` means that no translation is performed.  Any other value causes all of CR, LF, CR+LF, NEL (U+0085), CR+NEL, and LS (U+2028) to be translated to `#\newline` on input.  On output, the translation is implementation-dependent.  Other values MAY be specified by an implementation.  The default value is implementation-dependent.

 `case-sensitive`::

 Specifies if Scheme symbols are read from the port case-sensitively or not.  The value `#f` means that upper-case letters in symbols are translated to lower case unless escaped; `#t` means that no translation is done.  The default value is implementation-dependent.

Implementations MAY support other keys, SHOULD warn if they detect keys they do not understand or implement, and MAY signal an error in such cases.


== Port object procedures ==

`(input-port? `''obj''`)`

`(output-port? `''obj''`)`

Same as R5RS, but also return `#t` on bidirectional ports if the implementation provides them.

`(port? `''obj''`)`

Mentioned in R5RS section 3 but not in section 6.6.  Part of R6RS.

`(current-input-port)`

`(current-output-port)`

Same as R5RS.  These are binary ports whose character encoding is implementation-dependent.

`(current-error-port)`

From R6RS.  This is a binary port whose character encoding is implementation-dependent.

`(flush-output-port ` [[|''output-port'' ]] ` ` [[|''character-only?'' ]]`)`

Same as R6RS, except that if the ''output-port'' is omitted, the default port is the current output port.  Drains the character buffer of ''output-port'', if any.  Then, if the port is a binary port, drains the binary buffer, if any.

`(close-input-port `''port''`)`

`(close-output-port `''port''`)`

From R5RS.  `Close-output-port` implicitly calls `flush-output-port` first.

`(close-port)`

Closes both sides of a bidirectional port, if the implementation provides them; otherwise the same as `close-input-port` or `close-output-port` as the case may be.  It is harmless to close a part, or one side of it, if it is already closed.

`(eof-object? `''obj''`)`

Same as R5RS.

`(port-settings-list `''port''`)`

Return the settings list of ''port'' as a list in no particular order.  Additional implementation-defined keys or default values may be present.

== Character I/O procedures ==

`(character-port? `''obj''`)`

Returns `#t` if ''obj'' is a character port.  SRFI 91 calls this `char-port`.  Implementations may define other kinds of character ports.

`(read-char `[[|''character-input-port'' ]]`)`

`(write-char `[[|''character-output-port'' ]]`)`

`(newline `[[|''character-output-port'' ]]`)`

`(peek-char `[[|''character-input-port'' ]]`)`

`(char-ready? `[[|''character-input-port'' ]]`)`

Same as R5RS.  It is an error to output characters not present in the encoding of a ''character-output-port''.

`(read-line `[[|''character-input-port'' ]]`)`

Same as R6RS.  Reads a line from ''port'' (or the current input port) terminated by a `#\newline` character (which may be the result of newline conversion in the port).   The `#\newline` is not part of the returned string.  This is a convenience function.

`(open-input-string `''string''`)`

`(open-output-string)`

`(get-output-string `''output-string-port''`)`

Same as SRFI 6.

== Binary I/O procedures ==

String ports are character ports, but not binary ports, so these procedures do not apply to them.  Implementations MAY support other kinds of binary ports such as process ports or stream socket ports.

Mixing binary I/O with character I/O on the same port is safe if there is no character buffering on that port, but produces undefined behavior otherwise.

`(binary-port? `''obj''`)`

Returns `#t` if ''obj'' is a binary port.  SRFI 91 calls this `byte-port?`.

`(read-u8 `[[|''binary-input-port'' ]]`)`

`(write-u8 `[[|''binary-output-port'' ]]`)`

`(peek-u8 `[[|''binary-input-port'' ]]`)`

`(u8-ready? `[[|''binary-input-port'' ]]`)`

The direct binary analogues of `read-char`, `write-char`, `peek-char`, and `char-ready?` respectively.  They return an exact integer between 0 and 255 rather than a character.  SRFI 91 talks of `byte` rather than `u8`.

`(open-input-blob `''blob''`)`

`(open-output-blob)`

`(get-output-blob `''output-blob-port''`)`

Blobs are (possibly specialized) vectors containing integers from 0 to 255 inclusive.  These procedures are the binary analogues of the SRFI 6 string port procedures.  The term ''blob'' is subject to change.



== File Module ==

This is a separate module because some implementations will not have access to a file system.

`(call-with-input-file `''filename''` `''proc''`)`

`(call-with-output-file `''filename''` `''proc''`)`

`(with-input-from-file `''filename''` `''thunk''`)`

`(with-output-to-file `''filename''` `''thunk''`)`

`(open-input-file `''filename''`)`

`(open-output-file `''filename''`)`

Same as R5RS, except that any ''filename'' argument may be a string or a settings list.

`(delete-file `''filename''`)`

`(file-exists? `''filename''`)`

Same as R6RS.  Only string ''filename''s are supported.


== Reader and Writer Modules ==

These procedures are not in the core because many systems, especially embedded ones, don't require the ability to read or write general Scheme objects, and very small implementations may not want the overhead of a Scheme parser.  Writing may be useful even when reading is not, which is why there are two modules.

Note that implementations may provide ports that are not character ports, such as directory ports or vector ports, and extend these procedures to work on them.

=== Reader Module ===

`(read ` [[|''input-port'' ]]`)`

Same as R5RS.

=== Writer Module ===

`(write `''obj'' [[|''output-port'' ]]`)`

Same as R5RS, but specifies that only ASCII characters may be output (for re-readability).  Non-ASCII characters in symbols, strings, and character literals MUST be escaped.    

`(display `''obj'' [[|''port'' ]]`)`

Same as R5RS.  It is an error to output characters not present in the encoding of ''output-port''. 

== Thanks ==

Thanks to the R5RS and R6RS editors; to Marc Feeley, author of SRFI 91 and Gambit-C; and to Will Clinger, author of SRFI 6.