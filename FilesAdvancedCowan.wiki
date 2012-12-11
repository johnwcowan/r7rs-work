This is a WG2 proposal for a `(scheme files advanced)` library providing R6RS-equivalent file I/O operations in an upward compatible way, with the following exceptions:

 * custom ports
 * the `standard-*-port` procedures
 * the ability to change a binary port to textual after it has been used, though the `binary` setting-list key allows mixed textual and binary operations on the same port, with limitations


== Procedures from files library ==

The procedures `open-input-file`, `open-binary-input-file`, `open-output-file`, `open-binary-output-file`, `with-input-from-file`, `with-output-from-file`, `call-with-input-file`, and `call-with-output-file` are the same as in the `(scheme files)` library, except that they accept ''settings lists'' as well as filenames.  Settings lists allow various features of the port to be set when it is opened; see SettingsListsCowan for details.

The procedures `file-exists?` and `delete-file` are the same as in the `(scheme files)` library.

== Port procedures ==

`(port-position `''port''`)`

Returns the current position of ''port''.  For a binary port, returns the index of the position at which the next byte would be read from or written to the port as an non-negative exact integer. For a textual port, returns a value of some implementation-dependent type representing the port’s position; this value may be useful only as the ''position'' argument to `set-port-position!`, if the latter is supported on the port.

`(can-get-port-position? `''port''`)`

Returns `#t` if the port supports the `port-position` operation, and `#f` otherwise.

`(set-port-position! `''port''` `''position''`)`

Changes the current position of ''port''.  If port is a binary port, and ''position'' is a non-negative exact integer object, it is used as the index of the position at which the next byte will be read from or written to the port. If port is a textual port, and ''position'' is the return value of a preceding call to `port-position` on ''port'', then the next character will be read or written from that position.  If ''port'' is a binary output port and the ''position'' is beyond the current end of the data, `set-port-position!` will succeed, but the contents of any intervening positions are unspecified.

Calling this procedure on ''port'' resets the buffer state, allowing a binary operation to follow even if the previous operation was textual, an input operation to follow even if the previous operation was an output, or an output operation to follow even if the previous operation was an input.

`(can-set-port-position? `''port''`)`

Returns `#t` if the port supports the `set-port-position!` operation, and `#f` otherwise.

`(port-settings  `''port''`)`

Returns an approximation to the settings list used to create ''port''.  The order of keys may not be the same, some keys may be omitted if they have no effect on the implementation or if they explicitly specify the implementation default, and some values may be different if they have the same effect on the implementation.  If no settings list was used, the list `(path `''filename''`)` is returned.

== I/O procedures ==

`(binary-port-eof? `''port''`)`

Returns `#t` if the next attempt to read a byte from ''port'' would return an eof-object, and `#f` otherwise.

`(textual-port-eof? `''port''`)`

Returns `#t` if the next attempt to read a character from ''port'' would return an eof-object, and `#f` otherwise.

`(read-string! `''string'' [[|''port'' [ ''start'' [ ''end'' ]] ] ] `)`

Reads the next ''end'' − ''start'' characters, or as many as are available before the end of file, from the binary input ''port'' into ''string'' in left-to-right order beginning at the ''start'' position. If ''end'' is not supplied, reads until the end of ''string'' has been reached. If ''start'' is not supplied, reads beginning at position 0. Returns the number of characters read.
If no characters are available, an end-of-file object is returned.

`(read-lines ` [[|''input-port'' ]]`)`

Read all remaining characters in ''input-port'' as if by `read-line`, and return a list of strings representing the lines.

`(read-all-bytes ` [''port'']`)`

Returns a bytevector consisting of all the bytes that can be read from ''port'' before an eof-object is returned, or an eof-object if there are none.  The default port is the value of `(current-input-port)`.

`(read-all-chars ` [[|''port'' ]]`)`

Returns a string consisting of all the characters that can be read from ''port'' before an eof-object is returned, or an eof-object if there are none.  The default port is the value of `(current-input-port)`.

`(read-all ` [[|''port'' ]]`)`

Returns a list consisting of all the Scheme objects that can be read from ''port'' (as if by `read`) before an eof-object is returned, or an eof-object if there are none.  The default port is the value of `(current-input-port)`.

`(write-line `''string'' [[|''port'' ]]`)`

Write ''string'' to ''port'' as if by `write-string`, then write a newline to ''port'' as if by ''newline''.

== Conversion procedures == 

`(string->bytevector `''string'' [[|''settings-list'' [ ''start'' [ ''end'' ]] ] ] `)`

Converts ''string'' from ''start'' to ''end'' to a bytevector using the keys `encoding`, `newline`, `encoding-error`, and possibly other implementation-specific keys in ''settings-list'', and returns the string.

`(bytevector->string `''bytevector'' [[|''settings-list'' [ ''start'' [ ''end'' ]] ] ] `)`

Converts ''bytevector'' from ''start'' to ''end'' to a string using the keys `encoding`, `newline`, `encoding-error`, and possibly other implementation-specific keys in ''settings-list'', and returns the bytevector.

== File procedures ==

`(rename-file `''oldname newname''`)`

Renames a file named ''oldname'' to be named ''newname''.  Returns an unspecified value if it succeeds; otherwise, an error satisfying `file-error?` is raised.