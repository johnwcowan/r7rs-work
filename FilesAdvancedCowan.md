This SRFI defines library providing R6RS-equivalent file I/O operations in an upward compatible way,
with the following exceptions:

* no custom ports
* no `standard-*-port` procedures
* no ability to change a binary port to textual

## Procedures from files library

The procedures `open-input-file`, `open-binary-input-file`, `open-output-file`, `open-binary-output-file`,
`with-input-from-file`, `with-output-from-file`, `call-with-input-file`, and `call-with-output-file`
are the same as in the `(scheme files)` library, except that they accept *settings lists* as well as filenames.
Settings lists allow various features of the port to be set when it is opened;
see [SettingsListsCowan](SettingsListsCowan.md) for details.

The procedures `file-exists?` and `delete-file` are the same as in the `(scheme files)` library.

## Port procedures

`(port-position `*port*`)`

Returns the current position of *port*.  For a binary port, returns the index of the position at which the next byte would be read from or written to the port as an non-negative exact integer. For a textual port, returns a value of some implementation-dependent type representing the port’s position; this value may be useful only as the *position* argument to `set-port-position!`, if the latter is supported on the port.

`(can-get-port-position? `*port*`)`

Returns `#t` if the port supports the `port-position` operation, and `#f` otherwise.

`(set-port-position! `*port*` `*position*`)`

Changes the current position of *port*.
If port is a binary port, and *position* is a non-negative exact integer object,
it is used as the index of the position at which the next byte will be read from or written to the port.
If port is a textual port, and *position* is the return value of a preceding call to `port-position` on *port*,
then the next character will be read or written from that position.
If *port* is a binary output port and the *position* is beyond the current end of the data,
`set-port-position!` will succeed, but the contents of any intervening positions are unspecified.

Calling this procedure on *port* resets the buffer state,
allowing a binary operation to follow even if the previous operation was textual,
an input operation to follow even if the previous operation was an output,
or an output operation to follow even if the previous operation was an input.

`(can-set-port-position? `*port*`)`

Returns `#t` if the port supports the `set-port-position!` operation, and `#f` otherwise.

`(port-settings  `*port*`)`

Returns an approximation to the settings list used to create *port*.
The order of keys may not be the same,
some keys may be omitted if they have no effect on the implementation
or if they explicitly specify the implementation default,
and some values may be different if they have the same effect on the implementation.
If no settings list was used, the result is implementation-dependent.
In any case the result will at least include the `path` property.

`(make-pipe)`

Returns two ports, the read and write endpoints of a Posix or Windows pipe.
If the OS does not support pipes, an error is signaled.

`(select-ports `*timeout port* ...`)`

Blocks until at least one of the
*ports* is ready for operation or until the timeout
has expired.  For an input port this means that it either has data
sitting its buffer or that the underlying file descriptor has data
waiting.  For an output port this means that it either has space
available in the associated buffer or that the underlying file
descriptor can accept output.

The *timeout* value can be used to force the call to time out
after a given number of seconds.  A value of `#f` means to wait
indefinitely.  A zero value can be used to poll the ports.

Returns a list of the ports ready for
operation.  Note that this list may be empty if the timeout expired
before any ports became ready.

`(select-port-channels `*timeout port* ...`)`

The same as `select-ports` except
that it only looks at the operating system objects the ports refer
to, ignoring any buffering performed by the ports.

This procedure is intended for situations where the
program is not checking for available data, but is rather waiting
until a network or similar port has established a connection.
It should be used with care: for
example, if an input port has data in the buffer but no data
available on the underlying file descriptor,
will block, even though a read
operation on the port would be able to complete without blocking.

## Convenience procedures

`(binary-port-eof? `*port*`)`

Returns `#t` if the next attempt to read a byte from *port* would return an eof-object, and `#f` otherwise.
The default port is the value of `(current-input-port)`.

`(textual-port-eof? `*port*`)`

Returns `#t` if the next attempt to read a character from *port* would return an eof-object, and `#f` otherwise.
The default port is the value of `(current-input-port)`.

`(read-lines ` [*input-port*]`)`

Read all remaining characters in *input-port* as if by `read-line`,
and return a list of strings representing the lines.
The default port is the value of `(current-input-port)`.

`(read-all-bytes ` [*port*]`)`

Returns a bytevector consisting of all the bytes that can be read from *port* before an eof-object is returned,
or an eof-object if there are none.  The default port is the value of `(current-input-port)`.

`(read-all-chars ` [*port*]`)`

Returns a string consisting of all the characters that can be read from *port* before an eof-object is returned,
or an eof-object if there are none.  The default port is the value of `(current-input-port)`.

`(read-all ` [*port*]`)`

Returns a list consisting of all the Scheme objects that can be read from *port* (as if by `read`)
before an eof-object is returned, or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(write-line `*string* [*port*]`)`

Write *string* to *port* as if by `write-string`, then write a newline to *port* as if by *newline*.

`(print `*obj* ...`)`

Write each *obj* to the port that is the value of `(current-output-port)` separated by single spaces
and followed by a newline.

`(debug-print `*obj* ...`)`

Write each *obj* to the port that is the value of `(current-error-port)` separated by single spaces
and followed by a newline.


## File procedures

`(file-exists `*filename*`)`

Returns `#t` if a file named *filename* exists, and `#f` otherwise.

`(rename-file `*oldname newname*`)`

Renames a file named *oldname* to be named *newname*.  Returns an unspecified value if it succeeds; otherwise, an error satisfying `file-error?` is raised.

