## Abstract

This SRFI defines a library that can be imported in place of the R7RS-small library`(scheme file)`.
It provides R6RS-equivalent file I/O operations plus a few more generally portable ones,
in an upward compatible way.  The following R6RS concepts are not supported:

* custom ports
* `standard-*-port` procedures
* no ability to change a binary port to textual

## Issues

1. Can `let-settings-list` be implemented with syntax-rules alone?

2. Should flag constants be supplied to make the key `posix-permissions`
easier to use?

## File open procedures

The following procedures are present in `(scheme file)` and can be called in the same
way, but accept an additional *settings* argument as documented below.  If this
argument is omitted, they are semantically identical to their `(scheme file)` counterparts.
The meaning of the *settings* argument is explained below.

`(call-with-input-file `*string* [*settings*] *proc*`)`
`(call-with-output-file `*string* [*settings*] *proc*`)`
`(with-input-from-file `*string* [*settings*] *thunk*`)`
`(with-output-to-file `*string* [*settings*] *thunk*`)`
`(open-input-file `*string* [*settings*]`)`
`(open-binary-input-file `*string* [*settings*]`)`
`(open-output-file `*string* [*settings*]`)`
`(open-binary-output-file `*string* [*settings*]`)`
`(open-output-file `*string* [*settings*]`)`
`(open-binary-output-file `*string* [*settings*]`)`

## Settings lists

In this SRFI, a *settings list*
is a property list alternating between symbols (known as *keys*) and values.
Quasiquote syntax is useful in creating settings lists
with fixed keys but one or more variable values.

The following keys are defined by this proposal:

`bidirectional`

If the value of this key is true,
then the port will respond `#t` to both `input-port?` and `output-port?`,
and both input and output operations are accepted.
However, on a file port it is an error to perform an input operation
immediately followed by an output operation or vice versa,
unless a call to `set-port-position!` intervenes.
The input and output side of the port may be closed separately
using `close-input-port` and `close-output-port`; 
`close-port` will close both sides.
The default value is `#f`.

`append`

If the value of this key is true, the file pointer is
moved to the end of file before every write operation.
This is POSIX and Win32 O_APPEND.  The default value is `#f`.

`create`

If the value of this key is true and the file does not exist, it will be created.
This is POSIX and Win32 O_CREAT.  The default value is implementation-dependent.

`exclusive`

If the value of this key is true and the file exists, an error that satisfies `file-error?` is signaled.
This is only effective if the value of the `create` key is also true.  This is POSIX and Win32 O_EXCL.
The default value is `#f`.

`truncate`

If the value of this key is true and the file exists, it is truncated to zero length.
This is POSIX and Win32 O_TRUNC.  The default value is implementation-dependent.

`buffer`

For an output port, the value of this key defines when an output operation
flushes a buffer associated with the output port.
For an input port, the value defines how much data will be read to satisfy read operations.
The value `none` means there is no buffering;
the value `block` means there is a buffer of an implementation-dependent size;
an exact integer specifies the size of the buffer.
Other values may be supported by an implementation. 
The default value is implementation-dependent, and may be specified explicitly
by using the value `#f`.

`char-buffer`

Specifies what kind of character buffering is present on a textual port.
Character buffering affects how much translation between characters and bytes is done all at once.
The value `none` means no character buffering is employed;
the value `block` means there is a buffer of an implementation-dependent size for translation;
the value `line` is the same as `block`, except that on output,
the character buffer as well as the binary buffer (if any) is flushed after each newline is output;
an exact integer specifies the size of the buffer.
Other values may be supported by an implementation.  Buffer sizes are implementation-dependent.
The default value is implementation-dependent, and may be specified explicitly
by using the value `#f`.

When a file is opened in binary mode, only the binary buffer (if any) is used.
But in textual mode, both buffers are relevant.
When reading, the binary buffer is filled with bytes,
and then a byte-to-character conversion is done which fills the character buffer.
When the latter is empty, it's refilled from the binary buffer;
when the binary buffer is empty, it's refilled from the file.
When writing, the character buffer is used to fill the byte buffer.

It's efficient to do both I/O and character conversion en bloc when possible.
Of course, if the character encoding is an 8-bit one,
or if the external and internal encodings of strings are the same,
the implementation doesn't need to jump through these hoops,
and may ignore the value of this key.

`encoding`

Specifies what character encoding to use on a textual port.
The (case insensitive) values `us-ascii`,
`iso-8859-1`, and `utf-8` must be supported.
The value `native` means to use whatever the environmental default encoding is,
and is the same as not providing this key.
Other values may be supported by an implementation; if so, they should appear in the
[IANA list of encodings](http://www.iana.org/assignments/character-sets).
The default value is implementation-dependent, and may be specified explicitly
by using the value `#f`.

If a BOM (Byte Order Mark, U+FEFF) is present at the beginning of input
on a port encoded as UTF-8, UTF-16, or UTF-32, it is skipped.
A BOM is not automatically written on output.  Implementations may provide a way around this.

`newline`

Specifies how to translate newlines on a textual port.
The value `none` means that no translation is performed;
the values `cr`, `lf`, and `crlf` cause `#\newline`
to be translated to CR, LF, or CR+LF respectively on output
All of them also cause all of CR, LF, and CR+LF to be translated to `#\newline` on input.
Other values may be supported by an implementation.
The default value is implementation-dependent, and may be specified explicitly
by using the value `#f`.

`encoding-error`

Specifies what action to take if a character cannot be encoded as bytes
or a sequence of bytes cannot be decoded as a character in the specified encoding of a textual port.
The value `ignore` means that the untranslatable byte or character is ignored;
the value `raise` means that an error is signalled;
the value `replace` means that an untranslatable byte is translated to `#\xFFFD`
and an untranslatable character is translated to the byte encoding of `#xFFFD;`
The default value is implementation-dependent, and may be specified explicitly
by using the value `#f`.

`posix-permissions`

Specifies the permissions with which the file is created as an exact integer.
The default value is implementation-dependent, and may be specified explicitly
by using the value `#f`.

Implementations may support other keys, should warn if they detect
keys or values they do not understand or implement,
and may signal an error in such cases.

## Port procedures

`(port-position `*port*`)`

Returns the current position of *port*.
For a binary port, returns the index of the position at which the next byte
would be read from or written to the port as an non-negative exact integer.
For a textual port, returns a value of some implementation-dependent type
representing the port's position;
this value may be useful only as the *position* argument to
`set-port-position!`, if the latter is supported on the port.

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
If the OS does not support pipes, an error that satisfies `file-error?` is signaled.

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

Returns `#t` if the next attempt to read a byte from *port*
would return an eof-object, and `#f` otherwise.
The default port is the value of `(current-input-port)`.
It is an error to call this procedure
if the `peek-u8` procedure is not supported by *port*

`(textual-port-eof? `*port*`)`

Returns `#t` if the next attempt to read a character from *port*
would return an eof-object, and `#f` otherwise.
The default port is the value of `(current-input-port)`.
It is an error to call this procedure
if the `peek-char` procedure is not supported by *port*

`(read-lines ` [*input-port*]`)`

Read all remaining characters in *input-port* (as if by `read-line`),
and return a list of strings representing the lines.
The default port is the value of `(current-input-port)`.

`(read-all-bytes ` [*port*]`)`

Returns a bytevector consisting of all the bytes
that can be read from *port* before an eof-object is returned,
or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(read-all-chars ` [*port*]`)`

Returns a string consisting of all the characters (as if by `read-char`)
that can be read from *port* before an eof-object is returned,
or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(read-all ` [*port*]`)`

Returns a list consisting of all the Scheme objects
that can be read from *port* (as if by `read`)
before an eof-object is returned, or an eof-object if there are none.
The default port is the value of `(current-input-port)`.

`(write-line `*string* [*port*]`)`

Write *string* to *port* (as if by `write-string`),
then write a newline to *port* (as if by `newline`).

`(print `*obj* ...`)`

Write each *obj* to the port (as if by `display`)
that is the value of `(current-output-port)`
separated by single spaces and followed by a newline.

`(debug-print `*obj* ...`)`

Write each *obj* to the port (as if by `display`)
that is the value of `(current-error-port)`
separated by single spaces and followed by a newline.


## File procedures

These procedures are the same as in their counterparts `(scheme files)` library.

`(file-exists `*filename*`)`

Returns `#t` if a file named *filename* exists, and `#f` otherwise.

`(rename-file `*oldname newname*`)`

Renames a file named *oldname* to be named *newname*.  Returns an unspecified value if it succeeds; otherwise, an error satisfying `file-error?` is raised.


## Settings lists macro

`(let-settings `let-bindings settings-list*` . `*body*`)`

Expand to a `let` using *let-bindings* to bind *body*.
Before *body* is executed, any variables whose names
appear in *settings-list* are bound to the values that
follow them.

## Implementation

All procedures can be implemented on top of the R6RS
with the exceptions of `pipe`, `select-ports`, and `select-port-channels`,
which are implemented in Scsh.
