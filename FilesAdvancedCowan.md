# Advanced I/O

These operations are not implementable portably.

## Character encoding schemes

A *codec* is an algorithm for translating between a sequence of bytes
and a sequence of characters and vice versa.
Converting bytes to characters is *decoding*; converting characters to bytes is *encoding*.
Typical codecs include ASCII, UTF-8, ISO-8859-1, and ISO-2022-JP;
each one is named by a case-insensitive string, which is passed as the *codec* argument
to the procedures of this SRFI.

`(codec-valid? `*codec*`)`

Returns `#t` if it is possible to convert between bytes and Scheme strings using *codec*,
and `#f` otherwise.

`(codec-conversion-supported? `*codec1 codec2*`)`

Returns `#t` if it is possible to convert between *codec1* and *codec2*, and `#f` otherwise.

`(codec-guess `*bytevector* [ *hint* ]`)`

Returns a codec name which might be the correct encoding of the bytes in *bytevector*.
If *hint* is provided, it is a codec name
which gives a possibly helpful indication of the correct codec.
For example, given a hint of "UTF-8",
this procedure might return "UTF-8" if *bytevector* contains well-formed UTF-8,
or "ISO-8859-1" if it does not.

`(native-codec)`

Returns the codec name which the implementation natively uses for textual port I/O.

## Transcoders

A transcoder is an immutable object that combines a codec with an end-of-line style and an error-handling style.

`(make-transcoder `*codec eol-style error-style*`)`

The *codec* argument is a name of the codec to be used.

The *eol-style* argument is one of the symbols `cr`, `lf`, or `crlf`,
and indicates whether an end-of-line character (U+000A, `#\newline`) is encoded
as CR (`#xD`), LF (`#xA`), or CR+LF (`#xA` followed by `#xD`).
This is independent of the CES.
Implementations may allow other symbols as well.

Any of these bytes or byte sequences, and possibly others, are decoded to `#\newline`,
whatever the value of *eol-style*.

The *error-style* argument specifies how to handle an attempt to encode a character
that cannot be encoded by *codec* or an attempt to decode a byte or byte sequence
that cannot be decoded by *codec*.
It is one of the symbols `replace` or `raise`.
If the symbol is `raise`, an error is signaled,
but if the symbol is `replace`, then the following rules are applied:

A character that cannot be encoded using *ces* is replaced by a `#\uFFFD` character,
or if that cannot be encoded either, by a `#\?` character.
A byte or byte sequence that cannot be decoded using *ces* is decoded as an `#\uFFFD` character.
If that character either is not known to the Scheme implementation
or is forbidden in strings by the Scheme implementation, then it is decoded as an `#\?` character.

`(native-eol-style)`

Returns the symbol which represents the end-of-line style
which the implementation natively uses for textual port I/O.

`(native-error-style)`

Returns the symbol which represents the error-handling style
which the implementation natively uses for textual port I/O.

## Conversion ports

`(make-decoded-input-port `*binary-input-port transcoder* [ *size* ]`)`

Returns a textual input port which, when read from, reads bytes from *binary-input-port*,
decodes them as characters using *ces*, and provides the characters to the reader.
If specified, *size* is the size in bytes of an internal conversion buffer
to be used for decoding.

`(make-encoded-output-port `*binary-output-port transcoder* [ *size* ]`)`

Returns a textual output port which, when characters are written to it,
encodes them as bytes using *transcoder*, and writes the bytes to *binary-port*.
If specified, *size* is the size in bytes of an internal conversion buffer
to be used for decoding.

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
