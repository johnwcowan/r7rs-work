## Settings lists

In this proposal, a filename passed to any of `open-input-file`, `open-binary-input-file`, `open-output-file`, `open-binary-output-file`, `with-input-from-file`, `with-output-from-file`, `call-with-input-file`, and `call-with-output-file` may be specified either as a string (as in R7RS-small) or as a *settings list*, which is an alist where every key is a symbol.  The value is the cadr of the association rather than its cdr.

Quasiquote syntax is useful in creating settings lists.  Specifying a string instead of a list is equivalent to specifying the settings list `((path `*string*`))`.  Quasiquotation is often useful in creating a settings list with fixed keys but one or more variable values.

The following keys are defined by this proposal:

`path`::

> Specifies the filename to be opened.  The interpretation of filenames is implementation-dependent.  There is no default value, but implementations MAY accept other keys in lieu of this one for opening files or file-like objects that don't have string names.  In particular, filenames on Posix are really u8-vectors with some u8 values disallowed, and filenames on Windows are really u16-vectors with some u16 values disallowed, and while most names of actual files are representable as strings, some may not be.

`textual`::

> If the value of this key is true, then a call to `open-input-file`, `open-output-file`, `with-input-from-file`, `with-output-from-file`, `call-with-input-file`, or `call-with-output-file` will allow textual operations and respond `#t` to `textual-port?`.  If the `binary` key is present and has a true value also, binary operations may also be possible.  However, textual and binary operations may not be mixed on a port unless the value of the `char-buffer` key is `none` or unless a `set-port-position!` operation intervenes.  The value `#f` causes the key to be ignored.

`binary`::

> If the value of this key is true, then a call to `open-input-file`, `open-output-file`, `with-input-from-file`, `with-output-from-file`, `call-with-input-file`, or `call-with-output-file` will allow binary operations and respond `#t` to `binary-port?`.  If the `textual` key is present and has a true value also, textual operations may also be possible as explained under `textual`.  It is an error to specify the value as `#f` on calls to `open-binary-input-file` or `open-binary-output-file` (which would be self-contradictory); otherwise, the value `#f` causes the key to be ignored.

`bidirectional`::

> If the value of this key is true, then the port will respond `#t` to both `input-port?` and `output-port?`, and both input and output operations are accepted.  However, on a file port it is an error to perform an input operation immediately followed by an output operation or vice versa, unless a call to `set-port-position!` intervenes.  The input and output side of the port may be closed separately using `close-input-port` and `close-output-port`; `close-port` will close both sides.

`append`::

> If the value of this key is true, the file pointer is moved to the end of file before every write operation.  This is POSIX and Win32 O_APPEND.

`create`::

> If the value of this key is true, and the file specified by `path` does not exist, it will be created.  This is POSIX and Win32 O_CREAT.

`exclusive`::

> If the value of this key is true, and the file specified by `path` exists, an error that satisfies `file-error?` is signalled.  This is only effective if the value of the `create` key is also true.  This is POSIX and Win32 O_EXCL.

`truncate`::

> If the value of this key is true, and the file specified by 'path' exists, it is truncated to zero length.  This is only effective if the file is being opened for output.  This is POSIX and Win32 O_TRUNC.

`buffer`::

> For an output port, the value of this key defines when an output operation flushes a buffer associated with the output port. For an input port, the value defines how much data will be read to satisfy read operations.  The value `none` means there is no buffering; the value `block` means there is a buffer of an implementation-dependent size.  Other values MAY be supported by an implementation.  Buffer sizes are implementation-dependent.  The default value is implementation-dependent.

`char-buffer`::

> Specifies what kind of character buffering is present on a textual port.  Character buffering affects how much translation between characters and bytes is done all at once.  The value `none` means no character buffering is employed; the value `block` means there is a buffer of an implementation-dependent size for translation.  The value `line` is the same as `block`, except that on output, the character buffer as well as the binary buffer (if any) is flushed after each newline is output.  Other values MAY be supported by an implementation.  Buffer sizes are implementation-dependent.  The default value is implementation-dependent.

`encoding`::

> Specifies what character encoding to use on a textual port.  The (case insensitive) value `us-ascii` MUST be supported.  The values `iso-8859-1` and `utf-8` SHOULD be supported if the implementation contains the appropriate repertoire of characters.  The value `native` means to use whatever the environmental default encoding is, and is the same as not providing this key.  Other values MAY be supported by an implementation; if so, they SHOULD appear in the [IANA list of encodings](http://www.iana.org/assignments/character-sets).  The default value is implementation-dependent.

> If a BOM (Byte Order Mark, U+FEFF) is present at the beginning of input on a port encoded as UTF-8, it is skipped.  A BOM is not automatically written on output.  Implementations MAY provide a way around this.

`newline`::

> Specifies how to translate newlines on a textual port.  The value `none` means that no translation is performed.  The values `cr`, `lf`, and `crlf` cause `#\newline` to be translated to CR, LF, or CR+LF respectively on output; all of them also cause all of CR, LF, and CR+LF to be translated to `#\newline` on input.  The value `native` means whatever is the native line-end encoding, and is the same as not providing this key.  Other values MAY be supported by an implementation.  The default value is implementation-dependent.

`encoding-error`::

> Specifies what action to take if a character cannot be encoded as bytes or a sequence of bytes cannot be decoded as a character in the specified encoding of a textual port.  The value `ignore` means that the untranslatable byte or character is ignored.  The value `raise` means that an error is signalled.  The value `replace` means that an untranslatable byte is translated to `#\xFFFD;` if that character is available and can appear in strings in the implementation, or `#\?` if not, and an untranslatable character is translated to the byte encoding of `#xFFFD;` if there is one, or of `#\?` if not.  The default value is implementation-dependent.

`posix-permissions`::

> Specifies the permissions with which the file is created as an exact integer.

Issue: Should flag variables be defined for convenience?

Implementations MAY support other keys, SHOULD warn if they detect keys or values they do not understand or implement, and MAY signal an error in such cases.

Settings lists are also used by other proposals:  [NetworkPortsCowan](NetworkPortsCowan.md), [DirectoryPortsCowan](DirectoryPortsCowan.md), [ProcessPortsCowan](ProcessPortsCowan.md).

## Buffering rationale

When a file is opened in binary mode, only the binary buffer (if any) is used.  But in textual mode both buffers are relevant.  When reading, the binary buffer is filled with bytes, and then a byte-to-character conversion is done which fills the character buffer.  When the latter is empty, it's refilled from the binary buffer; when the binary buffer is empty, it's refilled from the stream.  When writing, the character buffer is used to fill the byte buffer.

It's efficient to do both I/O and character conversion en bloc when possible, but when switching between modes, it's undesirable to have a character buffer even in textual mode (though a binary buffer is still useful) so that the implementation doesn't convert bytes that should
not be converted.

Of course, when the character encoding is an 8-bit one, the implementation doesn't need to jump through these hoops.

## Settings lists in other contexts

See [LetSettingsKendal](LetSettingsKendal.md) for a proposal to help users write their own procedures accepting settings lists.
