## Directories

Directories are containers for files.
Directories are named by *filenames*, which can be either strings
or implementation-defined objects.

## Procedures

`(directory? `*filename*`)`

Returns `#t` if *filename* is the name of a directory,
or `#f` if it is not or the answer is not determinable.

`(make-directory `*filename*`)`

Creates a directory named *filename*.  An exception is signaled if this cannot be done.

`(delete-directory `*filename*`)`

Deletes a directory named *filename*.  An exception is signaled if this cannot be done.

`(open-directory `*filename*`)`

Returns a *directory port*.
It is not defined whether `port?` and `input-port?`
return `#t` or `#f` to a directory port.

*Filename* can be a string or a *settings list*
as described in [SettingsListsCowan](SettingsListsCowan.md).
In addition to the keys `path` and `encoding` specified there,
the key `hidden` can have the values `none` (don't expose hidden files),
`all` (expose all files), and `dots` (expose all files except `.` and `..`).
On Posix systems, hidden files have names beginning with `.`;
on Windows, hidden files are named `.`, `..`, or have the `hidden` file attribute.
On other systems, the definition of a hidden file is implementation-dependent.

`(directory-port? `*obj*`)`

Returns `#t` if *obj* is a directory port.

`(read-directory `*directory-port*`)`

Returns the next available filename as a string
(or, when necessary, an implementation-dependent object).
If no filenames are available, returns an eof-object.
Filenames are returned in arbitrary order.

`(close-directory `*directory port*`)`

Close *directory port*.
