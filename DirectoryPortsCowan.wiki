== Directory ports ==

This proposal depends on SettingsListsCowan.  See DirectoriesCowan for a proposal for creating and destroying directories.

== Procedures ==

`(open-directory `''filename''`)`

Returns a ''directory port''. It is not defined whether `port?` and `input-port?` return `#t` or `#f` to a directory port.

''Filename'' can be a string or a ''settings list'' as described in SettingsListsCowan.  In addition to the keys `path` and `encoding` specified there, the key `hidden` can have the values `none` (don't expose hidden files), `all` (expose all files), and `dots` (expose all files except `.` and `..`).  On Posix systems, hidden files have names beginning with `.`; on Windows, hidden files are named `.`, `..`, or have the `hidden` file attribute.  On other systems, the definition of a hidden file is implementation-dependent.

`(directory-port? `''obj''`)`

Returns `#t` if ''obj'' is a directory port.

`(read-directory `''directory-port''`)`

Returns the next available filename as a string (or, when necessary, an implementation-dependent object).  If no filenames are available, returns an eof-object.  Filenames are returned in arbitrary order.

`(close-directory `''filename''`)`

Close ''directory port''.
