== Directories ==

Directories are containers for files.  Directories are named by ''filenames'', which can be either strings or implementation-defined objects.

== Procedures ==

`(directory? `''filename''`)`

Returns `#t` if ''filename'' is the name of a directory, or #f if it is not, or the answer is not determinable.

`(make-directory `''filename''`)`

Creates a directory named ''filename''.  An exception is signaled if this cannot be done.

`(delete-directory `''filename''`)`

Deletes a directory named ''filename''.  An exception is signaled if this cannot be done.

== Reading directories ==

See DirectoryPortsCowan.