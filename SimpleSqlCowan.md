## Opaque object types

Database, statement, result-set

## Databases

(open-sql-database filename mode) -> db 

mode is symbol: read, write, create

(sql-database? obj) -> boolean

(sql-interrupt db timespec) -> undefined

Attempt to interrupt requests from any thread.

(sql-timeout db timespec) -> undefined

Set timeout in seconds and nanoseconds for each database request.
Signal an error if request does not complete.

(sql-in-transaction db thunk) -> what thunk returns

When proc returns, roll back unless sql-commit has been called.

(sql-commit db) -> unspecified

(sql-rollback db) -> unspecified

(sql-in-transaction? db) -> boolean

Is the database currently running a transaction?

## Statements and result sets

(sql-statement db code bindings) -> statement

It is an error to mutate either code or bindings; this enables caching
of either compiled but unbound statements, fully bound statements, or both.

(sql-statement? obj) -> boolean 

(sql-exec db statement) -> implementation-dependent

Use this procedure when statement does not return a result set.

(sql-result-set db statement thunk) -> whatever thunk returns

Executes statement and calls thunk, passing a result-set object.

(sql-read result-set) -> list

Returns a list of Scheme objects representing the next available
row of the result-set.  NULL is represented by the symbol nil.

(sql-read-all result-set) -> list of lists

Returns all available rows in this result-set.

(sql-for-each proc result-set) -> unspecified

Applies each result of result-set to proc for its side effects.

(sql-map->list proc result-set) -> list

Applies each result of result-set to proc and returns a list of the results.

(sql-fold proc knil result-set) -> any

Call proc on each result of result-set and the current state (initially knil).
Order of arguments?

## Blobs

ISSUE: Should this be gotten rid of?  It's not strictly necessary,
but handling really large blobs without it will be messy.
Ideally it should give us ports, but adding ports to a
Scheme implementation is hard.

(sql-make-blob db table column rowid length) -> blob

Make a blob of length bytes, all of which are 0, in the specified location.
The current position of the blob is set to 0.
Blobs cannot be extended.

(sql-open-blob db table column rowid [old-blob]) -> blob

Open the blob at the specified location.
The current position of the blob is set to 0.
If old-blob is provided, it may be reused.

(sql-blob? obj) -> boolean

(sql-blob-read blob count) -> bytevector

Reads *count* bytes from the current position of *blob*
into a newly allocated bytevector and returns it.
Stops if the end of the blob is reached.

(sql-blob-read! to at blob count) -> unspecified

Reads into bytevector *to* starting at position *at* until
*count* bytes are read or *to* is full, whichever comes first.

(sql-blob-read-all blob) -> bytevector

(sql-blob-write blob count bytevector [start end]) -> unspecified

Writes the bytes of *bytevector* from *start* (inclusive)
to *end* (exclusive) at the current position of *blob*.

(sql-blob-position blob) -> exact integer

(sql-blob-set-position! blob position) -> unspecified

## Exceptions

(make-sql-exception code message) -> sql-exception

(sql-exception? obj) -> boolean

(sql-exception-code sql-exception) -> exact integer

(sql-exception-message sql-exception) -> string

## Meta

This is not standardized over databases, so provided here.
There isn't much, but what there is is generally useful.

(sql-tables db) -> list of symbols

The symbols correspond to database tables (including views)
accessible to the current user.

(sql-columns db table) -> list of symbols

The symbols represent column names, and appear in ordinal position.

(sql-column-type db table column) -> string

Returns the declared type of the specified table and column.
The result is a string whose possible values depend on the database.
