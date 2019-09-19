## Opaque object types

Connection, statement, result-set

## Databases

(open-sql-connection database-type connection-string mode) -> connection 

database-type is a symbol used to figure out which database driver to use.
connection-string is meaningful to the driver.
mode is an alist mapping symbols to strings or exact integers,
also meaningful to the driver.

(sql-connection? obj) -> boolean

(sql-interrupt connection timespec) -> undefined

Attempts to interrupt any requests currently in progress on connection.
This must be called from a different thread than the requesting thread.

(sql-timeout connection timespec) -> undefined

Set timeout in seconds and nanoseconds for each request on the connection.
Signal an error if request does not complete.

(sql-in-transaction connection thunk) -> what thunk returns

ISSUE: When proc returns, rollback or commit?

(sql-commit connection) -> unspecified

(sql-rollback connection) -> unspecified

(sql-in-transaction? connection) -> boolean

Is the connection currently running a transaction?

## Statements and result sets

(sql-statement connection code bindings) -> statement

It is an error to mutate either code or bindings; this enables caching
of either compiled but unbound statements, fully bound statements, or both.

(sql-statement? obj) -> boolean 

(sql-exec connection statement) -> implementation-dependent

Use this procedure when statement does not return a result set.

(sql-result-set connection statement thunk) -> whatever thunk returns

Executes statement and calls thunk, passing a result-set object.

(sql-read result-set) -> list

Returns a list of Scheme objects representing the next available
row of the result-set.  NULL is represented by the symbol `null`.

(sql-read-all result-set) -> list of lists

Returns all available rows in this result-set.

(sql-for-each proc result-set) -> unspecified

Applies each result of result-set to proc for its side effects.

(sql-map->list proc result-set) -> list

Applies each result of result-set to proc and returns a list of the results.

(sql-fold proc knil result-set) -> any

Call proc on each result of result-set and the current state (initially knil).
Order of arguments?

(sql-column-fold proc-list column-list knil-list result-set) -> any

Folds specified columns simultaneously,
where *column-list* specifies the names of the columns of interest,
*proc-list* is the folding procs corresponding to the chosen columns,
and *knil-list* is the initial values of the current states of each fold.

## Blobs

ISSUE: Should this be gotten rid of?  It's not strictly necessary,
but handling really large blobs without it will be messy.
Ideally it should give us ports, but adding ports to a
Scheme implementation is hard.

(sql-make-blob connection table column rowid length) -> blob

Make a blob of length bytes, all of which are 0, in the specified location.
The current position of the blob is set to 0.
Blobs cannot be extended.

(sql-open-blob connection table column rowid [old-blob]) -> blob

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

(sql-exception-code sql-exception) -> database type-dependent object

(sql-exception-message sql-exception) -> string

## Meta

This is not standardized over database types, so provided here.
There isn't much, but what there is is generally useful.

(sql-tables connection) -> list of symbols

The symbols correspond to database tables (including views)
accessible to the current user.

(sql-columns connection table) -> list of symbols

The symbols represent column names, and appear in ordinal position.

(sql-column-type db table column) -> string

Returns the declared type of the specified table and column.
The result is a string whose possible values depend on the database type.
Note that in SQLite, because of its dynamic typing, it is not guaranteed
that the Scheme type of objects stored in the column have the type specified
by this procedure, or even that they all have the same Scheme type.
