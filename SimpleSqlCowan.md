Python APSW and Chicken sql-de-lite

## Opaque object types

Database, transaction, statement, query

## Databases

(open-ssql-database filename mode) -> db 

mode is symbol: read, write, create

(ssql-database? obj) -> boolean

(ssql-interrupt db timespec) -> undefined

Attempt to interrupt requests from any thread.

(ssql-timeout db timespec) -> undefined

Set timeout in seconds and nanoseconds for each database request; signal an error if request does not complete/

(ssql-in-transaction db proc) -> what proc returns

The proc is passed transaction object.  When proc returns, roll back unless ssql-commit has been called.

(ssql-transaction? obj) -> boolean

(ssql-commit transaction) -> unspecified

(ssql-rollback transaction) -> unspecified

(ssql-in-transaction? db) -> boolean

Is the database currently running a transaction?

## Statements and queries

(ssql-statement db code bindings) -> statement

It is an error to mutate either code or bindings; this enables caching.

(ssql-statement? obj) -> boolean 

(ssql-exec db-or-trans statement) -> changes

If you specify a db, and a transaction is in effect,
whether the statement is executed within the transaction is implementation-defined.

(ssql-in-query db-or-trans statement proc) -> what proc returns

Invoke proc with a query object.  Query is finalized when proc returns.
If you specify a db, and a transaction is in effect,
whether the query runs within the transaction is implementation-defined.

(ssql-read query) -> list

Returns a list of Scheme objects.  NULL is represented by the symbol nil.

(ssql-read-all query) -> list of lists

Returns all available results on this query.

(ssql-for-each proc query) -> unspecified

Applies each result of query to proc for its side effects.

(ssql-map->list proc query) -> list

Applies each result of query to proc and returns a list of the results.

(ssql-fold proc knil query) -> any

Call proc on each result of query and the current state (initially knil).
Order of arguments?

## Blobs

(ssql-make-blob db table column rowid length) -> blob

Make a blob of zero bytes in the specified location.  Blobs cannot be extended.

(ssql-open-blob db table column rowid [old-blob]) -> blob

Open the blob at the existing location.  If old-blob is provided, it is reused.

(ssql-blob? obj) -> boolean

(ssql-blob-read blob count) -> bytevector

(ssql-blob-read! to at blob count) -> unspecified

(ssql-blob-read-all blob) -> bytevector

(ssql-blob-write blob count bytevector [start end]) -> unspecified

(ssql-blob-position blob) -> exact integer

(ssql-blob-set-position! blob position) -> unspecified

## Exceptions

(make-ssql-exception code message) -> ssql-exception

(ssql-exception? obj) -> boolean

(ssql-exception-code ssql-exception) -> exact integer

(ssql-exception-message ssql-exception) -> string

## Meta

This is not standardized over databases, so provided here.

(ssql-tables db) -> list

Includes views.

(ssql-columns db table) -> list of 5-element lists

The columns appear in ordinal position.
The sub-elements are column name, type (as a string), nullable?, unique?, and default value
