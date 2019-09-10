Python APSW and Chicken sql-de-lite

## Opaque object types

Database, transaction, statement, query

## Databases

(open-ssql-database filename mode) -> db  ; read, write, create

(ssql-database? obj) -> boolean

(ssql-interrupt db timespec) -> undefined

(ssql-timeout db timespec) -> undefined

(ssql-in-transaction db proc) -> what proc returns

proc is passed transaction

(ssql-transaction? obj) -> boolean

(ssql-commit transaction) -> unspecified

(ssql-rollback transaction) -> unspecified

(ssql-in-transaction? db) -> boolean

## Statements and queries

(ssql-statement db code bindings) -> statement

It is an error to mutate either code or bindings; this enables caching.

(ssql-statement? obj) -> boolean 

(ssql-exec db-or-trans statement) -> changes

(ssql-in-query db-or-trans statement proc) -> what proc returns

proc is passed query

(ssql-read query) -> list

(ssql-read-all query) -> list of lists

(ssql-for-each proc query) -> unspecified

(ssql-map->list proc query) -> list

(ssql-fold proc knil query) -> any

## Blobs

(ssql-make-blob db table column rowid length) -> blob

(ssql-open-blob db table column rowid [old-blob]) -> blob

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

