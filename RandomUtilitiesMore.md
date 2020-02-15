This is an unformatted dump of things that Mikael More thinks R7RS-large should provide somehow, from #scheme:

## Utilities

Also some kind of general purpose routines for strings, chars   (there are some fundamental things here that are clearly not in SRFI 13 and 14, like, `string-strip`, `string-replace-char`, `join`, a `string-append` form that takes non-string arguments too a bit like `print`, `string<->object` serialization), lists (same here - `list-add!`, `list-rm!`, `filter!`).

u8vector/blob (same here - `u8vector<->string` and file) tree structures like WT tree, mailboxes,  exception handling that also catches the continuation for stack trace printouts and other handling, queues... `let-vector`,  `number<->string` with formatting parameters.

`let-args` is essentially `(define-macro (let-args var def . code) (quasiquote (apply (lambda ,def . ,code) ,var)))`.

`map-w-index` and `for-each-w-index` add a first argument to the called lambda, which is the zero-based index  of the element you are at.

## Custom ports

See R6RS or SRFI 181.