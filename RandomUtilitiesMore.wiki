This is an unformatted dump of things that Mikael More thinks R7RS-large should provide somehow, from #scheme:

== Utilities ==

Also some kind of general purpose routines for strings, chars   (there are some fundamental things here that are clearly not in SRFI 13 and 14, like, `string-strip`, `string-replace-char`, `join`, a `string-append` form that takes non-string arguments too a bit like `print`, `string<->object` serialization), lists (same here - `list-add!`, `list-rm!`, `filter!`).

u8vector/blob (same here - `u8vector<->string` and file) tree structures like WT tree, mailboxes,  exception handling that also catches the continuation for stack trace printouts and other handling, queues... `let-vector`,  `number<->string` with formatting parameters.

`let-args` is essentially `(define-macro (let-args var def . code) (quasiquote (apply (lambda ,def . ,code) ,var)))`.

`map-w-index` and `for-each-w-index` add a first argument to the called lambda, which is the zero-based index  of the element you are at.

== Custom ports ==

Scheme brings an ports/IO API with open-file, call-with-input-file , open-tcp-client etc. where the Scheme environment exports this functionality to the program this is great. it can include char encoding for instance. and object serialization of course.

I've gotten into uses like HTTPS and GZIP and HTTP and really any protocol out there.

My firm conclusion is that the absolutely finest, and possibly only really reliable way to implement these in Scheme (apart from as block procedures that take or deliver an u8vector/blob) is that they need to export ''Scheme ports''.

e.g., `(open-ssl-channel (open-tcp-client args))` => ''ssl-channel-port''.

The only major detail to be added to this is that there's a certain need of mixing binary and character data, as is done in HTTP for instance.

I got into this need for "application layer" port implementations  that have specific exports more or less at per-IO primitive level, from trying to implement an SSL channel atop bidirectional u8vector port.

That failed miserably as the SSL logics code would not be up to date about whether there was EOF or not etc.

This might be too implementation specific for any standard to touch it, i'm very well aware of this.

Though, ability to abstract protocols and IO is at the core of delivering practical things, which is the subject of the larger language as I got it.