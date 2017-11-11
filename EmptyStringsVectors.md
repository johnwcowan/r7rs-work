## Comparing empty strings and vectors with eq?

The vast majority of the Schemes in the test suite return `#f` to both `(eq? (vector) (vector))` and
`(eq? (string) (string))`.  I also tested bytevectors in those implementations that made it easy to do so.  Here are the cases that returned `#t`:

Chez returns `#t` for both empty strings and empty vectors.

Icarus (but not Vicare) and Chibi return `#t` for empty vectors only.

Ypsilon returns `#t` for all three cases.

NexJ returns `#t` for empty vectors but not empty strings.
