This is a tree of the standard R6RS conditions.
Periods are used for indentation to make the relationships easier to see;
thus `&warning`, `&serious`, `&message`, `&irritants`, and `&who`
are all on the same level.
Names in parentheses are the visible fields.

```
&condition
..&warning
..&serious
....&error
......&i/o
........&i/o-read
........&i/o-write
........&i/o-invalid-position (position)
........&i/o-filename (filename)
..........&i/o-file-protection
............&i/o-file-is-read-only
............&i/o-file-already-exists
............&i/o-file-does-not-exist
.......&i/o-port (port)
.........&i/o-decoding
..........&i/o-encoding (char)
....&violation
......&assertion
......&non-continuable
......&implementation-restriction
........&implementation-violation
........&no-infinities
........&no-nans
......&lexical
......&syntax (form, subform)
......&undefined
..&message (message)
..&irritants (irritants)
..&who (who)
```
