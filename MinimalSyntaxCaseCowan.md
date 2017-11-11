From [Eli Barzilay's posting](http://tmp.barzilay.org/foof.txt), lightly edited for continuity:

The "minimal API" of a *syntax-case* system is made of `syntax-case`, `syntax`, `syntax->datum`, and `datum->syntax`.

With `syntax-case` it is extremely straightforward to create something like `syntax-e` if it's not built in -- and `syntax-case` itself is then *no longer necessary*; `syntax->datum` can be done in exactly the same way (applied recursively), so it's just a convenience.  This leaves you with two things: `syntax` as a core lexical-scope-preserving quotation notation, and `datum->syntax` as a way to construct new identifiers unhygienically.  `Datum->syntax` is therefore the only real "complex API" here, and it's complexity is (very unsurprisingly) very similar to explicit renaming or syntactic closures, since in all three cases you take a symbol and choose a lexical scope to put it in.

[this really sufficient?](Is)
