No Scheme standard requires `(eq? #\a #\a)` to return `#t`, but most implementations do.  The exceptions:

SISC, IronScheme, KSi, Scheme 9, UMB, Llava: returns `#f`.

XLisp: returns `()`, which is the same as `#f`.

Schemik, SIOD: no character type.
