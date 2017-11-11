Now we have modules, I like the idea of gathering modules of similar functionality under a clean hierarchy, instead of a flat namespace.

* Move SRFI-9 record systems to (scheme records srfi-9), this leave space for other record systems in (scheme records ...)

* Rename (scheme io) to (scheme ports) as it deals with ports management, which are not exclusively used for I/O (as strings ports for example). Don't require it into the base because some implementation don't need this.

* Don't provide an umbrella module for I/O.

* Leave interaction-environment in the (scheme repl) module.

* Move case-lambda in (scheme arguments case-lambda)

* Move (scheme multiple-values) back to the base.

* Rename (scheme unicode) to (scheme char)

* Move char-alphabetic?, char-numeric?, char-upper-case?, char-lower-case? and char-whitespace? to (scheme char)

* Move the normalization procedures to (scheme char normalization).

* Move parameters functions to (scheme parameters)
>
