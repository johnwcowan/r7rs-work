## Substitution

`(substitute `*newobj oldobj list*`)`

`(substitute! `*newobj oldobj list*`)`

`(string-substitute `*newchar oldchar string*`)`

`(string-substitute! `*newchar oldchar string*`)`

`(vector-substitute `*newobj oldobj vector*`)`

`(vector-substitute! `*newobj oldobj vector*`)`

These procedures return a newly constructed container (list, string, or vector) in which all elements of the original container that are the same (in the sense of `eqv?`) as *oldobj* (or *oldchar*) have been replaced by *newobj* (or *newchar*).  The procedures with `!` in their names do the same thing, but may destructively modify the container.  (Common Lisp SUBSTITUTE, NSUBSTITUTE.)

## Transformation

`(transform `*transformer predicate list*`)`

`(transform! `*transformer predicate list*`)`

`(string-transform `*transformer predicate string*`)`

`(string-transform! `*transformer predicate string*`)`

`(vector-transform `*transformer predicate vector*`)`

`(vector-transform! `*transformer predicate vector*`)`

These procedures return a newly constructed container (list, string, or vector) in which all elements of the container (list, string, or vector) on which *predicate* returns `#t` have been replaced by the result of calling *transformer* on the element.  The procedures with `!` in their names do the same thing, but may destructively modify the container.  (Closely related to Common Lisp SUBSTITUTE-IF, NSUBSTITUTE-IF.)



## Issues

Should these procedures, if accepted, be merged into the SRFI 1, SRFI 13, and SRFI 43 packages?  They are only separated from it on historical grounds.
