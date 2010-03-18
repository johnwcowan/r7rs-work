There has been some talk discussing record let. Obviously there is a solution already that works with procedural interfaces. There are a few options. Most people have suggested something along the lines of a `LET-RECORD` form that takes either a record object or a record object and its RTD.

`(let-record <record-spec> <bindings> <body> ...)`

Where <record-spec> is one of the following:

`<identifier-to-record>`
`(<identifier-to-record> <record-rtd>)`

And <bindings> is a list of binding forms of the following form:

`(<name> <field>)`

Now, if we allow only the `<identifier-to-record>` syntax, then we force, I believe, the general case of this macro to break down to the procedural layer. This may not be effecient if the distance and barriers of the original record definition and the use are great enough. If we allow the `(<identifier-to-record> <record-rtd>)` form, then it is possible to do this at a purely syntactic level, and potentially generate better code even despite a large number of barriers.

There is another option that has not been mentioned, and I thought I would just bring it up. I have rarely seen `LET-RECORD` in the Scheme code I have read, so unless someone presents a strong case for its general ubiquity or popularity, then it may be better to leave such forms to WG2. In any case, it is also possible to specify that any given record definition (syntactic) could also have an additional specification to bind the name of a `LET-RECORD` form.

{{{ 
(define-record-type MY-RECORD
  (fields X Y Z)
  (parent pnt)
  (binder LET-MY-RECORD))

; Whereupon we may do something as follows:

(define Z (make-my-record 1 2 3))
(let-my-record z ([[A|z]] [[B|y]] [[C|x]]) (list a b c))

; => (3 2 1) 
}}}

This has the benefits that the syntactic form above has, namely, of compile-time error checking and efficiency, but also has the nicer form of the run-time version above. One disadvantage of this approach is that you would not be able to use the same binder on different types of records. I actually think that this may be a good thing. Any time that I have two records that share a common interface, I use a parent type to encapsulate that common functionality. I wonder about the actual utility of accepting any field name for determination of correctness at run-time. At any rate, if we have a procedural layer, then doing a run-time version of `LET-RECORD` is a simple exercize in macros for any user, whereas it is not so easy for an user to create a purely syntactic version of `LET-RECORD` if it is not standardized.

Just another thought.