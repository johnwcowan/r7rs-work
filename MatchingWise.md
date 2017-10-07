== Pattern Matching ==

This is a proposal for a WG2 pattern matching library.  The motivation of
this proposal is to provide a pattern-matching facility similar to that in
the ML family of languages [[Harper|2008]], as such a facility allows the
succinct expression of some algorithms in a language very similar to the
underlying mathematics of the problem set.

The Pattern Matching package can be imported as `(rnrs match)`, which
provides new versions of several forms from the `(rnrs core)` library, and
one form from the `(rnrs control)` library.  Renaming or prefixing can be
used to make the pattern-matching forms provided by `(rnrs match)` available
without replacing forms from `(rnrs core)` and `(rnrs control)`.

Pattern matching algorithms have been provided as an extension to standard
Scheme syntax by a number of existing implementations, including Bigloo
[[Serrano|2010]], Chicken Scheme [[Chicken|Team 2009]], Racket [[Flatt|2010]] and
others.  Many of these implementations have been influenced by a widely used
platform independent pattern matching library by Andrew K. Wright [Wright
1994].  A more recent library by Alex Shinn [[Shinn|2010]] provides similar
functionality in a mostly R5RS-compliant implementation.

A similar pattern matching language for macro definition is also provided by
the core `syntax-case` form.

In addition, very limited forms of non-pattern value matching are supported
by the argument syntax of the R6RS Scheme `lambda`, `case-lambda`, and
`define` forms.  A limited pattern functionality is provided in Common Lisp
by the `DESTRUCTURING-BIND` macro.

Where the above sources for pattern matching come into conflict, this
proposal attempts to provide compatibility first with the RNRS `syntax-case`
form, and secondly to the existing pattern matching implementations.


== Matching ==

The forms in this library work by attempting to ''match'' a Scheme value of
arbitrary complexity against one or more patterns.  If such a match
succeeds, zero or more variable bindings are created for the scope of a
corresponding body expression or expressions.  Match failure generally
triggers attempted matching against another pattern specified in the same
form, and a ''match error'' is generated if none of the provided patterns
match the given value.


=== Simple Patterns ===

A pattern can be

  * a boolean, number, string, character, or byte-vector literal, an empty
    vector literal, or the empty list

    examples:
{{{
	#\A
	42
	#()
	()
}}}

    in this case, the value MUST match in the sense of `equal?`, and no
    variable is bound by this match.

  * a quoted datum

    examples:
{{{
	'b
	(quote (a b (c d)))
}}}

    in this case, the value MUST match in the sense of `equal?`, and no
    variable is bound by this match

  * the special symbol `_`

    in this case, any value will match, and no variable is bound by this
    match.

  * any other symbol

    examples:
{{{
	a
	matched-val
}}}

    in this case, any value will match, and that symbol is bound to the
    matched value for the scope of the body expression or expressions of the
    pattern matching form used.


=== Aggregate Patterns ===

Patterns can be aggregated into more complex patterns in a couple of ways.
When this is done, multiple constituent patterns ("subpatterns") can be
required to match, and all symbols bound to a value by any matching
subpattern are bound for the scope of the body expression or expressions of
the pattern matching form used.

It is a syntax violation for the same unquoted symbol to appear more than
once in a pattern, except for the special symbols `_' and `...'.

An aggregate pattern can be

  * a proper list of N subpatterns

    examples:
{{{
	(a b c)
	(a '= b)
	(_ middle _)
	(a (b c) d)
}}}

    in this case, any list value of the same length where each subpattern in
    the pattern list matches the corresponding element in the value list
    will match, and the bindings corresponding to each subpattern will be
    carried out.

  * an improper list of one or more subpatterns

    examples:
{{{
	(a b . c)
	(a (b c) . d)
}}}

    in this case, every subpattern but the last MUST match the corresponding
    element of the value list, and the last subpattern of the improper
    pattern list MUST match the corresponding element of the value list if
    the value list is improper and has the same length as the pattern list,
    or the (possibly empty) remainder of the value list otherwise, and the
    bindings corresponding to each subpattern will be carried out.

  * a proper or improper list of one or more subpatterns with the literal
    `...` symbol occurring once or more in any position except either the
    first position of any list, or the last position of an improper list.

    examples:
{{{
	(a ...)
	(a ... b c)
	(a (b c) ... d)
	(a b ... . d)
}}}

    in this case, the pattern list is matched as in the above proper or
    improper list cases, except that the subpattern before the `...` symbol
    can match zero or more instances of the pattern preceding the `...`
    symbol.  Variables bound by a subpattern preceding a `...` are bound to
    a (possibly empty) list of all values matched by the pattern as if by a
    `map` of the `match` operator for that subpattern over each individual
    value matched.

  * a vector of one or more subpatterns

    examples:
{{{
        #(a b c)
        #(1 b 3)
        #((a b) (c d))
}}}

    in this case, any vector value of the same length where each subpattern in
    the pattern vector matches the corresponding element in the value vector
    will match, and the bindings corresponding to each subpattern will be
    carried out.


  * a vector of N subpatterns with the literal `...` symbol in any position
    except the first

    examples:
{{{
	#(a ...)
	#(a b ... c)
	#(1 ... b)
	#((a b) ... (c d))
}}}

    in this case, the pattern list is matched as in the above vector case,
    except that the subpattern before the `...` symbol can match zero or
    more instances of the pattern preceding the `...` symbol.  Variables
    bound by a pattern preceding a `...` are bound to a (possibly empty)
    list of all values matched by the pattern as if by a `map` of the
    `match` operator over each individual value matched.


== Exceptions ==

In all of the matching forms, if the input value cannot be matched against any
of the supplied patterns, an error MUST be signaled.


== Basic Forms ==

The `(rnrs match)` library exports the form

      `(match ....)`	   	 	syntax

`match` has the syntax

{{{
	(match <val>
          (<pat> <body> <body> ...)
          (<pat> <body> <body> ...)
            ...)
}}}

and attempts to match the input value ''<val>'' against each specified
pattern ''<pat>'' in order.  For the first pattern which successfully
matches the input value, `match` MUST evaluate the corresponding expressions
''<body>'' in the scope of the bindings created by the successful match, and
MUST return the value of the last corresponding ''<body>'' expression,
which MUST be evaluated in tail context.

Note that it is not required that each pattern ''<pat>'' be an aggregate
pattern, but the meaning of a simple pattern ought to be considered
carefully.  In particular, a pattern consisting of a single symbol will
always match, and bind the entire list of arguments to that symbol (this can
be used in a manner similar to the `else` clause in a `cond` form), while a
pattern consisting of the special symbol `_` will always match without
binding.

If no pattern ''<pat>'' successfully matches the input value ''<val>'', an
error MUST be signaled.


== Derived Forms ==

For convenience, the following derived forms are also exported by the `(rnrs
match)` library.  They can be imported with a prefix such as `match-` so
that they will not collide with the Scheme core, or can be allowed to
override it to make pattern matching appear to be integral to Scheme.

      `(lambda ....)`	   	 	syntax

      `(case-lambda ....)`	    	syntax

      `(let ....)`	 		syntax

      `(let* ....)`	   	 	syntax

      `(letrec ....)`	   	 	syntax

`lambda`, which is used to create a function of zero or more arguments which
matches those arguments, taken as a list, against a single pattern, has the
syntax

{{{
	(lambda <pat>
	  <body> <body> ...)

}}}

and is equivalent to

{{{
	(core:lambda id
	  (match id
	    (<pat> <body> <body> ...)))
}}}

`case-lambda`, which is used to create a function of zero or more arguments
which matches those arguments, taken as a list, against one or more patterns,
has the syntax

{{{
	(case-lambda
	  (<pat> <body> <body> ...)
	  (<pat> <body> <body> ...)
	  ...)
}}}

and is equivalent to

{{{
	(core:lambda id
	  (match id
	    (<pat> <body> <body> ...)
	    (<pat> <body> <body> ...)
	    ...))
}}}


`let`, analogous to core `let`, has the syntax

{{{
	(let ((<pat> <expr>) ...)
	  <body>
	  <body>
	  ...)
}}}

Each ''<expr>'' MUST be evaluated and then pattern matched against the
corresponding pattern ''<pat>'' in the current environment (in some
unspecified order). The expressions ''<body>'' MUST then be evaluated in an
environment containing all of the bindings resulting from each pattern
match, and the value of the last ''<body>'' expression, which MUST be
evaluated in tail context, MUST be returned.

If any of the patterns ''<pat>'' fail to match the value of the
corresponding expression <expr>, the expressions in ''<body>'' MUST NOT be
evaluated, and an error MUST be signaled.  It is not specified which of the
expressions '<expr>' will have already been evaluated when this exception is
raised.


`let*`, analogous to core `let*`, has the syntax

{{{
	(let* ((<pat> <expr>) ...)
	  <body>
	  <body>
	  ...)
}}}

`let* `is similar to `let`, except that each `(<pat> <expr>)`
clause MUST be evaluated and then pattern matched sequentially, in
left-to-right order, and identifiers bound by matching each pattern
''<pat>'' match MUST be available in the environment in which each
subsequent expression ''<expr>'' is evaluated.

If any of the patterns ''<pat>'' fail to match the value of the
corresponding expression <expr>, none of the subsequent expressions
''<expr>'' and none of the expressions ''<body>'' MUST be
evaluated, and an error MUST be signaled.


`letrec`, analogous to core `letrec`, has the syntax

{{{
	(letrec ((<pat> <expr>) ...)
	  <body>
	  <body>
	  ...)
}}}

`letrec `is similar to `let`, except that each expression ''<expr>'' clause
MUST be evaluated in an environment containing all bindings from all
patterns ''<pat>''.  As with the core `letrec` form, each variable to be
bound by a pattern ''<pat>'' of the `letrec` form is bound (in a new
environment) to a fresh location prior to the evaluation of any expression
''<expr>'', the expressions ''<body>'' are then evaluated (in some
unspecified order) in this environment.

As with `let`, if any of the patterns ''<pat>'' fail to match the value of
the corresponding expression <expr>, none of the expressions ''<body>'' MUST
be evaluated, and an error MUST be signaled.  It is not specified which of
the expressions '<expr>' will have already been evaluated when this
exception is raised.


== Notes ==

The following questions are not dealt with in this draft, but merit
consideration as part of the RNRS standardization process, or in future RNRS
revisions:

  * Quasipattern forms, defined with quasiquotation syntax, as supported by
    several existing implementations.  These are most powerful when used
    with the splicing operator `,@` to extend `...` notation to a repeating
    sequence of more than one subpattern occuring directly within a list.

  * boolean patterns defined with `and`, `or`, or `not`, allowing more
    complex combinations of subpatterns in an aggregate pattern.  The `or`
    form carries with it the possibility of binding a matching value to a
    given variable in some matches of the same pattern, but not others,
    while the `not` form does not perform variable binding itself (but is
    often used within an `and` form).

  * Extending the provided pattern grammar to include constructors of RNRS
    syntactic or procedural record types, or enumeration types, either
    automatically at record or enumeration type definition time, or by
    manually specifying records to be eligible for such matching through
    some additional syntax.

    Such support might allow, given an enumeration constructor `e` and a
    record constructor `make-r`, pattern matches on the form

{{{
	((make-r w x y) (e z))
}}}

    against a value of

{{{
	(a b)
}}}

    where `a` is a record of the type constructed by `r`, and `b` is an
    enumeration of the type constructed by `e`, resulting in the variables
    `w`, `x`, `y`, and `z` being bound for the scope of the match operator
    used.  Exceptions, as a special case of record types, could be similarly
    matched, and pattern-matching variants of `with-exception-handler` and
    `guard` might be provided.

    Such matching is not specified in this proposal, as it would require
    changes to the `(rnrs records ...)` and `(rnrs enums)` libraries.

    Typically, such interaction would extend to definining appropriate
    pattern-matching scaffolding when a new record or enumeration
    constructor is defined.   Such an interaction ought to be considered
    if this form of pattern matching is later aded to the core language.

  * Allowing the specification of guard expressions in `match` clauses which
    must evaluate to true for that clause to match.  This has proved a very
    useful feature in languages of the ML family [[Ruegg|2009]], but adds some
    complexity to the matching implementation.  If this is examined in
    future versions, a variant of the ''fender'' syntax of the RNRS
    `syntax-case` library form or the `=>` failure continuation support of
    the Wright [[Wright|1994]] pattern matching library merit particular
    consideration.

  * Laying the groundwork for replacing the core version of `lambda`, `let`,
    `let*`, and `letrec` with the versions provided by the `(rnrs match
    core)` library in a future revision in the RNRS series

  * Unifying the syntax used here with that of the core syntax-case form,
    which uses a very similar pattern matching language

  * Vector-based treatment of `...` matches within a vector aggregate
    pattern.  For consistency with existing implementations, the a pattern
    such as
{{{
	#(a ...)
}}}
    the variable `a` to a ''list'' of zero or more matching values.  Once
    can imagine cases where a ''vector'' of such matches would be useful,
    but this may be harder to implement, and would differ from historical
    practice.


== Sources ==

[[Chicken|Team 2009]] Chicken Team, "Pattern Matching" from the Chicken Scheme
Wiki, revision 14108, 2009, available at
http://wiki.call-cc.org/man/3/Pattern%20matching

[[Harper|2008]] Harper Robert, ''Programming in Standard ML'', 2008, Carnegie
Melon,
ch. 5, available at http://www.cs.cmu.edu/~rwh/smlbook/

[[Flatt|2010]] Flatt, Matthew and PLT, ''Reference: Racket'', Version 5.0,
2010, available at
http://download.racket-lang.org/docs/5.0/pdf/reference.pdf

[[Ruegg|2009]] Ruegg, Michael "Pattern Matching in Scala", University of
Applied Sciences, Rapperswil, 2009, available at
http://wiki.ifs.hsr.ch/SemProgAnTr/files/PatternMatchingInScala.pdf
which provides an overview of pattern matching in Haskell, Erlang,
F#, XSLT, and Prolog as well as in Scala

[[Serrano|2010]] Serrano, Manuel ''Bigloo: A practical Scheme compiler,
User manual for version 3.4b'', 2010, Bigloo Project, ch. 7, available at
http://www-sop.inria.fr/mimosa/fp/Bigloo/doc/bigloo.html

[[Shinn|2010]] Shinn, Alex ''match.scm'', 2010, available at
http://synthcode.com/scheme/match.scm
a USENET post describing the initial release of this library is
available at
https://groups.google.com/forum/?pli=1#msg/comp.lang.scheme/Rc2gH1YJpDA/_S8R500jQQkJ

[[Wright|1996]] Wright, Andrew K. "Pattern Matching for
Scheme", 1996, Rice University, available at
http://download.plt-scheme.org/doc/103p1/pdf/match.pdf

== Source ==

This proposal was put together by Jim Wise from postings on the
discuss@r6rs.org mailing list.