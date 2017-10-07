This proposal contains various procedures that accept and return procedures, as well as a few others, drawn from an earlier version of Chicken.  Common Lisp has a few of them too, and more come from [[http://programmingpraxis.com/contents/standard-prelude/|the Standard Prelude from ''Programming Praxis'']].


== Combinators ==

These procedures are documented in an unusual style.  Rather than showing only how the procedures themselves are invoked, it also shows how the returned procedures would be invoked.  This is done in order to make the descriptions easier to understand.  For example, if `complement` were documented in the standard style, the description would say "Returns a procedure which, when applied to an argument, returns `#t` when ''proc'' would return `#f` when applied to the same argument, and `#f` otherwise", which is more convoluted and harder to understand.  However, this is merely a documentation style; it would be pointless to actually invoke these procedures in this fashion.

`((constantly `''obj'' ...`)` ''arg'' ...`)`

Returns the ''objs'' as its values, ignoring ''args''.

`((complement `''proc''`)` ''obj''`)`

Returns `#t` when `(`''proc obj''`)` returns `#f`, and `#f` otherwise.

`((compose `''proc'' ... `)` ''arg'' ...`)`

Passes the ''args'' to the first ''proc'', which returns any number of values.  These are then passed to the next ''proc'', and so on until the final ''proc'' is reached.  If there are no ''procs'', returns its arguments as values.

`((simple-compose `''proc'' ...`)` ''arg''`)`

Passes ''arg'' to the first ''proc'', which returns one value.  This is then passed to the next ''proc'', and so on until the final ''proc'' is reached.  If there are no ''procs'', returns its argument.

`((swap `''proc''`)` ''obj,,1,, obj,,2,,''`)`

Invokes `(`''proc obj,,2,, obj,,1,,''`)`.

`((flip `''proc''`)` ''arg1 arg2''`)`

Returns `(`''proc arg2 arg1''`)`.

`((fst `''proc''`)` ''obj,,1,, obj,,2,,''`)`

Returns ''obj,,1,,''.

`((snd `''proc''`)` ''obj,,1,, obj,,2,,''`)`

Returns ''obj,,2,,''.

`((conjoin `''predicate'' ...`)` ''arg'' ...`)`

Returns `#t` if the ''args'' satisfy all the ''predicates'', and `#f` otherwise.

`((disjoin `''predicate'' ...`)` ''arg'' ...`)`

Returns `#t` if the ''args'' satisfy any of the ''predicate''s.

`((each-of `''proc'' ... `)` ''arg'' ...`)`

Applies each of the ''proc''s in turn to ''args'', discarding the results and returning an unspecified value.

`((all-of? `''predicate''`)`

Applies ''predicate'' to each element of ''list'' in turn, and immediately returns `#f` if ''predicate'' is not satisfied by that element; otherwise returns `#t`.

`((any-of? `''predicate''`)` ''list''`)`

Applies ''predicate'' to each element of ''list'' in turn, and immediately returns `#t` if ''predicate'' is satisfied by that element; otherwise returns `#f`.

`((map-reduce `''mapper reducer''`)` ''list''`)`

Returns ``(apply ''reducer'' `(`''mapper list''`))`.

`((left-section `''proc arg'' ...`)` ''obj'' ...`)`

Applies ''proc'' to ''args'' concatenated with ''objs''.

`((right-section `''proc arg'' ...`)` ''obj'' ...`)`

Applies ''proc'' to ''objs'' concatenated with ''args'', where ''args'' are in reverse order.

== Syntax-like procedures ==

These are Scheme procedures that correspond to basic syntax.  They are derived from [[https://github.com/danfuzz/samizdat/blob/master/doc/library-guide/If.md|the If class of Samizdat]].

`(begin-procedure `''thunk'' ...`)`

Invokes ''thunks'' in order, and returns what the last thunk returns, or an unspecified value if there are no thunks.

`(if-procedure `''value then-thunk'' [[|''else-thunk'' ]]`)`

If ''value'' is true, invokes ''then-thunk'' and returns what it returns.  Otherwise, invokes ''else-thunk'' and returns what it returns, or if ''else-thunk'' is not specified, returns an unspecified value.

`(if-not-procedure `''value else-thunk''`)`

If ''value'' is true, returns an unspecified value.  Otherwise, invokes ''else-thunk'' and returns what it returns.

`(value-procedure `''value then-proc else-thunk''`)`

If ''value'' is true, invokes ''then-proc'' on it and returns what ''then-proc'' returns.  Otherwise, invokes ''else-thunk'' and returns what it returns.

`(case-procedure `''value thunk-alist'' [[|''else-thunk'' ]]`)`

Searches ''thunk-alist'' for ''value'' (as if by `assv`).  If there is no such entry in ''thunk-alist'', invokes ''else-thunk'' and returns what it returns, or returns an unspecified value if ''else-thunk'' is not provided.  If there is a matching entry in ''thunk-alist'', its cdr is invoked as a thunk, and `case-procedure` returns what the thunk returns.

`(and-procedure `''thunk'' ...`)`

Invokes each ''thunk'' in the order given, and returns `#f` immediately if any of them return `#f`.  Otherwise returns the value of the last thunk, or `#t` if there are none.

`(or-procedure `''thunk'' ...`)`

Invokes each ''thunk'' in the order given, and if any of them returns true, `or-procedure` returns that value immediately.  Otherwise returns `#f`.

`(loop-procedure `''thunk''`)`

Invokes ''thunk'' repeatedly.  Does not return.

`(while-procedure `''thunk''`)`

Invokes ''thunk'' repeatedly until it returns `#f`.  Returns an unspecified value.


== Other procedures ==

`(always `''obj'' ...`)`

Ignores its arguments and always returns `#t`. 

`(never `''obj'' ...`)`

Ignores its arguments and always returns `#f`.

`(identity `''obj''`)`

Returns ''obj''.
