There seems to be a serious issue with how we handle keywords (by which we mean, things like the `else` and `=>` in `cond`). R5RS was vague on the matter, and each choice of implementation approach seems to have issues.

Thing which need considering:

 * `else` and `=>` in `cond`
 * `else` in `case`
 * Is `syntax-rules` a keyword in this sense?  Some Schemes say yes, others no.
 * And how about `unquote` and `unquote-splicing` within `quote`?

The R6RS standard decided to use explicitly exported and defined auxiliary syntax values, where every syntax-rules literals clause matched hygienically using `free-identifier=?`. This is a particular case of the "Keywords as Bindings" option below.

Let's summarise the issues with different approaches.

== Keywords as symbols ==

One approach is to say that the implementation of `cond` must match a symbol called `else` - so it's purely symbolic equality, rather than bothering about lexical environments.

Problems: See this thread: http://lists.scheme-reports.org/pipermail/scheme-reports/2011-May/000632.html

Particularly, symbolic equality bypasses the normal identifier equality tests used by `syntax-rules`, thereby violating referential transparency and hygiene in some cases.

== Keywords as Bindings ==

Under this scheme, `else` and `=>` are bound to something (a value? a macro? a pineapple?) along with the definition of `cond`, and we check that the same binding is in place when `else` is used in the wild.

This means that if we do:

{{{
(let ((else #f))
(cond
     (else 1))
}}}

...we'll get an error, not 1, as we've rebound `else`; that arm of the `cond` will evaluate `else` and get `#f`, so no arm of the `cond` matches.

However, it has another issue. Andy Wingo, I believe, gave an example of a module that exposes both `compile` (a procedure) and `eval-when` (a macro that uses `compile` as a keyword). It's then impossible to expose `eval-when` into a sandbox, still able to use the `compile` keyword, without then also giving them the `compile` procedure. In this situation you can use keywords-as-symbols, as arbitrary expressions can never appear there; but in a case like `cond`, it would be impossible to export `cond` and all its functions without also exporting a procedure that happened to also be called `else` or `=>`.

== Any others? ==

Please add alternative implementation techniques here, and discuss their problems and characteristics.

== Conclusions ==

It looks like the problems can be avoided by:

 * Not allowing the choice of a keyword or an arbitrary expression. `cond` suffers because one might bind a variable called `else` and then attempt to check the truthfulness of its value as a `cond` test. `case`'s `else` appears in a context where that would be impossible.
 * For cases where keywords and arbitrary expressions (or, more precisely, references to variables) can collide, it looks like "Keywords as Bindings", with the keywords bound to special sentinel syntax values and compared as being the same identifier, is the best approach.

Can anyone refute that?