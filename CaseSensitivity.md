Historically one of the most heated flame-wars in the Scheme
community.  We have to decide whether we want to preserve backwards
compatibility with standards up through the R5RS, or reaffirm the
switch to case-sensitivity in the R6RS.  The charter gives precedence
to the former, so unless we can reach a rough consensus (> 90%) to the
contrary, the default should remain case-insensitive.

Whatever we choose we should realize that some implementations are likely
to use a different default in their preferred environments.

Note that the default case-sensitivity is orthogonal to the issue of
whether and how behavior can be toggled on a per-file or
per-expression basis.  If there is a toggle, it may be specified by
WG1 for both groups, or only by WG2.

Below are some common arguments for each side.  The number of arguments
on each side is irrelevant - people should weigh each argument according
to its importance.

**Pro-folding:**

* R[0-5]RS and IEEE Scheme compatible
* possibly easier for beginners not expecting case distinctions to be significant
* allows using different cases as stylistic differences in source
* prevents using separate identifiers which differ only in case, which is considered poor style
* less confusion when code is read aloud, e.g. variable names

**Pro-preserving:**

* R6RS compatible
* the popular default in a number of modern implementations
* easier compatibility with external data (XML, some filesystems, FFIs)
* users from other languages usually expect case sensitivity
* using different cases for the same identifier can be confusing
* transliterations of math formulae may be easier to read with case distinctions
* more general - can write a (begin/ci ...) macro to implement the alternative
* allows using separate identifiers which differ only in case (e.g., capitalizing a class but not an instance)
* simpler to implement in the presence of complex character sets
* most programmers won't be able to recognize case-folded equivalents from languages they don't speak

**Proposals:**

* [CaseSensitivityArcfide]
