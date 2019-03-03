Errata from the [final draft of R7RS](https://bitbucket.org/cowan/r7rs/src/draft-10/rnrs/r7rs.pdf).  This list is unofficial.

1\.  In Section 3.1 (Variables, syntactic keywords, and regions), the claim in paragraph 2 that all variable binding
constructs can be explained in terms of `lambda` does not apply to top-level bindings.

2\.  Section 7.1.5 (Transformers) doesn't make it clear that a top-level `syntax-rules` pattern must be a list pattern, 
not a vector pattern or an identifier pattern.

3\.  In Section 6.3 (Booleans), the procedure `boolean=?` is defined to return `#t` if the arguments are all booleans
and are either all `#t` or all `#f`.  The words "are all booleans and" incorrectly suggest that the value is `#f` if at least one argument is not a boolean.  In fact it is an error to apply `boolean=?` to non-booleans.

4\. In Section 4.1.2 (Literal expressions), the examples `'#` and `#` should be `'#\a` and `#\a` respectively.

5\. In section 7.1.1 (Lexical structure), the escape sequence `\|` is not shown as permitted in <string element>.
The list in Section 6.7 shows that it is equivalent to `|`.  Similarly, the escape sequences `\"` and `\\` should be allowed
in <symbol element>.  This makes the same escape sequences valid in both strings and symbols.

6\. In Section 7.3 (Derived expression types), the syntax-rules definition of `case` is incorrect;
the fourth syntax rule must be moved after the fifth to avoid an improper match against
the fourth rule when `=>` is present.  Here is the correct version:

```
 (define-syntax case
   (syntax-rules (else =>)
     ((case (key ...)
        clauses ...)
      (let ((atom-key (key ...)))
        (case atom-key clauses ...)))
     ((case key
        (else => result))
      (result key))
     ((case key
        (else result1 result2 ...))
      (begin result1 result2 ...))
     ((case key
        ((atoms ...) => result))
      (if (memv key '(atoms ...))
          (result key)))
     ((case key
        ((atoms ...) => result)
        clause clauses ...)
      (if (memv key '(atoms ...))
          (result key)
          (case key clause clauses ...)))
     ((case key
        ((atoms ...) result1 result2 ...))
      (if (memv key '(atoms ...))
          (begin result1 result2 ...)))
     ((case key
        ((atoms ...) result1 result2 ...)
        clause clauses ...)
      (if (memv key '(atoms ...))
          (begin result1 result2 ...)
          (case key clause clauses ...)))))
```

7\. In Section 7.1.1, the lexical rule <special initial> incorrectly omits `@`.

8\. Bibliographic reference ![13] should link to [SRFI 4](http://srfi.schemers.org/srfi-4/srfi-4.html).

9\. In section 4.2.2, add "interleaving evaluations with assignments" to the definition of `letrec*`.
Replace the meaningless example with this:

```
;; Returns the arithmetic, geometric, and
;; harmonic means of a nested list of numbers
(define (means ton)
  (letrec*
     ((mean
        (lambda (f g)
          (f (/ (sum g ton) n))))
      (sum
        (lambda (g ton)
          (if (null? ton)
            (+)
            (if (number? ton)
                (g ton)
                (+ (sum g (car ton))
                   (sum g (cdr ton)))))))
      (n (sum (lambda (x) 1) ton)))
    (values (mean values values)
            (mean exp log)
            (mean / /))))
```

Note that evaluating `(means '(3 (1 4)))` returns three values: 8/3, 2.28942848510666 (approximately), and 36/19.

10\.  In section 7.1.5, add <bytevector> to the alternatives for the <pattern datum> rule.

11\. Section 1.3.4 refers to "the initial environment" containing `*`, which is not true for programs.
It should refer instead to "an environment containing the base library".

12\.  An example in section 5.3.3 refers to `integer-sqrt` instead of `exact-integer-sqrt`.

13\. In section 4.3.1, the body of a `let-syntax` expression is said to contain "one or more definitions";
it should be "zero or more definitions".

14\. In section 6.6 for `char-upcase/downcase/foldcase` and 6.7 for `string-upcase/downcase/foldcase`
the reader is referred to UAX #29, but it should be TR #44.

15\. In section 7.3, the definition of `case-lambda` should use `letrec-syntax`, not `let-syntax`.

16\. In section 4.3.2 (but not in 7.1.3), change <literal> to <pattern literal> to avoid confusion.

17\. (removed, already in R7RS-small)

18\. In the first bullet of the "Incompatibilities with R6RS" section, for "have to be be" read "have to be".

19\. In numeric tower bullet of the "Incompatibilities with R6RS" section, for "but the R6RS procedures `real-valued?`,
`rational-valued?`, and `integer-valued?` were not" read "but the semantics of the R6RS procedures `real?`, `rational?`, and `integer?` were not adopted.  (Note that the R5RS/R7RS semantics are available in R6RS using `real-valued?`, `rational-valued?`, and `integer-valued?`)".

20\. From [Richard Kelsey's R5RS errata](http://www.mumble.net/~kelsey/r5rs-errata.html): In the explanation of
`list-ref`, for "fewer than *k* elements", read "*k* or fewer elements". Thus, `(list-ref '(x) 1)` is an error.

21\. In Appendix B, for "All algebraic operations except `/` produce exact values given exact inputs" read
"The algebraic operations `+`, `-`, `*`, and `expt` where the second argument is a non-negative integer all
produce exact values given exact inputs".

22\. In Appendix A, the `(scheme r5rs)` library should export `syntax-rules`, `else`, `...`, `=>` and `_`.

23\. In the definition of `string-for-each`, for "the elements of the *lists*" read "the elements of the *strings*".

24\. The value of the example `(real? 2.5+0.0i)` in Section 6.2.6 is shown as `#f`, as in R6RS.  This contradicts the prose explanation.  No resolution of the conflict has been reached as yet.

25\. In the definition of `fold-char`, the sentence "If the argument is an uppercase letter, the result will be either a lowercase letter or the same as the argument if the lowercase letter does not exist or is not supported by the implementation" is no longer appropriate as of Unicode 8.0, when folding began to convert Cherokee lower-case letters to upper case.  Replace it with "If the result of folding is not supported by the implementation, the argument is returned".

26\. In the definition of \<feature requirement> in Section 7.1.7, for <library name>
read `(library `<library name>`)`.

27\. The claim in the definition of `current-second` that the TAI-UTC offset as of January 1, 1970 was ten seconds is factually
incorrect per [the USNO's list of offsets](http://maia.usno.navy.mil/ser7/tai-utc.dat).  The correct value for that date
is 8.000082 seconds.

28\. The definition of `char-numeric?` refers to a nonexistent Unicode property Numeric_Digit.
The intention was to refer to characters whose Numeric_Type property is Decimal,
which is defined by Unicode as those which have a non-empty value in field 6 of the UnicodeData.txt file.
This is equivalent to characters with a General Category of `Nd`.
The simplest fix is to change the relevant paragraph of Section 6.6 from "Numeric_Digit" to
"Numeric_Type=Decimal".  (Note: R6RS uses the term "Numeric", which also is
not a Unicode property.)
