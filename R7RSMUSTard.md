These are some of the things that an R7RS-small implementation must do or not do:

1. Provide the base library and all its identifiers.

2. Provide all the identifiers from any additional standard library that it provides.

3. Provide a mode of operation in which the lexical syntax conforms to R7RS-small.

4. Detect and report errors for which "an error is signaled" is used.

5. Return an object and not signal an error for any procedure that returns an undefined value.

6. Support `! $ % & * + - . / : < = > ? @ ^ _ ~` as special characters in identifiers.

7. Implement proper tail calls.

8. Memoize the value of a forced promise.

9. Not force the arguments of procedures that operate uniformly on those arguments.

10. Not change the value of a parameter in any thread other than one which calls `parameterize`.

11. Precede the loading/expansion of a library with the loading/expansion of any libraries it depends on.

12. Import bindings from a single loading only.

13. In the REPL, provide at least the base library without needing to import it.

14. Return `#f` from applying `eq?` when `eqv?` also returns `#f`.

15. Return `#t` from applying `equal?` when `eqv?` also returns `#t`.

16. Not loop infinitely in `equal?`.

17. Return inexact results to mathematical procedures (except `exact`) when any argument is inexact.

18. Implement a coherent subset of the numeric tower. \[Note: Coherent subsets include fixnums and flonums, integers and flonums, real numbers, and the full tower.]

19. Support exact integers throughout the range of permitted indexes.

20. Read integer constants as exact integers if they are within the index range.

21. When returning an inexact result instead of an exact one due to implementation restrictions, return the highest available precision.

22. Support infinities, NaNs, and negative zero, if at all, in accordance with IEEE 754.

23. Use the same numeric rules in `string->number`, `read`, and when reading programs.

24. Support characters corresponding to the ASCII character repertoire.

25.  Support the character names `#\alarm #\backspace #\delete #\escape #\newline #\null #\return #\space #\tab`.

26. Implement the character class predicates in a Unicode-compatible way.

27. Not forbid ASCII characters (other than #\null) in strings.

28. Support the trichotomy law for string comparisons.

29. Support at least the argument 5 to `scheme-report-environment`.

30. Not close ports automatically unless they are garbage.

31. Ensure that characters seen by `char-ready?` cannot be removed from the input before reading them.

32. In `write`, use datum labels to represent circular structure; in `write-shared`, use datum labels to represent both circular and shared structure.

33. Not loop forever in `display`.

34. Accept source files in `load`.

35. In `exit` and `emergency-exit`, not signal an error or return to the continuation.

36. Permit `\x` escapes in identifiers surrounded by vertical bars.

37. Not provide a standard feature identifier if the corresponding feature is not provided.

38. Be hygienic in derived forms and macros.

39. Implement comparison procedures transitively.

40. Support both textual and binary ports.

41. Make standard object types (including eof objects) disjoint.

42. In `map` and `for-each`, terminate on the shortest argument list.

These are some of the things that an R7RS-small implementation should do (that is, if they do not, a rationale must be given):

1. Signal an error as soon as `syntax-error` is expanded.

2. Document the mapping between library names and the file system, if any.

3. Permit redefining identifiers in the REPL.

4. In mathematical procedures (other than `inexact`), produce exact results when given exact arguments.

5. Return an inexact result rather than reporting an implementation restriction if exact rationals are not provided.

6. Match or exceed the precision provided by IEEE 754.

7. Make it very likely that `current-jiffy` returns a compactly implemented integer.
