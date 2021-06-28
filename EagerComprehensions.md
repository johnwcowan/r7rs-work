# Eager Comprehensions

Eager comprehensions are a loop facility for Scheme based on list comprehensions.  Unlike Haskell's, and like Python's, they execute eagerly, in keeping with Scheme's general eager evaluation.

The details are at [SRFI 42](https://srfi.schemers.org/srfi-42/srfi-42.html).  This is just an epitome except for the comprehensions and generators derived from the [Shivers loop](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.128.5560&rep=rep1&type=pdf) (PDF) and [foof-loop](http://mumble.net/~campbell/scheme/foof-loop.txt) loop macros, marked by subdivisions below.

## Comprehensions

These macros assemble streams of results from generators invoked inside them to produce a single (composite or summary) result.  There may be multiple generators, in which case they are understood to be nested and are run rightmost-fastest.

`do-ec` evaluates a command once for its side-effects; all other comprehensions are based on this one.

`list-ec` evaluates an expression repeatedly, returning a list of results.

`append-ec` evaluates an expression (which must return a list) repeatedly, appending the results together.

`string-ec` evaluates an expression (which must return a character) repeatedly, returning a string of the results.

`vector-ec` evaluates an expression repeatedly, returning a vector of results.

`vector-of-length-ec` is the same as `vector-ec`, but is given the length of the final vector for efficiency.

`sum-ec` evaluates an expression (which must return a number) repeatedly, returning the sum of the results.

`product-ec` evaluates an expression (which must return a number) repeatedly, returning the product of the results.

`max-ec` evaluates an expression (which must return a number) repeatedly, returning the `max` of the results.

`min-ec` evaluates an expression (which must return a number) repeatedly, returning the `min` of the results.

`any?-ec` evaluates an expression repeatedly as a boolean.  Any true result aborts the loop and returns `#t`; otherwise, `#f` is returned.

`every?-ec` evaluates an expression repeatedly as a boolean.  Any false result aborts the loop and returns `#f`; otherwise, `#t` is returned.

`first-ec` returns the first value of the expression, aborting the loop.

`last-ec` returns the last value of the expression.

`fold-ec` evaluates an expression repeatedly and reduces the resulting sequence using a specified initial value and binary reduction procedure.

`fold-ec3` does what `fold-ec` does, but the value is used only if the sequence is empty.  Otherwise, a specified unary procedure is applied to the first member of the sequence, and further reduction is done using the binary procedure.

### Suggested comprehensions

The following comprehensions are suggested by SRFI 42 but are not part of it.

`array-ec` evaluates an expression repeatedly and uses the stream of values to construct an array of a specified shape in row-major order.

`bitwise-ec` evaluates an expression as a boolean repeatedly, mapping true values to 1 bits and false values to 0 bits, and returning an integer based on those bits taken in little-endian order.

`stream-ec` lazily generates a [SRFI 40](https://srfi.schemers.org/srfi-40/srfi-40.html) stream, running generators only as needed.

The SRFI also mentions the possibility of `begin-ec`, `|-ec`, `|-ec`, and `&&-ec` for [scsh](http://www.scsh.net/).

### Not-recommended comprehensions

The following comprehensions are mentioned by the SRFI but are considered too specialized:

`gcd-ec` evaluates an expression (which must return a number) repeatedly, returning the greatest common divisor of the results.

`lcm-ec` evaluates an expression (which must return a number) repeatedly, returning the least common multiple of the results.

### Proposed comprehensions from foof-loop

`reverse-ec` is like `list-ec` but produces its values in reverse (potentially more efficiently).

`append-reverse-ec` is like `append-ec` but appends the reverse of each of its values (potentially more efficiently).

## Generators

These macros are used within comprehensions to specify how streams of values are generated.  Each generator binds a variable to the value currently being generated; it may also bind another variable to an index number, an exact integer counting the number of generated values so far.  These variables are visible in expressions of generators to the right.

`:` examines its arguments to determine whether to use the `:list`, `:string`, `:vector`, `:range`, `:real-range`, `:char-range`, or `:port` generator; it is extensible.

`:list` generates results from one or more lists.

`:string` generates character results from one or more strings.

`:vector` generates results from one or more vectors.

`:integers` generates the non-negative exact integers starting from 0.

`:range` generates a finite range of exact integers, with specified low (inclusive), high (exclusive) and step values.

`:real-range` is like `:range`, but handles arbitrary real numbers.  Roundoff errors are avoided.

`:char-range` is like `:range`, but generates characters in Unicode order, with a specified start (inclusive) and stop (exclusive); the stride is always 1.

`:port` generates results by reading a specified port with a specified reader procedure until an eof-object is returned.

`:do` defines a generator using a named-let with optional inner and outer lets; all other generators are based on this one.

`:let` defines a generator that binds a variable to a single value and generates that value.

`:parallel` runs several sub-generators in parallel, terminating when any of its sub-generators do.

`:while` evaluates a guard expression repeatedly and runs a sub-generator while the guard expression is true.

`:until` evaluates a guard expression repeatedly and runs a sub-generator until the guard expression is true.

### Suggested generators

The following generators are suggested by SRFI 42 but are not part of it:

`:array` generates results from one or more arrays in row-major order.

`:random-integer` generates a specified number of random integers in a specified range.

`:random-real` generates a specified number of random real numbers between 0 (inclusive) and 1 (exclusive).

`:bitwise` generates a sequence of bits obtained by appending the binary digits of specified integers.

`:stream` is like `:list`, but generates results from one or more [SRFI 40](https://srfi.schemers.org/srfi-40/srfi-40.html) streams.

`:lines-of-file` is like `:port`, but the reader is `read-line` and the optional index variable indexes lines.

`:chars-of-file` is also like `:port`, but the reader is `read-char` and there are two optional index variables, one for lines and one for columns within a line.

`:directory` generates a sequence of filenames from a directory.

`:match` generates a sequence of strings matching a regular expression or glob.

`:env` generates a sequence of environment variables.

### Proposed generators from foof-loop

`:substring` iterates over the characters of part of a string specified by low (inclusive) and high (exclusive) bounds.

`:subvector` iterates over the elements of part of a vector specified by low (inclusive) and high (exclusive) bounds.

`:reverse-range` generates a sequence of exact integers in decreasing order, with specified low (inclusive), high (exclusive), and step values.

`:reverse-real-range` is like `:reverse-range`, but handles arbitrary real numbers.  Roundoff errors are avoided.

`:reverse-substring` is like `:substring`, but generates characters in reverse order.

`:reverse-subvector` is like `:subvector`, but generates elements in reverse order.

`:list-pairs` is like `:list`, but iterates over the pairs rather than the elements of lists.

`:let-values` is like `:let`, but captures the multiple values of its expression in multiple variables.

### Proposed generators from Shivers loop

`:previous` binds *n* variables to the last *n* values of the loop.

## Qualifiers

These macros can be intermixed with generators to control the behavior of generators to the right of them.

`if` examines an expression and runs generators to its right only if the expression is true.

`not`, `and`, and `or` are the compositions of `if` that you'd expect.

`begin` takes a sequence of expressions, all but the last of which are evaluated for effect.

`nested` groups qualifiers and generators into a single parenthesized form, without any special semantic effect.

## Extensibility

The basic approach to extension is to write new comprehensions and generators as syntax-rules macros which invoke an explicit continuation argument.  There are some macros and procedures that assist with this:

`:dispatched` is a generator that invokes a specified dispatch procedure over a list of arbitrary setup arguments.  The dispatch procedure returns either a generator procedure which when invoked repeatedly produces a stream of values, or `#f` meaning that it does not know how to handle the arguments.  This is used to implement the `:` generator.

`:generator-proc` is a macro that wraps a generator, returning a dispatch procedure for it. It makes writing procedures for `:dispatched` easier.

`:-dispatch-ref` is a procedure that returns the current global dispatch procedure for the `:` generator.

`:-dispatch-set!` is a procedure that changes the global dispatch procedure for the `:` generator.

`make-initial-:-dispatch` is a procedure that returns the initial dispatch procedure for the `:` generator.

`dispatch-union` is a procedure that returns a dispatcher providing the union of two dispatchers.  An error is signalled if arguments are passed that both dispatchers are able to handle.

