== Randomness in the style of Common Lisp ==

This proposal for random numbers is not a literal transcription of the Common Lisp (CL) interface, but it uses the same concepts and provides the same facilities.  The text is a heavily edited version of the CL Hyperspec.

A ''random-source object'' is an encapsulation of the state information used by an implementation-dependent pseudo-random number generator.  The state can be printed out and successfully read back in by the same implementation, but might not function correctly as a state in another implementation.  Random-source objects are a disjoint type.  This facility should not be used where strong randomness is required.

== Procedures ==

`(make-random-source . `''args''`)`

Constructs and returns a random-source object that has been randomly initialized by some implementation-defined means.  The CL equivalent is `(make-random-state t)`.  If ''args'' are provided, they are used in an implementation-defined way to affect what result is returned.

`(random-source? `''obj''`)`

Returns `#t` if ''obj'' is a random-source object, and `#f` otherwise.  The CL equivalent is `random-state-p`.

`(random-source-state `''random-source''`)`

Returns an implementation-specific object representing a copy of the state encapsulated by ''random-source''.  This object MUST be printable and rereadable using standard Scheme lexical syntax.  It MUST also be suitable for passing to `make-random-source-from-state`.  Providing this mechanism makes it possible to save and reconstitute a random-source in a file or database, or to pass it across a network to an equivalent implementation.  Mutating the result of this procedure does not affect ''random-source''.  There is no CL equivalent of this procedure, because CL `random-state` objects are themselves required to be printable and rereadable (they are typically CL `structs`).

`(make-random-source-from-state `''state''`)`

Constructs and returns a random-source object whose state is a copy of ''state'', so that mutating ''state'' does not affect the random-source object.  The result will generate the same sequence of pseudo-random numbers that the original random-source object would have generated as of the time `random-source-state` was invoked on it.  It is an error to pass a ''state'' object that has been mutated.  There is no CL equivalent of this procedure.

`(copy-random-source `''random-source''`)`

Constructs and returns a random-source object whose state is an independent copy of the state of ''random-source''.  Calling this procedure is equivalent to calling `(make-random-source-from-state (random-source-state `''random-source''`))`, but potentially more efficient because it can avoid copying the state twice.  The result and ''random-source'' will henceforth return the same sequence of values, allowing the same series of pseudo-random numbers to be generated many times within a single program.   The CL equivalent is `make-random-state` with a random-state argument.

`(current-random-source`)`

A parameter that returns the default random-source object used by `random`.  Its initial value MUST be a random-source object, but is implementation-dependent.  The CL equivalent is `*random-state*`.

Note:  The equivalent of CL `(make-random-state)` and `(make-random-state nil)` is `(copy-random-source (current-random-source))`.

`(random `''limit'' [''random-source'']`)`

Returns the next pseudo-random number from ''random-source''.  The result is a non-negative number less than ''limit''.  ''Limit'' MUST be either an exact integer (in which case `random` returns an exact integer), or an inexact real number (in which case `random` returns an inexact real number).  If ''random-source'' is not specified, the value of `(current-random-source)` is used.  The CL equivalent of this procedure is `random`.

An approximately uniform choice distribution is used. If ''limit'' is an integer, each of the possible results occurs with (approximate) probability 1/''limit''.

== Examples ==

{{{
 (<= 0 (random 1000) 1000) =>  true
 (let ((state1 (copy-random-source (current-random-source)))
       (state2 (copy-random-source (current-random-source))))
   (= (random 1000 state1) (random 1000 state2))) =>  true
}}}

== Common Lisp Hyperspec links ==

 * [[http://www.lispworks.com/documentation/HyperSpec/Body/t_rnd_st.htm|System Class RANDOM-STATE]]
 * [[http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_rnd.htm|Function MAKE-RANDOM-STATE]]
 * [[http://www.lispworks.com/documentation/HyperSpec/Body/f_random.htm|Function RANDOM]]
 * [[http://www.lispworks.com/documentation/HyperSpec/Body/f_rnd_st.htm|Function RANDOM-STATE-P]]
 * [[http://www.lispworks.com/documentation/HyperSpec/Body/v_rnd_st.htm|Variable *RANDOM-STATE*]]

