Extensions to the Guile/Chicken [getopt-long](http://wiki.call-cc.org/eggref/5/getopt-long)
library:

 * New `multiple?` property:  if `#f` or missing, allows options to appear only once;
   if `#t`, allows options to appear multiple times and passes a ''list'' of values
   in order of appearance to the `predicate` and `transformer` procedures.
   
 * New `acceptor` property (better name, perhaps?), a procedure which is invoked
   with the option and value after transformation (so you can store
   the value somewhere).
   In principle this could be done by the transformer procedure instead,
   but it seems cleaner to use a separate procedure.
   
 * Accept the form `--option value` as well as `--option=value`.

 * Allow specifying default option values.

 * Accept a string as the input, which is broken up using spaces;
   double quotes around an option value allow spaces within it.
   Issue: figure out how to supply the command name for `usage`.
   
 * Make a less opaque way of returning unprocessed arguments,
   such as returning multiple values.
   
 * Nail down conditions for specifying what's wrong:
   incorrect grammar, unknown option, bad value,
   missing option, etc.
   
 * A fresh implemntation under the MIT license is needed, mwhich can be layered over
 [SRFI 37](https://github.com/scheme-requests-for-implementation/srfi-37/srfi-37.html).
