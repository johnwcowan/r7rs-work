MIT Scheme now has support for positive and negative infinities and for signaling (the default) and non-signaling !NaNs.  I'm documenting this here because the MIT Scheme manual doesn't yet describe these features, and having them here might be useful if we discuss this stuff in WG2.

The printed representations are `#[+inf]`, `#[-inf]`, and `#[NaN]`, and are not `read`able.

To disable signaling: `(flo:ignoring-exception-traps `''thunk''`)`.  For example:
{{{
  (flo:ignoring-exception-traps
    (lambda ()
      (let* ((i (* 1.0e200 1.0e200)) (n (- i i))) (list i (- i) n))))

  ;Value 13: (#[+inf] #[-inf] #[NaN])
}}}

Different !NaN values are not `eqv?`.

Complex numbers can have one exact part and one inexact part.

These procedures are available for manipulating the floating-point environment:
{{{
  flo:nan?
  flo:raise-exceptions!
  flo:restore-exception-flags!
  flo:rounding-mode
  flo:rounding-modes
  flo:save-exception-flags
  flo:set-environment!
  flo:set-rounding-mode!
  flo:set-trapped-exceptions!
  flo:significand-digits-base-2
  flo:significand-digits-base-10
  flo:supported-exceptions
  flo:test-exception-flags
  flo:test-exceptions
  flo:trap-exceptions!
  flo:trappable-exceptions
  flo:trapped-exceptions
  flo:untrap-exceptions!
  flo:update-environment!
  flo:with-default-environment
  flo:with-exceptions-trapped
  flo:with-exceptions-untrapped
  flo:with-rounding-mode
  flo:with-trapped-exceptions
}}}