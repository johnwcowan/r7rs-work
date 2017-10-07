= WG1 Ballot Items To Finalize By Oct. 29 =

Ballot 8 was conducted informally on [[https://groups.google.com/forum/?fromgroups=#!topic/scheme-reports-wg1/-Z65vUeerPk|a poll thread]] of the WG1 mailing list.

=== #315 null character may not be usable in strings ===

The original proposal was to make `(string-set! str n #\null)` unspecified, which was rejected, at least partly on the ground that R7RS implementations can already forbid specified characters from appearing in strings.  However, the ASCII character set (which includes `#\null`) could not be forbidden, per UnicodeCowan.  Consequently, the rationale given by several voters that there was no need to make a special case for `#\null` was incorrect.  The WG therefore chose from the following options:

`forbid-nothing`: every character supported by the implementation must be allowed to appear in strings.

`forbid-any-but-128`: every ASCII character ''including'' `#\null` must be allowed in strings.  (This is the default, corresponding to the `no` vote on Ballot 5.)

`forbid-any-but-127`: every ASCII character ''excluding'' `#\null` must be allowed in strings.

`forbid-any-but-97`: every printable ASCII character plus `#\space`, `#\tab`, and `#\newline` must be allowed in strings.

`open`: any character may be forbidden in strings.

`reader`: the characters required by the reader, plus `[A-Za-z]`, must be allowed in strings.

`unspecified`: say nothing.


  * '''Options:''' forbid-nothing, forbid-any-but-128, forbid-any-but-127, forbid-any-but-96, open, reader, unspecified, undecided
  * '''Default:''' forbid-any-but-128
