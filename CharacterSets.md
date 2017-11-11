R5RS specifies only the characters necessary to handle parsing Scheme
source code itself, leaving almost everything else unspecified.  The
character ordering only guarantees that the characters a..z, A..Z and
0..9 are in order, and the actual integer values are left unspecified
(although in practice almost all implementations use ASCII).

Unicode is fast becoming the most widely used character set, and full
Unicode compliance was specified in the R6RS.  However, Unicode is
expensive to support on small and embedded systems, is slower and more
space intensive than most native character sets, and there are
competing unified character sets which should not be dismissed easily.
Given this, while one would expect a general-purpose featureful Scheme
implementation to support Unicode, it is not immediately clear that we
should _require_ Unicode support unconditionally in the WG1 standard.

Given this, we need to determine what and how much we specify about
characters and strings, including their integer values, orderings and
case mappings.

Current Proposals:

* [UnicodeCowan](UnicodeCowan.md)
