= Aaron W. Hsu's View on Case Sensitivity in Working Groups 1 & 2 =

== Problems ==

  * Scheme implementations always take their own approach to case-folding in that they choose one or the other, and don't care about the standard
  * Case behavior is excessively religious and emotional
  * We have two precedences, one R6RS and one R5RS, in standards for doing different case behavior
  * In the presence of different character sets and traditions, certain case behaviors are more prevalent
  * The solution to the behavior must be compatible both in Scheme Core and Scheme 7

== Proposed Solution ==

  * (Scheme Core) Case behavior by default is implementation dependent
  * (Scheme Core) Implementations must implement both case folding and case preserving modes
  * (Scheme Core) Implementations must recognize the flags `#!fold-case` and `#!no-fold-case`
  * (Scheme Core) Support the `||` case-preserving reader syntax, but do not have them delimit atoms
  * (Scheme Core) Implementations must recognize the parameter `case-sensitive` values `#t` and `#f`
  * (Scheme Core) The module system should provide some means for controlling case sensitivity at a suitable level of granularity
  * If the character set used by a Scheme implementation is Unicode, then the official Unicode case folding algorithm should be used

== Clarifications ==

The use of parameters in Scheme Core would affect any procedures called that make use of the reader. Specifically `read` would be sensitive to such parameters. The term paramter here does not imply that an official standard of parameter need be adopted for this proposal to be useful. Rather it means that the identifiers deemed to be "parameters" are expected to behave like parameters in their public interface; they may or may not be implemented as real parameters in practice. 

== Rationale ==

I would like to avoid creation of "compatibility" layers in Scheme implementations that intend to break the default case sensitivity defaults. Since most code that needs to run or most legacy code that is important can be counted on to still run under their same implementations in this proposal, this actually improves the conformance and maintains a large body of backwards compatible source code without requiring modification. Such source code can easily be moved to systems with different defaults by using flags or modules. This is no more difficult than requiring special encantations in the target Scheme implementation. It is also standardized, so the user can easily make a library entirely portable in this regard by either using a flag or providing a module. Doing so is superior to requiring each implementation to have a unique encantation for calling the code with the correct behavior. 

The use of a parameter allows for much nicer interactions with the reader at a Scheme Core level without complicating the semantics. It also means that you have more control at run-time of the reader without making this more complex. It permits an acceptable level of control in Scheme Core, while scaling up to phasing levels and procedural macros in Scheme 7.

== Case-sensitive and Case-Folded Modules (by John Cowan) ==

What happens when a case-preserving module imports names from a case-folded module?  Since Unicode case folding is downcasing, the obvious answer is that all such names are implicitly exported in lower case.  Thus a case-preserving module must refer to case-folded names in lower case only, and an attempt to reference them in upper case will fail.

The reverse situation is more difficult.  A case-preserving module may export `foo`, `Foo`, and `FOO` as three separate names; how can they be written in a case-folding module that imports them?  The answer is that case-folding affects only reading and printing; small Scheme identifiers may contain both lowercase and uppercase letters, thanks to the presence of escaping mechanisms in identifier syntax.  Consequently, the case-folding module can refer to these identifiers as `foo` (or `FOO`), `|Foo|`, and `|FOO|` respectively.

The comparative straightforwardness of this style is the justification for adding the CL-ish `|...|` syntax (which is supported in at least PLT, Gauche, Gambit, Chicken, Bigloo, Kawa, SISC, and Chez already) to the R6RS Unicode escape syntax of `\x<hexdigits>;`.  Using only Unicode escapes in a case-folding system, `|Foo|` would be spelled `\x46;oo` or `\x46;OO`, and `|FOO|` would be spelled `\x46;\x4F;\x4F;`, all of which are hideous and unreadable. 

== Issues ==

Should `case-sensitive` interact with the flags? That is, if a reader or load call encounters a flag, should the parameter be altered? Should the parameter's value be altered only in the call or should it persist?

  At the moment I am inclined to think that it is okay for the two to interact, and probably a good thing, provided that the extent of that interaction remains limited by the call. That is, the value of the parameter should be reset to the incoming value on return. This is because the scope of that flag should only extend to the end of the file, and should not have an effect on continuing computation in the environment that made the call. (Aaron W. Hsu)

== Endorsements ==

John Cowan endorses this page, with the following exceptions:

 * The Unicode folding algorithm should be used whether the Scheme is Unicode-based or not, as it's trivial to subset.  It maps about 1000 individual characters to other characters and about 100 more characters to two-character sequences (such as ß to ss).  In situations where this is excessively expensive, implementations MAY limit the characters permitted in a symbol/identifier, as discussed in UnicodeCowan.

  * Per PortsCowan, I now think that the proper home of symbol/identifier case sensitivity is a port rather than a global parameter.
