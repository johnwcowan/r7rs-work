## Implementation Contrasts

This is an index to pages that describe various detailed differences between Scheme implementations.

As of 2016-08-05, the list of Scheme implementations tested is:

[Racket](http://racket-lang.org/), [Gauche](http://practical-scheme.net/gauche/), [MIT](http://www.gnu.org/software/mit-scheme/), [Gambit](http://dynamo.iro.umontreal.ca/wiki/index.php/Main_Page), plain [Chicken](http://call-cc.org/), Chicken with the [numbers egg](http://wiki.call-cc.org/eggref/4/numbers), [Cyclone](https://github.com/justinethier/cyclone), [Bigloo](http://www-sop.inria.fr/members/Manuel.Serrano/bigloo/), [Scheme48](http://s48.org/), [scsh](http://www.scsh.net/), [Guile](http://www.gnu.org/software/guile/), [Kawa](http://www.gnu.org/software/kawa/), [Sagittarius](https://code.google.com/p/sagittarius-scheme), [SISC](http://sisc-scheme.org/), [Chibi](https://code.google.com/p/chibi-scheme/), [Picrin](https://github.com/picrin-scheme/picrin), [Foment](https://code.google.com/p/foment/), [SCM](http://people.csail.mit.edu/jaffer/SCM.html), [Chez](http://scheme.com/), [Vicare](http://marcomaggi.github.io/vicare.html), [Larceny](http://www.larcenists.org/), [Ypsilon](https://code.google.com/p/ypsilon/), [Mosh](https://code.google.com/p/mosh-scheme/), [IronScheme](https://github.com/leppie/IronScheme), [NexJ](http://nexj-scheme.org/), [JScheme](http://jscheme.sourceforge.net/jscheme/main.html), [STklos](http://www.stklos.net/), [KSi](http://ksi.sourceforge.net/), [SigScheme](https://code.google.com/p/sigscheme/), [Shoe](http://www.nocrew.org/software-shoe.html), [TinyScheme](http://tinyscheme.sourceforge.net/), [Mini-Scheme](https://github.com/catseye/minischeme), [Scheme 9](http://www.t3x.org/s9fes/), [RScheme](http://www.rscheme.org/rs/index.html), [S7](https://ccrma.stanford.edu/software/snd/snd/s7.html), [Unlikely Scheme](https://marijnhaverbeke.nl/unlikely/), [SIOD](http://people.delphiforums.com/gjc/siod.html), [BDC](http://carlstrom.com/bdc-scheme/), [XLisp](http://www.xlisp.org/), [Rep](http://librep.sourceforge.net/), [Schemik](http://schemik.sourceforge.net/), [Elk](http://sam.zoy.org/elk/), [UMB](http://www.cs.umb.edu/~wrc/scheme/), [Llava](http://llava.org/), [SXM](http://www.malgil.com/sxm/), [Sizzle](http://www.grabmueller.de/martin/www/sizzle/sizzle.en.html), [FemtoLisp](https://github.com/JeffBezanson/femtolisp), [LMU Scheme](http://www.mathematik.uni-muenchen.de/~forster/sw/lmuscheme.html), [Dfsch](http://hakl.net/software/dfsch.en.html), [Inlab](http://www.inlab.de/scheme/), [Oaklisp](http://www.bcl.hamilton.ie/~barak/oaklisp), [Owl Lisp](https://code.google.com/p/owl-lisp/).

My test system now runs 64-bit Linux, but most of the pages below report the results from using 32-bit Linux.  Most of my Schemes (except Racket and JVM-based and CLR-based ones) are built from source, usually the latest release but in some cases the head of source control.  However, not all versions are up to date, and the versions tested have changed over the past few years, so some of the claims in the pages below are surely obsolete.  Implementers and users are urged to [notify me](mailto:cowan@ccil.org) when they see something incorrect.

As of 2016-04-01, Oaklisp is not building on my 64-bit Linux system again.

As of 2014-11-29, I successfully built 32-bit Oaklisp on 64-bit Linux, and restored it to the list above.

As of 2013-07-29, I could not build Oaklisp (it doesn't seem to like 64-bit systems) or Dream (link dead), so they were excluded from the suite from then on.

As of 2013-01-31, I could not build [VX](https://code.google.com/p/vx-scheme/) or [Spark](https://github.com/vijaymathew/spark-scheme), so they were excluded from the suite from then on.

KSM, Luna, MScheme, QScheme, Rhizome/pi, and VSCM either will not build on my system or will not run.  For these and many other Schemes see the [fairly complete list of Scheme implementations](http://community.schemewiki.org/?scheme-faq-standards).

S7 was formerly, but incorrectly, known in these pages as "Scheme 7", and some references to this name survive.  It's really named after [the motorcycle](http://en.wikipedia.org/wiki/Sunbeam_S7_and_S8).

Not all these pages have information on all these Schemes.

* [ApplyArgsLimit](ApplyArgsLimit.md)
* [ArgumentOrder](ArgumentOrder.md)
* [BackslashBar](BackslashBar.md)
* [BracketsBraces](BracketsBraces.md)
* [CallCc](CallCc.md)
* [CaseInsensitivity](CaseInsensitivity.md)
* [CharEq](CharEq.md)
* [CommaInIdentifiers](CommaInIdentifiers.md)
* [CompilerAvailable](CompilerAvailable.md)
* [ComplexLog](ComplexLog.md)
* [ComplexRepresentations](ComplexRepresentations.md)
* [CondExpand](CondExpand.md)
* [CwifClosePort](CwifClosePort.md)
* [DatumLabels](DatumLabels.md)
* [DefineSyntaxDefines](DefineSyntaxDefines.md)
* [DisjointPromises](DisjointPromises.md)
* [DotComma](DotComma.md)
* [EmbeddedCrLf](EmbeddedCrLf.md)
* [EmptyDefine](EmptyDefine.md)
* [EmptyList](EmptyList.md)
* [EmptyStringsVectors](EmptyStringsVectors.md)
* [EvalDefine](EvalDefine.md)
* [ExactExpt](ExactExpt.md)
* [ExactSqrt](ExactSqrt.md)
* [FixnumInfo](FixnumInfo.md)
* [FloatPrecision](FloatPrecision.md)
* [ForceNonPromise](ForceNonPromise.md)
* [GetFromClosedStringPort](GetFromClosedStringPort.md)
* [GuardBehavior](GuardBehavior.md)
* [HashBangEof](HashBangEof.md)
* [HashInIdentifiers](HashInIdentifiers.md)
* [HashQuote](HashQuote.md)
* [HygienicInclusion](HygienicInclusion.md)
* [ImmediateStringsCoalesced](ImmediateStringsCoalesced.md)
* [ImmutableStrings](ImmutableStrings.md)
* [ImproperLists](ImproperLists.md)
* [KeywordSyntax](KeywordSyntax.md)
* [LetrecStar](LetrecStar.md)
* [MaxInfNan](MaxInfNan.md)
* [MultipleValues](MultipleValues.md)
* [NegativeRationalize](NegativeRationalize.md)
* [NegativeSqrt](NegativeSqrt.md)
* [NilIsFalse](NilIsFalse.md)
* [NonFiniteNumbers](NonFiniteNumbers.md)
* [NumericTower](NumericTower.md)
* [OneArmedIf](OneArmedIf.md)
* [OnePlusEx](OnePlusEx.md)
* [PetrofskyCatastrophe](PetrofskyCatastrophe.md)
* [PetrofskyLet](PetrofskyLet.md)
* [PrefixedStringToNumber](PrefixedStringToNumber.md)
* [PropertyLists](PropertyLists.md)
* [QuoteDelimiter](QuoteDelimiter.md)
* [ReadLine](ReadLine.md)
* [ReadMutable](ReadMutable.md)
* [Readtables](Readtables.md)
* [RedefiningKeywords](RedefiningKeywords.md)
* [RedefiningSyntax](RedefiningSyntax.md)
* [RedundantImports](RedundantImports.md)
* [RoundInf](RoundInf.md)
* [SelfQuotingVectors](SelfQuotingVectors.md)
* [SetUndefinedVariable](SetUndefinedVariable.md)
* [SchemeOnWindows](SchemeOnWindows.md)
* [ShortEquality](ShortEquality.md)
* [SquareBrackets](SquareBrackets.md)
* [StandaloneExecutables](StandaloneExecutables.md)
* [StringSymbolConversion](StringSymbolConversion.md)
* [StringTitlecase](StringTitlecase.md)
* [SyntaxAbuse](SyntaxAbuse.md)
* [SyntaxDefinitions](SyntaxDefinitions.md)
* [UnicodeSupport](UnicodeSupport.md)
* [UnsignedImaginary](UnsignedImaginary.md)
* [UpperCaseEscape](UpperCaseEscape.md)
* [VerticalLineSymbols](VerticalLineSymbols.md)
* [WhatLoadReturns](WhatLoadReturns.md)
* [Zero](Zero.md)

> [More implementation contrasts](http://web.mit.edu/~axch/www/scheme/choices.html).
