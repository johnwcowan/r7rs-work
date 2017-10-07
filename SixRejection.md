These are condensed and anonymized [[http://www.r6rs.org/ratification/results.html#X67|objections to R6RS]] by the people who voted against it, with comments underneath each that show how R7RS meets (or fails to meet) that objection.  Statements about R7RS-large reflect WG votes, but are subject to change.  '''This document is still being edited.'''  All errors and misstatements not present in the original comments are Cowan's responsibility.

The order of the objections given here follows the order of the R7RS-small report as much as possible, which follows the R5RS report as much as possible.

This document is explicitly a work of R7RS advocacy.  Its purpose is not to attack R6RS, but to explain why people who rejected R6RS at the time of ratification may find R7RS more to their liking (and should therefore vote for it).  

== General ==

 * R6RS simultaneously breaks with the Scheme tradition to favor homogeneity and software-engineering appeal and still doesn't go far enough towards becoming a useful engineering tool.
    * Dividing R7RS into large and small languages was intended to meet this objection.  The relative sizes of the languages are approximately thus:  R4RS < R5RS < R7RS-small < R6RS < Common Lisp < R7RS-large.

 * Interoperability should not be achieved by demanding that one size fits all.  Requiring all of the many features of R6RS limits the capacity of the implementers to work in small, specialized fields.  Embedded devices immediately come to mind, but other things also come to mind, such as applications that may not desire unicode, libraries, or other pieces.  R6RS removes the aspects of elegant simplicity and limited scope that make Scheme such a great language for teaching. Right now, Scheme is easy to learn, because the core concepts are simple, and they extend outward in reasonable, predictable ways.
    * R6RS is about 340% the size of R5RS.  By contrast, R7RS-small is about 50% larger than R5RS, but only the base library is actually required, and that actually defines fewer names than R5RS.

 * The change to case-sensitivity flies in the face of Scheme tradition, for little to no benefit.  Existing programs that assumed case-insensitivity will have to be massively overhauled.
    * The R7RS `include/ci` library declaration permits case-insensitive code files to be included in libraries and main programs without making any changes to the files themselves.  However, it's true that case-insensitive code can't be just typed or pasted into a REPL.

 * R6RS has simply suffered considerable feature creep and is vastly overspecified and overcomplex. In a word, it is too ambitious (or presumptuous, depending on your point of view). It will make Scheme too much complicated for a gain not worth it.  We should focus on defining vital SRFIs, and that we all agree on portable extensions of Scheme, to implement, say, TCP/IP, non blocking IOs or bindings to a graphical library.
    * R7RS-large will consist of a series of optional packages, several of which will be closely aligned with existing SRFIs.  Not all portable extensions can be implemented portably, however.

 * The standard library report gratuitously eschews important and widely-used existing SRFIs such as the SRFI-1 list library in favor of its equivalent incompatible functionality.
    * R7RS-small has changed R5RS in small and upward-compatible ways for SRFI-1 compatibility.  SRFI-1 as a whole will be part of R7RS-large.

 * Why should features such as, say, byte vectors, enumerations and hash tables should even be included in the RnRS reports in the first place?  The SRFI process has proven itself and works better than the committee process being voted on here.
   * Bytevectors are provided because existing code has used strings in the same function, which is unreliable when characters require more than 8 bits.  The other data structures mentioned will be provided in R7RS-large.

 * R6RS has the goal of making Scheme be like C or Perl: a workable programming language like other programming languages.  This seems reasonable on its face, but Scheme has survived for a long time by not being a programming language like others, but rather by being an oddball: minimally specified, usable for real-world programming only by using numerous implementation-specific features, but ideal for teaching and research.
    * R7RS-small retains the qualities mentioned.

 * It is not clear that R6RS even specifies the right things to make Scheme industrial strength.
    * Probably not, by the standards of Java or Python, which come with huge libraries built in.  The R7RS-large libraries will be mostly (or perhaps entirely) optional, but an implementation providing them all should have plenty of industrial strength.  Attributes of industrial-strength languages other than convenient libraries, such as static typing and pervasive object-orientation, are highly controversial, and really would produce a language that is no longer Scheme.

* Each new large feature (enums, records, conditions, identifier macros, Unicode) is individually motivated, but all together we end up with a very large ball of mud.  The size of this ball directly impairs one of Scheme's important features from an industrial perspective: its role as a small language suitable for automatic program transformation and other PLT sorts of techniques.
    * The design of R7RS should reduce such coupling: no R7RS-small library depends on any other except the base library.  (However, loading makes little sense with neither I/O nor `eval`).  Inter-library dependencies in R7RS-large will be minimized.

 * R6RS gives a fairly low priority to backwards compatibility.  Implementations are evidently expected to provide mechanisms for switching.  There's no incremental update path; each Scheme program must be completely transformed all at once, which effectively means it will never happen.
    * Whether or not this is true (and moving code from R5RS to R6RS systems does not appear to be that difficult), R7RS has given very high priority to backwards compatibility.

 * Libraries and `syntax-case` are entirely new and declarative sub-languages fundamentally different from the core language, which has always been imperative.
    * True.  However, this is also true of `syntax-rules`, which has been around since R4RS (though non-normative at the time).

 * The effort required to implement a standard-compliant Scheme becomes much larger than before.  This will discourage future implementers, endangering the diversity that is one of Scheme's present strengths.
    * Simple R7RS systems are easy to put together from scratch: in principle, they can be smaller than small conformant R5RS systems (they need not have files or the Scheme reader and writer, for instance).  It's also expected to be easy to upgrade R5RS systems to R7RS with minimal (or no) code breakage.

 * R6RS should be ''smaller'' than R5RS, where some of the required features get moved to optional libraries.
    * The R7RS base library is smaller than R5RS.  All other R7RS-small libraries are optional.

 * R6RS mandates too much, trying to standardize insufficient compromise solutions for things that are better left unspecified.  The editors seem to be concerned that there is too much variety in the Scheme world, and propose to eliminate it by making the rulebook thicker.  That is exactly the wrong approach.  Celebrate the variety, and encourage it, so that Scheme implementations can make their own decisions about what is important to them.  R6RS is a misguided attempt to make Scheme more suitable for the mainstream, but effectively does the very opposite by removing the assets that always distinguished Scheme from other dialects: smallness and simplicity.
    * R7RS-small mandates far less; R7RS-large will probably mandate little or nothing.  R7RS-small is not quite as small and simple as R5RS, but focuses on adding a modest amount of function and only tightening up where variety is truly harmful.

 * The Scheme community relies in considerable part on the efforts of individual developers, or small teams of developers, who have to implement these reports -- or who would presumably like to do so if it did not take forever.  I think the Scheme community would be better served by a much smaller language, whose features included the minimum necessary so that the language could easily be extended by loading Scheme source code itself (and perhaps compiling it).
    * The smaller base library makes this possible to some extent.  However, Scheme standards have never been about the "minimum necessary", or procedures like `length` and `list` would never have been provided.  What is more, facilities like sockets can't be layered over a "minimum necessary" core unless that core contains an FFI, which is a large and complex feature.  Sometimes the Facade pattern is the Right Thing.

== Lexical syntax ==

 * A petty irritant for me is the official sanction of square brackets: they are ugly and unhelpful in reading code. They create a very discordant visual rhythm and should be summarily outlawed. Square brackets are a gross violation of the LISP Party Ethic and offending implementers shall be required to submit thorough self-criticism of their motivations and then will be summarily shot. I really mean this.
    * Square brackets are not defined in R7RS lexical syntax.

 * There is no convincing evidence of the value of supporting Unicode for program text. That said, the capability of S-expression `read` makes it hard to avoid. The addition of Unicode escapes is ugly.
    * There is anecdotal evidence that Java gained early acceptance in Japan because it was the first widely used programming language that made it possible to write entire programs (modulo reserved words) in Japanese.  In Scheme, where there are no reserved words, it should be straightforward to write entire programs in ''any'' natural language with the addition of localized libraries that bridge between local and standard identifiers.

== Primitive expression types ==

* Why would one aim to nail down so many boundary conditions without also specifying order of argument evaluation?
   * Scheme has always provided compilers the freedom to evaluate procedure arguments in any order desired.

== Derived expression types ==

 * The `when` and `unless` forms should not be specified.
    * R7RS has added them to the base library.  Many people find them convenient in imperative programs.  However, the return value is now always unspecified, as it cannot be relied on in any case.

== Macros ==

 * Scheme can live with the R5RS hygienic macros for a while still.
    * R7RS-small does live with them.

 * Identifier syntax is purely syntactic sugar that complicate the semantics of the language.  Identifiers may no longer be simple variable references, making code potentially more difficult to read.  This breaks R5RS compatibility by eliminating a specific invariant on which R5RS programs and macros can rely, namely that a reference to an identifier has no side effects.  It weakens every macro in the entire language by removing knowledge about the language the macros are expanding.  Code-walking macros become more difficult to write.
    * Neither R7RS-small nor R7RS-large provides identifier syntax.

 * The `syntax-case` macro system is an unfortunate choice.  Both syntactic closures and explicit renaming macros are older, and are simpler and more in the spirit of Scheme.  The description of the mark-antimark for the `syntax-case` algorithm significantly lengthens and increases the complexity of this chapter.
    * R7RS-small provides only `syntax-rules`.  R7RS-large provides for explicit renaming; additional macro systems may be added.

 * The R6RS treatment of `_` and `...` patterns.  It's unfortunate that R5RS macros can't match `...` as a literal in templates, but with the same restriction on `_` many existing macros will break, and moreover they have no easy workaround, requiring otherwise simple pattern matching macros to be rewritten in much longer and more convoluted low-level code.
    * R7RS-small adopts these conventions of R5RS, but also adopts SRFI 46.

 * There is no way of importing `set!` for a single level.  The composite library should not export `define-syntax` and `let[rec]-syntax for level 1`, or syntax-rules for level 0.  In the context of the `(rnrs)` import, there is nothing one can do with these bindings at those levels.
    * R7RS-small does not provide phasing, as its only macro system is `syntax-rules`.  The question is still open for R7RS-large.  Note that identifiers must have the same definition in every phase.

 * R6RS links library instantiation and visitation not to import clauses, but instead to the presence of identifier references in the client, and provides no guarantee of instantiation otherwise.
    * R7RS requires whatever libraries are imported to be instantiated at least once, but provides no guarantees for or against multiple instantiation.

 * The `syntax-case` system implements a term-rewriting system, with limited programmability, to solve a problem for which there are significantly simpler solutions.  This sub-language introduces a new kind of variable that behaves differently from the variables we're used to, and pushes pattern-matching and templating to the center of the macro-writing process.
    * R7RS-small does not have `syntax-case`.  It is uncertain whether R7RS-large will have it.  This complaint is also true of `syntax-rules`, however, which has been available since R4RS.

== Programs ==

 * R6RS utterly neglects the notions of "top-level program" and "top-level environment", which are absolutely fundamental to Scheme as a language and a system.
    * Both top-level programs and REPLs are part of R7RS, though there is no requirement to provide a REPL.

 * R6RS moves away from the ability to be used interactively through a REPL by removing `load` and offering only a static linking model for programs.  It does not explain how this affects program development and debugging using a REPL, which is the bread-and-butter of a large number of Scheme users.
    * REPLs are rather complex beasts, and probably won't be standardized.  However, R7RS does provide some minimal REPL semantics.  In particular, a variable that appears within a lambda-expression may have its value changed with retroactive effect, but changing a syntax keyword is not retroactive.

 * The draft recognises libraries, a top-level and scripts, three variants of what is basically the same thing (see Queinnec & Padget: ''Modules, Macros and Lisp'') - not the Scheme way.
    * Minimalism is not always the Scheme way, or we'd have only lists, and only standardize `car, cdr, set-car!, set-cdr!, pair?, cons`.

== Records ==

 * The record system is far too complicated.  The constructor-related parts of the syntactic record layer and of the procedural layer are bewilderingly complex.  The custom constructor design is a good example of premature generalization and piling feature upon feature, and their current API adversely affects other aspects of the design, such as modularity.The problem of two record systems that don't quite work together.  See Will Clinger's essay [[http://www.ccs.neu.edu/home/will/R6RS/essay.txt|"Fixing the Syntactic Record Layer"]].
    * R7RS-small provides only a SRFI-9-compatible syntactic record layer.  R7RS-large will provide a procedure API, possibly SRFI 99.

 * You cannot simply take a record value and access one of its fields by name.  See [http://lists.r6rs.org/pipermail/r6rs-discuss/2007-August/thread.html#3145].  We want a container that requires certain behavior of the values it contains while not requiring that theybe on an approved list or inherit from the same ancestor.  Importing the accessors of each type of value we wish to contain makes the container's import list the approved list.
    * Providing access to fields by name at run-time makes simple fast implementations of records difficult or impossible.  On the other hand, it's easy to provide such things along with a specific implementation of SRFI 9, though not portably.

 * Despite the last-minute change in the 5.97 draft that attempted to fix things by piling yet another feature, a `parent-rtd` clause, on top of the syntactic layer, you ''still'' have to know whether the base record type was defined using the syntactic or record layer, and you ''still'' can't change a record definition from one layer to the other without running the risk of breaking client code.  Record types defined by the syntactic layer are not interchangeable with record types defined by the procedural layer:  in consequence, the code you write for a record type definition that inherits from some base type depends upon whether that base type was defined using the syntactic or procedural layer.
    * This problem will be solved in R7RS-large.

 * Both record-system layers are complex, which makes it hard for a casual reader to understand their relationships.
    * SRFI 9 is extremely simple, though it takes a little bit of effort to remember exactly what goes where.

 * The record procedural layer is the more expressive layer, so the draft's new warnings that try to frighten programmers into preferring the syntactic layer would have limited impact even if they were true.
    * R7RS will not try to frighten anyone into anything.

== Libraries ==

 * Scheme needs a simple module system that can be grasped easily.
    * Although the R7RS library system has some extensions over the R6RS one, such as `include`, `include/ci`, and `cond-expand`, it is also simplified, removing all talk of versioning and phasing.  Phasing may be added back in some form for R7RS-large.

 * There is no need to embed library versions in import specifications. Matching library versions should be solved outside of the language.  Everything done with versions in the current draft could be added in a backwards compatible manner in future drafts.  Also, the version reference syntax is very complex and rather ugly.
    * Version numbers are not a feature of R7RS libraries.

 * The R6RS library mechanism fails to be modular; users must know and exclude from libraries any exported redefinitions, whether or not they use those identifiers!
    * This continues to be a problem in R7RS.  A ticket has been filed to permit ambiguous imports of unused identifiers.

 * It is perfectly meaningful to say that if a specific library is implemented, it must meet the entire specification.  But ''all'' libraries must be optional in a base language and a base language implementer ''does not'' have to provide all libraries.
    * This is exactly the R7RS approach.

 * R6RS does not provide unsafe arithmetic or list operations.
    * R7RS does not require the safety guarantees of R6RS, leaving the balancing between safety, speed, size, and other concerns up to the implementation.

 * The library system is basically a way to tell the linker how to construct programs from parts.  Rather than invent a new language to do this, it's possible to build the linker such that the programmer can insert Scheme code to control aspects of the linking process.  Such code could do ''more'' than the current library system does, and because of the added expressivity, it could do so more concisely in many cases.
    * The R6RS and R7RS library systems are designed for simplicity and portability to all kinds of existing implementations of libraries.  For those purposes, a declarative system is satisfactory, but doesn't disallow introduction of more complex module systems for other purposes.

 * Many R6RS features are useful and definitely should be part of the standard, but rather than arbitrarily including them in the language, the should be defined ''on top of the language''.  Only features that are required in order to support the libraries should be carefully added to the language itself.  This approach would lead to a ``core language'' and a set of ``extension libraries'' that is implemented in terms of the core language. These libraries should be optional extensions to the core language.
    * While it is not always possible to define R7RS-large packages in terms of the R7RS-small language (i.e. a package supporting TCP connections in the large language cannot be written portably), the overall design of core and extension libraries has been adopted for R7RS.  Some libraries are considered part of the small language, though they are technically optional; the rest belong to the large language.

 * What is the principled reason why `assoc` and `memq` (for example) are exiled to libraries without bringing along the equally derivative `append`, `string-for-each!`, `vector-map`, and so on; the library system fails to delineate a kernel, which I had thought was one of its goals.
    * As noted, the base library is not a kernel.  However, all these procedures are in the R7RS base library.

 * The reason for ABSOLUTELY prohibiting linking to multiple versions of the same library per formal comment 130 is very weak.  Scheme and the LISP family in general are incisive, powerful languages that give their users great power that can be easily misused.
    * R7RS-small does not formally treat versions.

 * Scheme should accommodate the desires of library writers to intermix definitions and expressions (insert usual platitudes about needless restrictions---and how Scheme is perhaps more than anything else a system for communicating to other programmers...).
    * R7RS libraries permit the intermixture of definitions and expressions (wrapped in `begin`), imports, exports, and other library declarations.

 * The enforced phase separation in the library system described in the R6RS proposal overspecifies the library system and disallows different implementation strategies, thus prohibiting the Scheme community from experimenting and innovating.  In addition, the library system becomes an obstacle to the use of Scheme as a teaching language, because beginners now must deal with it long before it can be explained to them why.
    * R7RS-small does not require phase information, because the only macro system is `syntax-rules`, which does not run user-supplied Scheme code at compile time.  (Note that R6RS is not as fully phase-separated as Racket, because while an identifier may be defined or undefined at a given phase, it may not be defined ''differently'' at different phases.)  The R7RS-large library system is not yet specified; certainly implicit phasing will be one of the options considered.  One can write a great deal of Scheme without writing any libraries, and R7RS REPLs are encouraged to import a useful subset of the available libraries.

 * R6RS library descriptions need to be enclosed in an additional layer of parentheses, as opposed to a single definition at the top of the file.
    * Still true in R7RS.  Libraries remain a single S-expression in R7RS.  However, the `include` form in library declarations makes it possible to load another file inside a module, and that file doesn't require the additional level of parentheses.

== Numbers ==

 * It is not clear what problem implicit mantissa widths solve, and they are ill-defined.  Explicit mantissa widths are "additional features", the need for which should be eliminated by removing restrictions against non-decimal floating-point values.
    * Neither implicit nor explicit mantissa widths are provided in R7RS.

 * Requiring the full numeric tower works against using Scheme in smaller environments such as embedded systems, where full support conflicts with other implementation priorities.
    * R7RS-small does not require the full tower.

 * Procedures such as `div0-and-mod0`, `exact-integer-sqrt` simply do not belong in a base language.  In fact a lot of the numerical procedures belong in a library.
    * Integer division has been moved to a library in R7RS, as have the transcendental and complex number procedures (the latter primarily because inexact and non-real numbers are not required in R7RS).  The `exact-integer-sqrt` procedure is provided in the base library because implementations are not required to provide numbers beyond integers.

 * Given that fixnum and flonum types have implementation dependent ranges, there is no need to require fixnum and flonum types in the standard.
    * These types are not present in R7RS-small.  Flonum and fixnum packages will be provided in R7RS.

 * Scheme is best served by retaining its jewel-like character: tiny, beautiful and perfect.  There is no need to expand Scheme as the editors propose -- if I want Common Lisp, I know where to find it. 
    * It's debatable whether the exact set of procedures provided in R5RS is really so jewel-like or perfect.  Certainly there were many, if small, changes from R2RS to R5RS.  

== Pairs ==

 * Immutable pairs may make optimization easier, but do not fit at all with the practice and spirit of Lisp.
    * They aren't provided in R7RS-small, except that literals are immutable (as in R5RS).

== Characters ==

 * Implementations should be allowed to support characters beyond Unicode.
    * R7RS permits such characters, though we don't expect to see widespread implementation of them.  

 * Scheme has been around for a long time, and has seen many encodings come and go - there's no reason to bind it to a single encoding forever.
    * Unicode is unique, being the only encoding that in theory (and to a great extent in practice) allows the encoding of all the world's writing systems.  R7RS does not require more than the ASCII repertoire to be supported, but does require Unicode conformance in case mapping, char-integer conversion, etc.  If Unicode were to be superseded, a lot more would have to change besides Scheme.

 * Character-level case mappings encourage programmers to write broken algorithms.
    * Unfortunately, they are an IEEE Scheme feature and can't easily be removed.  R7RS, like R6RS, requires that they comply with simple Unicode case-mapping, whereas strings must comply with full Unicode case-mapping.  Implementations may forbid certain characters in strings.

 * It is perhaps better to remove those features of R5RS that prohibit Unicode implementation (such as `char->integer` and `integer->char`), and rely on extended SRFI-13 and SRFI-14 for Unicode support.
    * Actually those features aren't problematic for Unicode.  In any case, they are IEEE Scheme features and cannot be easily removed.

== Strings ==

 * The normalization procedures handle normalization at the wrong level.  It's inherently an encoding issue, and is best treated as such.  Providing this functionality at the string level forbids the implementation strategy of keeping all strings in the same normalization form - a very appealing strategy on many levels.
    * R7RS-small provides normalized comparison procedures rather than normalization procedures.  This feature is subject to change.

 * O(1) string access suggests that all Schemes should use UCS-4 character vectors for their string representation, discouraging the use of alternate representations such as UTF-8 and ropes or trees.
    * R7RS does not prescribe O(1) access time for strings, and warns against any such assumption.

 * There are competing character sets that may be preferable to Unicode in some circumstances, and requiring Unicode would forestall their use.  Unicode necessarily requires more storage space than ASCII, and more complicated runtime support, which may not be desirable in all cases.
    * Any encoding may be used internally to an implementation, provided that it covers at least the ASCII repertoire.  

== Vectors ==

 * There is nothing to indicate that the editors ever considered the inclusion of multidimensional arrays, even though they are the subject of SRFI-25, SRFI-47, SRFI-58, and SRFI-63, and portably supported by SLIB.
    * R7RS-large will include multidimensional arrays, though the form they will take is not yet known.  Advanced array operations developed since Common Lisp was standardized will certainly be looked at.

== Bytevectors ==

 *   R6RS does not provide homogeneous numeric vectors.  The fact that bytevectors are, by their nature, type aliasing nightmares is too lightly ignored in the draft.  Making bytevectors the basic implementation strategy of homogenous vectors makes it harder for Scheme systems that compile to C for portability to implement homogeneous vectors efficiently, due to the C aliasing rules.  The suggestion is made in the rationale that bytevectors plus sealed and opaque types can lead to efficient implementations of them.  This assumes the existence of a "suitably smart compiler".  IEEE-754 does ''not'' specify the bit patterns for single- and double-precision floating-point numbers in a way that this report seems to think can be exploited in bytevectors.
    * R5R5-small only provides 8-bit facilities.  There is a R7RS-large proposal on the table that provides other types (s8, u16 and s16 both big- and little-endian, standard-order and native floats, etc.) in both homogeneous and heterogenous styles.

 * The core language must only include the abstract vector type. Having particular types of vector representations is precisely the domain of SRFIs. The R6RS proposal byte vector syntax is not widely agreed upon, and it requires two sets of vector procedures in the core language.
    * R7RS does provide bytevectors in the base.  The lexical syntax provided is SRFI-4-compatible rather than R6RS-compatible.

 * Strings always were a bit sore in Scheme, with their manifestly-typed locations.  Instead of solving that, this draft adds bytevectors.  Neither can contain variables in the right-hand side of a syntax rule - the kind of arbitrary restriction of which Scheme should be free.
    * If you want generic vectors, you know where to find them.  Strings are often not used as containers, but as immutable values, and this is even more true in a Unicode world.

== Multiple values ==

 * Multiple-return values *still* has not been fixed. The most straightforward solution is simply to make all continuations multi-valued - or rather that applying a single-argument continuation to multiple values results in the coercion of the actual arguments to some list form.  It would actually be even better to eliminate multi-valued continuations entirely, but that would be at the cost of breaking existing code (SRFI code even).
    * Treating multiple values in this way requires checks and coercions every time a value that might not be singular is passed to a continuation expecting one value, which is almost all the time.  Leaving the behavior undefined allows avoiding such coercions and potentially the checks as well.

== Dynamic environment ==

 * It has been clearly shown that `dynamic-wind` (and R5RS `call/cc`) is implementable in terms of R4RS `call/cc`, but that the reverse is not true. Given the renaming facilities available in the library system, it would seem ''really'' obvious that R4RS `call/cc` should be in the core and R5RS `dynamic-wind` and `call/cc` should be a separate library.
    * R7RS provides R5RS `call/cc` and `dynamic-wind` in the core.

 * The Scheme specification needs a solid model for asynchronous events.
    * R7RS-large may have something like this.

== Errors, exceptions and conditions ==

 * Far too much of R6RS appears fixated on defining and punishing syntactic errors.
    * R7RS sticks with "is an error", as R5RS and earlier versions did.  This allows implementations to choose how to handle syntactic and most other errors.

 * The exception system is far too large and over-specified. Implementations should be free to experiment with extensions in these areas.  The division into simple and compound conditions, along with the infrastructure required to manipulate these, is overly complex and may be awkward to use in practice.
    * R7RS-small provides the R6RS ''exception'' system, but not the ''condition'' system; any Scheme object can be signalled (as is also true in R6RS).

 * The exception/condition functionality ''should'' be part of the core specification - as is shown by the inclusion of a section on exceptions in the formal semantics.
    * R7RS exception signaling and handling are in the base library.

 * The R5RS report used the phrase "an error is signalled" to indicate that implementations must detect and report an error, without going into specifics. In contrast, the R6RS proposal requires the signaling of exceptions, and it goes further to propose a complex and very specific hierarchy of exceptions. Once again, common practices in the Scheme community are ignored, and instead of relying on the SRFI process, exceptions are mandated in the core language.
    * R7RS-small follows R5RS.

 * All condition types should be record types, and compound conditions should be eliminated.
    * R7RS-small has no specific condition types (except `error`) or hierarchy.  It's not clear yet what will happen in R7RS-large.

== I/O ==

 * The I/O system was totally rewritten for no reason, and all aspects of it are controlled with gratuitous syntactic forms - second class forms which discourage high-level procedures.
    * R7RS-small provides only R5RS I/O plus a close parallel of it for binary I/O.  R7RS-large will have a more complex I/O package, but its content is not yet defined.

 * R6RS I/O is lacking support for PGM/PPM files, open TCP sockets, and other features.
    * R7RS-large will have a TCP package.

 * All but fundamental I/O primitives should be put in a library: non-abstract operations do not belong in the core language.
    * In both R6RS and R7RS, there is no I/O in the R7RS base library at all.

== Enumerations ==

 * There's simply no reason for enumerations, since Scheme has traditionally used symbols for the same purpose.  It is an example of premature generalization and piling feature upon feature.
    * Enumerations are not in R7RS-small; the R7RS-large situation is still open.

== FFI ==

 * R6RS is not good enough for commericial usage since it lacks a FFI to interface it with libraries implented in C/C++, and it specifies too much for an experimental language.  Neither fish nor fowl.
    * The R7RS-large WG decided not to attempt to specify a standard FFI.

== Hash tables ==

 * The core specification must not mandate any particular standard for dictionary structures, as the existing implementations vary widely in APIs. A side effect free association list structure may be permitted, but anything more sophisticated must be relegated to a library.
    * R7RS, like R5RS, has the traditional `assoc` and friends, enhanced to allow any equivalence predicate.  Of course, such association lists are not inherently immutable.

  * The standard library report gratuitously eschews important and widely-used existing SRFIs such as SRFI-69 hash tables in favor of its equivalent incompatible functionality.
    * Partly because the WG could not agree on the appropriate API, R7RS-small does not provide hash tables, though they are present in almost every major Scheme implementation.  Note that what matters for compatibility is the availability of specific function points, rather than names and argument order, which can be handled with shim libraries.

 * Hash tables, while useful, are not necessary (after all, other equally useful data structures were not added).
    * Hash tables, along with other containers, will be provided in R7RS-large, though the exact API is as yet undecided.

== Formal semantics ==

 * The operational semantics for the proposed R6RS is nowhere near as clean and succinct as the denotational semantics for R5RS. Considerations of their non-normativeness aside, the denotational semantics presented, in a couple of pages, a clear formalization of the basis of Scheme.
    * The R5RS denotational semantics has been retained essentially unchanged.

== Process ==

 *  It would be regrettable if something becomes a standard because "we didn't have time to come up with a better [idea] by the last couple of days".
    * The R7RS process has encouraged continuous feedback throughout the development process, which has taken place entirely in the open.

 * Several features specified in R6RS have been tested in existing implementations of Scheme insufficiently to warrant inclusion in the standard.  Some features of the draft, such as the I/O system and records, are completely new and clearly suffer from feature creep.  The standard is not the place to experiment.
    * The WG agrees in general; almost all the syntax and procedures of R7RS are already widely available in Scheme implementations, with the exception of the library system, which is very close to (but simplified from) the R6RS system. 
 
 * The jump from R5RS to R6RS is too big for its users (implementers and other programmers) to digest all at once, and has not set a good example for R7RS.  To make sure we all continue communicating and learning from one another, the revision process needs to be more open and incremental in the future, I think.
    * The R7RS process has been much more open (everything has been done in public, and informal comments from the public have been accepted throughout the process), and its changes to R5RS are quite incremental.  We believe that it will be straightforward to implement R7RS on any R5RS system that already has some sort of modules, and on any R6RS system.

 * The current thrust of Scheme development raises a concern that the language will go the way of C++, which seems to have given rise to a large variety of mutually nearly incomprehensible C++ dialects and programming styles, and to a great deal of confusion about what the various features are supposed to do.
    * This is rather speculative.

 * We want to experiment and innovate, and the purpose of standardization is the exact opposite: to prevent experimentation and innovation. The objective of standardization is to get it right on the first try, to allow long term stability and interoperability, when the benefits of such outweigh the benefits of innovation.
    * Experimentation and innovation are among the purposes of R7RS-small.

 * The goal in R6RS should be to make it easier to achieve conformance ''and'' easier to experiment without getting the pedants' backs up. A good standards document helps the documentation effort.
    * The small base library makes conformance straightforward, if not necessarily easy.  The other libraries must be provided in full if they are provided at all, which also makes conformance straightforward.  

 * R6RS will likely create a schism between the various scheme implementations, because it ignores the desires of parts of the community, in order to cater to some others. Scheme can't operate under the assumptions of one-size fits all, because the Scheme community is naturally already heavily invested in areas where divergence (to some degree) is encouraged.
    * The WG agrees.  We hope that R7RS will see wider take-up among the implementer community.

 * If R6RS fails, the next attempt should be ''in the direction'' of previous revised reports in requiring unanimous consent ''for the core'' (which includes the library mechanism, but precious few libraries).  Therefore the process in the future must continue to require super-majorities for core ratification and maybe even greater ones than the current 60% -- and that helps to properly draw the line between it and the required standard libraries.  Library ratification does not need to be as strict: as long as they are "acceptable" and have nothing dreadfully wrong with them, it is better to have usable standards than no standards at all, and in theory the SFRI process has allowed the community to discover what works in practice and is worth standardizing.
    * This is roughly what the R7RS process is doing.  The current threshold is 85% to ratify R7RS-small, 75% to ratify R7RS-large.

== Performance ==

 * R6RS puts too much emphasis on performance.  Is R5RS Scheme so slow that we need to introduce into the standard immutable variables, fields, and pairs?  I am all for speed, but not at the expense of inconsistencies in the language design.  Immutability hinders debugging and "live-repair", and goes against the basic design principles of Scheme.
    * R7RS has only the immutability of R5RS; that is, literal constants are immutable, but implementations are not required to detect attempts to mutate them.