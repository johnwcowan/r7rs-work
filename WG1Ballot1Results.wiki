= Notes about Results =

See [[:WG1BallotExplanation|WG1BallotExplanation]].

= WG1 Finalized Ballot Items =

== Working Group 1 ==

=== #1 Which VCS do we use? ===

We need a VCS to keep track of changes to the standard as we start
drafting it.

  * '''Options:''' bzr, darcs, git, hg, monotone, svn, undecided
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Durusau|Durusau]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]]
  * '''Results:''' '''hg''', git, svn, monotone, darcs, undecided, bzr
  * '''Ratios:''' 5:5, 6:4, 7:2, 8:2, 8:2, 8:0

== WG1 - Modules ==

=== #2 Module System ===

As per the charter, we need a module system
proposal which allows sharing of code between
implementations.

This is one issue where we can't default to
the R5RS, since it has no module system. If
we can't come to consensus, we will have to
take the R6RS module system as-is.

Note the `r6rs--` option is just the
R6RS module system without versioning or
phasing.

  * '''Proposals:'''
    * '''ganz:''' ModulesGanz
    * '''hsu:''' ModulesAndPackagesArcfide
    * '''shinn:''' ModulesShinn
  * '''Options:''' ganz, hsu, shinn, r6rs, r6rs--, undecided
  * '''Default:''' r6rs
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Durusau|Durusau]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''shinn''', r6rs--, ganz, undecided, hsu, r6rs, no
  * '''Ratios:''' 6:4, 6:4, 8:5, 7:2, 8:2, 9:0
  * '''Rationales:'''

 `Gleckler`::
   I've changed my top preference to ModulesShinn for two primary reasons: 1) It is a compatible subset of R6RS, with two syntactic differences that should be easy to implement, so it should be easy to use R6RS libraries that only make use of its subset of the R6RS module system; and 2) it includes the `include` form, allowing code to be separated from its library specification, which makes it easy to use code that is intended to be loaded at top level, e.g. in R5RS implementations, eliminates unnecessary indentation, and feels more compatible with interactive development.
 `Harvey`::
   Honestly, after reading a bazillion emails on this topic, I'm having trouble understanding the differences among these proposals. They all claim not to screw up the REPL, which is the important thing. But if "wg2" were an option it'd definitely be my first choice. I know there's no NO option, but according to John's excellent chart, the r6rs option is the only one that's way, way, way worse than nothing, because of "phasing: yes." Shinn gets the edge on simplicity: top-level only, explicit exports, etc. Ganz beats out Hsu also on simplicity, since Hsu has two columns in the table with different properties, already way too complicated. I feel compelled to reiterate that none of these proposals strikes me as jewel-like.
 `Hsu`::
   Rationale: R6RS-- is our best compromise and the most useful for WG1, but modules should be syntactic entities.
 `Medernach`::
   This is only a preference indication, more work need to be done in this area before deciding a final module system.
 `Rush`::
   Seriously. I have not had enough time to digest the various proposals on this topic. And I find it unfortunate that the voting system forces me to express an opinion on topics which I have not fully evaluated just to make sure that i give sufficient weight to my *actual* vote.
 `Shinn`::
   Anything beyond a static syntax is specifying too much, and would make integration with existing systems difficult to impossible.

== WG1 - Core ==

=== #40 SRFI vs. R6RS precedence ===

Given equal technical merit and compatible extensibility for WG2,
should WG1 prefer SRFIs or standardized behaviors from R6RS when faced
with the choice. For example, a version of syntax-violation
vs. syntax-error.

This is a meta-item, to be used only as a guideline.

  * '''Options:''' srfi,r6rs,undecided
  * '''Voters:''' 11: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''srfi''', r6rs, undecided
  * '''Ratios:''' 7:3, 8:1
  * '''Rationales:'''

 `Ganz`::
   I'd prefer to decide case-by-case...in this case syntax-error
 `Harvey`::
   "Given equal technical merit" is so unlikely that I don't think this will ever actually be invoked. But the less like r6rs the better I like it!
 `Medernach`::
   It is difficult to decide of a general rule. What matters mainly is what people really use and find most useful.

=== #37 transcript-on and transcript-off ===

These were relegated to a compatibility library
in R6RS.  Do we want to keep them, drop them, or
move them to a library?

Yes means to keep them in the core, as in R5RS,
and no means to remove them entirely.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' yes
  * '''Voters:''' 16: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Durusau|Durusau]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''no''', module, wg2, yes
  * '''Ratios:''' 8:6, 13:0, 13:3
  * '''Rationales:'''

 `Radul`::
   When in doubt, dike it out.

=== #38 letrec* ===

R6RS added letrec* and defined the semantics
of internal define to be equivalent.  Do we
want to add this?

Choose `letrec*` just to add the syntax, `define` to change the
behavior of internal define, or `yes`/`both` for both.

  * '''Options:''' both, letrec*, define, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''both''', letrec*, define, module, wg2, no, undecided
  * '''Ratios:''' 10:2, 10:0, 9:3, 10:0, 10:1, 10:1

=== #41 Should we adopt the SRFI-1 extension to MAP and FOR-EACH? ===

This extension allows the list arguments to be of unequal length, and
stops the procedure whenever any of them run out.  R5RS says the lists
''must'' be of the same length, R6RS says they ''should'' be.

`Yes` to allow unequal length.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', no, module, wg2, r6rs
  * '''Ratios:''' 9:4, 9:1, 9:0, 9:1
  * '''Rationales:'''

 `Medernach`::
   Silently accepting lists of unequal length is error-prone, especially if argument lists are received in parameters and if we are not sure about their length, it means we need to check at every map / for-each call if lists are on the same length, really not a good idea to my opinion. If one wants a mapping function accepting unequal length lists it is easy to write its own version.

=== #42 Should we adopt the SRFI-1 extension to ASSOC and MEMBER? ===

This extension accepts a third argument, the equality predicate to be
used.  Alternatively we could use the R6RS predicates ASSP and MEMP.

  * '''Options:''' srfi-1, r6rs, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''srfi-1''', r6rs, wg2, yes, module
  * '''Ratios:''' 11:2, 11:1, 12:2, 10:3
  * '''Rationales:'''

 `Radul`::
   Either or both.

=== #33 dynamic-wind ===

New to R5RS, do we reaffirm the sometimes debated dynamic-wind?

Removing this would require a strong rationale indicating that it's
fundamentally flawed.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' yes
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', module, wg2, no, undecided
  * '''Ratios:''' 9:3, 11:1, 11:1, 11:3

=== #34 multiple values ===

New to R5RS, do we reaffirm multiple values, specifically the
procedures `call-with-values` and `values`?

Removing this would require a strong rationale indicating that it's
fundamentally flawed.

Note if these forms are removed or placed in a module, for consistency
none of the core library should return multiple values (as is the case
in R5RS).

`Yes` to keep them, `no` to remove them, and `module` to relegate them
to a module.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' yes
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', module, wg2, undecided, no
  * '''Ratios:''' 8:4, 10:2, 10:3, 10:2
  * '''Rationales:'''

 `Radul`::
   Multiple return values are important, but is seems wrong to do them by introducing a special data type (the frob produced by VALUES) and special implicit destructuring (the lambda list of the receiver argument of CALL-WITH-VALUES) for that purpose alone. Linguistically, it would be much cleaner to just have destructuring in bindings in general, and have VALUES just be a convention for using LIST (or VECTOR). The circumstances where explicit destructuring of returned compounds is difficult are defining (or assigning) multiple names to be the components of the return of a single expression.
 `Shinn`::
   I dislike MV for the complexity it introduces into any combinators or higher order utilities, but would rather preserve backwards compatibility by relegating it to a module.
 `Sussman`::
   It is important to allow multiple values that can result in multiple definitions and assignments, but it is not apparent to me why we need a special data type to implement them.

=== #54 optional arguments ===

Scheme's primitive mechanism of improper lambda-lists allows for
optional arguments, but only with extra machinery.  CL, DSSSL, and
some Schemes provide a special word such as `#!optional` in
lambda-lists, showing that the arguments which follow are optional and
may have default values.  SRFI-89 provides both optional and keyword
arguments via `lambda*` and `define*` and without introducing #!foo
special tokens.

Note the original ticket description mentions `case-lambda`, but this
is easily provided as a separate module, and will be a separate item.

  * '''Options:''' dsssl, srfi-89, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 16: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Durusau|Durusau]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''wg2'', '''no''', srfi-89, undecided, dsssl
  * '''Ratios:''' 6:5, 5:3, 6:2, 5:2
  * '''Rationales:'''

 `Ganz`::
   NAMES ARE PRECIOUS: OVERLOAD lambda AND define RATHER THAN RENAMING THEM
 `Harvey`::
   Much as I'd like optional arguments, I can't support srfi-89 because it depends on srfi-88, keywords, which are an incompatible change that will break r5rs programs.
 `Radul`::
   This is feature creep. Optional arguments are a subset of the functionality of a good pattern-matching and destructuring mechanism for bindings. I believe that WG2 should consider such a mechanism.
 `Shinn`::
   Too much to work out, too many differing opinions, let's leave this to wg2.
 `Shivers`::
   I dislike the current n-ary-via-lists mechanism very much. It inflicts lists on the language's core. It is essentially impossible to manage in the compiler. But I am not a fan of the alternatives proposed herein.
 `Sussman`::
   WG2 should consider extending the DEFINE and LET syntax to include a nice pattern destructuring language. But don't accept some partially cooked kludge.

=== #57 Simple randomness ===

Student programs often want a small amount of randomness, not
necessarily of very high quality.  Shall we provide a simple interface
to a random variables in WG1 Scheme?

  * '''Proposals:'''
    * '''cowan:''' RandomCowan
  * '''Options:''' cowan, srfi-27, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''wg2'', '''no''', srfi-27/module, srfi-27, cowan/module, cowan, srfi-27/core, cowan/core, hsu/core, hsu, hsu/module, undecided, srfi/27-core, module
  * '''Ratios:''' 9:3, 9:4, 9:4, 10:3, 10:3, 10:3, 11:2, 10:2, 10:2, 10:2, 12:0, 12:0, 12:1
  * '''Rationales:'''

 `Gleckler`::
   RandomCowan does not allow control over the seed, so it is of such limited usefulness as to not be worth including. The API defined by SRFI 27 does allow control of the seed, and makes random sources first class, both of which are good ideas. However, the API is awkward, especially `random-source-state-ref` and `random-source-state-set!`. I'd like to see WG2 do a survey of existing implementations and find something better than both of these proposals.
 `Hsu`::
   Rationale: Randomness without seeds is not useful. We should have a complete, functional, but simple interface rather than one that is over done.
 `Lucier`::
   If there cannot be a quality, standard random number generator suitable for simulation purposes (which SRFI-27 is, see the mail list discussion of SRFI-27), I'd prefer to leave it to wg2.
 `Medernach`::
   This is really a module issue, let people choose among a set the one which fits the best their needs. Standardize names only for helping code reuse. By the way if one need repeatability why not roll their own random stream from a saved persistent table ? (as good old random number tables for those who knew about it :)
 `Radul`::
   If you can't do it right, don't do it at all. Unlike a module system, randomness can be retrofit at user level, so Scheme will not shrivel up and die if we wait for perfection.
 `Rush`::
   This is essentially a strong *NO* vote. Again, I feel that this voting system here inapropriately conflates two issues: whether we should *have* such an interface, and *if* we should have such an interface, what shape it should have.
 `Shinn`::
   This is actually pretty crucial for simple games, but I'd still rather leave it up to wg2. Providing just `current-seconds` is enough to allow users to implement a much better random library than any of the simplified interfaces suggested.
 `Sussman`::
   Do not introduce anything "not necessarily of very high quality" into the language! Don't do anything that Knuth and Kahan would not approve of! If you have good integers and assignment a user can make his own, so this is not essential.

=== #59 current-error-port ===

Pretty much all Schemes except embedded ones provide a notion of
current error distinct from current output.  Should this be exposed as
a Scheme output port?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', module, wg2, undecided
  * '''Ratios:''' 10:3, 12:0, 12:1

=== #60 Simple file operations ===

Should WG1 provide a module equivalent to the (rnrs files) module?
This provides `delete-file` and `file-exists?`, which are pretty much
necessities for any file-driven programming.

Note PortsCowan automatically includes these - voting for them here
guarantees them even if not included by a specific proposal.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''module''', yes, wg2, undecided, no
  * '''Ratios:''' 8:2, 8:0, 8:2, 8:1
  * '''Rationales:'''

 `Shivers`::
   This is sitting down between two stools.

=== #64 Consistency in sequence procedures ===

Should we add the 10 procedures mentioned at CompleteSequenceCowan in
order to make the Scheme sequence types consistent?  They are
`make-list copy-list list-set! string-map string-for-each
string->vector copy-vector vector-map vector-for-each vector->string`,
all with the obvious interface and semantics.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', module, wg2, no, undecided
  * '''Ratios:''' 9:2, 9:1, 9:2, 8:2
  * '''Rationales:'''

 `Harvey`::
   The whole reason we /have/ more than one sequence type is that each is better at some things than others. I don't think we should make it harder to learn which is which!
 `Radul`::
   Except for the string ones. A string is really not a vector of characters --- unicode causes infinite amounts of trouble. But the list and vector manipulations are fine.
 `Shinn`::
   These should be in a module. Arguably, the list equivalents like map and for-each should be in modules too. Their extremely useful but a loop syntax is generally faster and easier to read.
 `Sussman`::
   It is not very good to think of strings as 1-dimensional arrays of characters. What about accents and other hairy stuff? Be afraid! Consider the complexity of Unicode!

=== #65 Precision indicators ===

R5RS requires that Scheme support five indicators for the precision of
floating-point values, not only the default `e` but also `s`, `f`,
`d`, and `l`.  Only a few Schemes actually support more than one
precision, so this is mostly noise.  Shall we make it an optional
feature?

  * '''Options:''' required, optional, no, wg2, undecided
  * '''Default:''' required
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''optional''', wg2, required, undecided, yes, no
  * '''Ratios:''' 8:1, 8:2, 8:2, 8:1, 8:0

=== #66 Add EXACT-INTEGER? ===

Should we add an EXACT-INTEGER? predicate? Currently, to determine
whether a number is both an integer and exact, we must test for both,
which requires some hackery or poor pattern matching to optimize in
existing Scheme implementations.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', no, wg2, undecided, module
  * '''Ratios:''' 11:2, 11:3, 10:2, 11:0
  * '''Rationales:'''

 `Radul`::
   Leave the hackery and the pattern matching in the implementation, don't put it into the standard.
 `Sussman`::
   Compiler shoould be able to optimize "(and (integer? x) (exact? x))"

=== #44 Testing function arity ===

We would like a standard for checking function arity. 
SRFI-102 proposes a way to check function arity:

  * '''Options:''' srfi-102, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''no''', wg2, srfi-102, undecided
  * '''Ratios:''' 10:2, 10:3, 11:2
  * '''Rationales:'''

 `Ganz`::
   THIS WOULD NEED TO BE EXTENDED SOMEHOW FOR KEYWORD ARGUMENTS A LA SRFI-89
 `Harvey`::
   Much as I'd like arity information, this is too complicated for wg1, because of its support for case-lambda.
 `Radul`::
   This is feature creep. It is useful, but since it is not closed, it opens the door to an endless pile of additional features that people will want to solve further and further problems. For example, the procedure returned by calling (lambda (f) (lambda args (apply f args))) wants to have the same arity as F. This must then either be deduced (impossible in general) or specified by the programmer (piling feature upon feature). And it doesn't end there. "No reflection" is a reflection sweet spot; we can jump to the next one if we want, and if we know what it is, but we should not wander aimlessly through this space.

=== #51 support for cyclic structures in primitives ===

list?, length, equal? and other fundamental primitives may diverge
when given cyclic data.  In the former two cases, avoiding this is
simple and not inefficient, and the equivalents are already provided
in SRFI-1.  In the latter case a
[[http://www.r6rs.org/r6rs-editors/2006-February/000969.html|proposal]]
was made and rejected on the R6RS list.  In the former case, R6RS
seems to require `list?` return `#f` and `length` raise an error.

Do we want to specify the behavior when these primitives encounter
cyclic data?

Options are `equal?` to specify `equal?` must not terminate on cyclic
input, `r6rs` to specify R6RS behavior for `list?` and `length`,
`srfi-1` to specify the SRFI-1 semantics (where `length` returns `#f`)
and `equal?+r6rs` or `equal?+srfi-1` are options for both.

  * '''Options:''' equal?, r6rs, srfi-1, equal?+r6rs, equal?+srfi-1, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' r6rs, wg2, ''no'', equal?+srfi-1, srfi-1, native, srfi-38/core, srfi-38/module, undecided, equal?+srfi, equal?+r6rs, module
  * '''Ratios:''' 4:2, 4:4, 4:3, 3:3, 4:1, 4:1, 4:1, 4:2, 3:1, 3:1, 4:0
  * '''Rationales:'''

 `Harvey`::
   I think I must be misunderstanding the issue about EQUAL?. You want to specify that it *must not terminate*? That seems, um, draconian.
 `Hsu`::
   Note: I want R6RS behavior, which requires that all of `list?`, `equal?`, and `length` terminate and return reasonable values. I especially do not like the idea of requiring `equal?` to not terminate.
 `Medernach`::
   About shared structures I would prefer an alternative parenthesis syntax for declaring this kind of values something like this: (make-shared ((a 'foo) (b (1 (a b) c)) (c #(2 b))) (list a b c)) equivalent to in SRFI-38 external representation: ('foo #1=(1 ('foo #1#) #2#) #2=#(2 #1#)) The rationale behind it is to avoid wild mutations when building shared structures, and a more human readable notation. Maybe this has to be discussed in a module for graph-like or circular data ?
 `Shinn`::
   Definitely or list? and length, equal? might need more consideration.
 `SnellPym`::
   I assume that the above is a typo, and specifying "equal?" means that equal MUST terminate on cyclic input!

=== #58 exact-integer-sqrt ===

Should WG1 include `exact-integer-sqrt` from R6RS?  It allows square
root operations in Schemes that don't provide inexact arithmetic, and
has different semantics from `sqrt`, as it rounds its argument down to
the nearest exact square.

  (exact-integer-sqrt k) => (values s r) ; k = s^2 + r

`r6rs`/`yes` for R6RS semantics, `list` to use a list instead of MV,
or `single` to only return `s`.

  * '''Options:''' r6rs, list, single, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''r6rs''', module, wg2, single, no, undecided, list
  * '''Ratios:''' 6:4, 8:3, 7:2, 8:2, 8:2, 7:2

=== #61 finite? nan? ===

Shall we add these numeric predicates defined on the IEEE floating
point values from #20?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', module, wg2, undecided, both
  * '''Ratios:''' 9:2, 10:1, 10:2, 10:1

=== #63 call/cc short name ===

Should we allow `call/cc` as an equivalent to
`call-with-current-continuation`?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' yes
  * '''Voters:''' 12: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]]
  * '''Results:''' '''yes''', no, module, wg2
  * '''Ratios:''' 7:5, 7:2, 7:2
  * '''Rationales:'''

 `Shinn`::
   Emphatically no. Aliases do '''not''' belong in a small standard.

=== #53 Implicit BEGIN to implicit LET-NIL ===

In general, in places where an implict BEGIN occurs, it is possible to
change this to an implicit LET-NIL and remain backwards
compatible. Should we do this?

This is a meta-item to be used as a guideline, and specific places
would need to be brought up for review.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', wg2, no, undecided, module
  * '''Ratios:''' 8:1, 8:2, 8:2, 8:0
  * '''Rationales:'''

 `Shinn`::
   I'd only consider this on a case-by-case scenario.

== WG1 - Exceptions ==

=== #18 Exception System ===

R6RS provided a detailed exception system with
support for raising and catching exceptions, using
a hierarchy of exception types.

Do we use this, or parts of it, or a new exception
system?  The `r6rs` option is just for the core
exception handling, not the conditions hierarchy.

  * '''Proposals:'''
    * '''cowan:''' ExceptionHandlingCowan
  * '''Options:''' cowan, r6rs, wg2, none, undecided
  * '''Default:''' none
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''wg2'', r6rs, '''no''', cowan/module, cowan, r6rs/module, r6rs/core, undecided, cowan/core
  * '''Ratios:''' 6:5, 7:3, 9:1, 7:4, 7:4, 7:4, 10:1, 9:1
  * '''Rationales:'''

 `Gleckler`::
   While the R6RS exception system is not perfect, I'm happy with it. In WG1, it belongs in a module, not in the core. If we don't agree to use the R6RS system, then I'd rather see WG2 refine it instead of including ExceptionHandlingCowan in WG1, since the ExceptionHandlingCowan proposal doesn't explain the rationale for its deviations from R6RS. I've studied the mailing list archive and can't find a convincing argument for ExceptionHandlingCowan, either, so I'm sticking with R6RS or, as a fallback position, WG2. The largest flaw I see with the R6RS condition system is that its condition taxonomy is too coarse and focused on operating-system issues. Compare it with the taxonomy of Gambit or MIT Scheme, for example. (See ExceptionTaxonomies for details of the condition taxonomies of many Scheme implementations.) Some people have proposed taking the R6RS exception system but not its condition taxonomy. If we do, I hope we'll still standardize on some taxonomy rather than none. Without a common taxonomy, it's hard to share code.
 `Hsu`::
   I propose that we provide exception handling as specified in R6RS without the condition system, or that we consider the condition system separately. Rationale: R6RS does mostly the right thing here with exceptions. The condition system could use more work, but we should leave that to someone else for now.
 `Medernach`::
   We already have call-with-current-continuation, don't we ? I feel exceptions as inappropriate for WG1. Especially because of the complex / ad hoc exception taxonomy, mainly because reunifying existing taxonomies leads to over-specifications. However I strive myself for a good interruption system, this is not really continuation: when a process is interrupted it is not waiting for a value, it could be resumed safely only once and it could be interrupted at (quite) any moment. I would like to be able to interrupt a computation because of a timer or because I ran many prediction branch in parallel and want to stop false ones, or just because something from the "outside world" just happens (user requesting a break, hosting machine is shutting down, memory is running out, etc. who knows ?) IMHO trying to enumerate all exceptional situations (ie having a taxonomy) and standardizing them is not a good design for WG1. This simply has to do with cancelling a transaction or making continuations checkpoints and restart from there requested by something external. Everyone always wants critical programs to fall back consistently. Maybe this has something to do with parallelism, but not only (On a single process I could setup a clock, if I finish before it this is nice else I give the work done so far, latter if needed I could resume the work where I was or from latest checkpoint, etc.) Some really simple and generic interface like: "After an asynchronous call to interrupt (named ?) process the "interruptor" have a checkpoint (continuation-like ie expecting a value) and an interrupted process which could be discarded or resumed." is more flexible, schemish and appealing to me.
 `Radul`::
   The only reason I can imagine for wanting exceptions in the core is to specify which conditions various provided procedures (including ERROR) will raise. Leave this to WG2: let them amend the specifications of any WG1 procedures with their behavior in exceptional circumstances.
 `Rush`::
   This is essentially a strong *NO* vote. Again, I feel that this voting system here inapropriately conflates two issues: whether we should *have* such an interface, and *if* we should have such an interface, what shape it should have. It is particularly frustrating to express any positive preference over what is essentially a binary issue just to give sufficient weight to my *no* vote.
 `Shinn`::
   The core R6RS exception system is fine, it was the extensive condition hierarchy and required exception situations I had issues with.

=== #17 error ===

Do we support the near ubiquitous SRFI-23 error procedure,
and if so should it use the SRFI-23 signature, R6RS, or
type-dispatch on the first argument to allow both?

Note ExceptionHandlingCowan currently includes a SRFI-23 compatible
`error` procedure.

  * '''Options:''' srfi-23, r6rs, both, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''srfi-23''', r6rs, both, wg2, wrfi-23, no, undecided, module
  * '''Ratios:''' 7:5, 7:4, 9:1, 9:1, 9:1, 9:1, 9:0
  * '''Rationales:'''

 `Hsu`::
   Note: I have a problem with type dispatching on both because R6RS allows for any type of value to be the first argument to the `error` procedure, which can be convenient, especially with regards to strings.
 `Shinn`::
   Definitely not the R6RS version, it breaks too much code.

== WG1 - I/O ==

=== #30 string ports ===

Do we support string ports, as implemented by SRFI-6
or as by R6RS?

Note that currently PortsCowan provides SRFI-6 string ports.

  * '''Options:''' srfi-6, r6rs, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''srfi-6''', r6rs, module, wg2, undecided, no
  * '''Ratios:''' 10:2, 10:1, 11:1, 11:1, 10:1
  * '''Rationales:'''

 `Hsu`::
   I like the R6RS version better because it separates the port type returned from `get-string-output-port` and the means of extracting the data. I think this separation is good for a number of reasons, the two primary ones being that we can make it possible for an application to write to an output port without being able to extract the data from the port, easily. Secondly, that the port returned can, if desired, be identical in behavior to a normal textual output port, as opposed to forcing a different port type.
 `Radul`::
   Perhaps all the I/O stuff can be hidden off in a module? Maybe except a tiny facade of the most basic things?
 `SnellPym`::
   Implementations can provide SRFI-6 through the usual mechanisms, rather than it being put into WG1.

=== #52 read/write cyclic data ===

SRFI-38 standardizes the #0=(1 . #0#) shared
structure notation for read/write.  In the case
of write, this can be expensive to compute, but
otherwise the common case of the repl printing
a cyclic structure results in an infinite loop.

Do we want to add support for this, as an option
or separate set of procedures?

`srfi-38` for separate procedures or `native` to require `read` and
`write` to handle cyclic notation.

  * '''Options:''' srfi-38, native, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''native'', srfi-38, wg2, srfi-38/core, srfi-38/module, no, module, undecided
  * '''Ratios:''' 6:4, 5:5, 6:2, 6:2, 6:2, 8:2, 8:1
  * '''Rationales:'''

 `Ganz`::
   SUPPORT AS AN OPTION
 `Gleckler`::
   SRFI 38 supports this notation, but doesn't require that `read` and `write` support it, so writing doesn't have to become more expensive. Since many programs won't need this feature, it belongs in a module.
 `Hsu`::
   Rationale: This is a common extension that should be supported, as it makes the programs more reliable. It should be built in, not tacked on, so native is the way to go.
 `Radul`::
   Some variation on this theme (I would like to see the names for common subexpressions be able to be semantic, and perhaps introduced by an object named something like LET instead of inline) would certainly be good to have around, but it would probably complicate the WG1 language too much.
 `Rush`::
   Again with a binary issue including a multi-valued one. In this case, however, I am in favor of the proposal because there should be no way to inadvertantly throw a built-in into an infinite loop.
 `Shinn`::
   It's crucial to be able to have control over whether or not shared structure is handled on read and write.
 `Sussman`::
   The srfi-38 stuff is certainly valuable, but it really is not essential in the core of the language. This is really a common-subexpression elimination of data objects.

== WG1 - Macros ==

=== #7 (... ...) ellipse escaping in syntax patterns ===

A popular extension, formalized in the R6RS,
is to allow "(... <templ>)" in a syntax-rules template
to be an escape for "<templ>".  Do we use this, and
if so what does (... <t1> <t2>) mean?

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 11: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', wg2, undecided, no, yes/core, yes/module
  * '''Ratios:''' 6:3, 7:1, 6:2, 6:1, 7:0

=== #39 syntax-error ===

Should we have syntax-error parallel to SRFI-23 error?  This is evoked
when macros are expanded.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 10: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]]
  * '''Results:''' '''yes''', wg2, module, no
  * '''Ratios:''' 8:2, 8:0, 8:2
  * '''Rationales:'''

 `Ganz`::
   OR PARALLEL TO R6RS ERROR?
 `Hsu`::
   This is not something that we should include natively because the normal way to implement this is through a procedure, which does not work with non-procedural macro systems, which are the only types of macro systems on the table for WG1. We could implement it syntactically, but I think it is better to do this procedurally in WG2.

=== #5 syntax-rules ===

Do we keep syntax-rules in the core, relegate
it to a standard module, or leave it out entirely
(possibly letting WG2 specify it).

`Yes` to keep in core, `no` to remove from Scheme entirely.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' yes
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', module, wg2, undecided
  * '''Ratios:''' 7:5, 9:2, 9:1

=== #10 identifier syntax ===

R6RS introduced identifier syntax as a way to
expand identifiers in non-macro positions.

Orthogonal to the overall macro system and what
types of expanders are provided, do we provide
a means to specify identifier syntax?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''no''', wg2, yes, undecided
  * '''Ratios:''' 10:1, 10:3, 10:1
  * '''Rationales:'''

 `Radul`::
   If a thing wants to change how a form is interpreted, the thing should be in the operator position of that form.
 `Shinn`::
   Identifier syntax is actually really cool. I bash it only because it's a big semantic change, and not suitable for a small R5RS-compatible language. I would not be overly opposed to it in wg2.

=== #47 internal define-syntax ===

R6RS extends define-syntax to be allowed
in local lexical contexts.  Do we allow
this as well?

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', wg2, no, undecided
  * '''Ratios:''' 13:0, 13:1, 11:2

=== #6 syntax-rules _ patterns ===

R6RS adds _ as a wild-card pattern, breaking
some existing R5RS macros.  Do we add the _ wildcard,
or leave it as a normal identifier as in R5RS?

Yes to add, no for R5RS.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', no, wg2, undecided
  * '''Ratios:''' 8:4, 7:2, 9:2

=== #8 SRFI-46 ellipse specifier in syntax-rules ===

As an alternative to #7, SRFI-46 proposed
allowing an optional ellipse specified as
an identifier before the literals list in
syntax-rules:

  (syntax-rules ::: ()
     <ellipse now represented as ::: instead of ...>)

Do we allow this?

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', wg2, no, yes/core, yes/module, undecided
  * '''Ratios:''' 5:4, 7:3, 4:4, 6:2, 8:2
  * '''Rationales:'''

 `Hsu`::
   This is no longer necessary with the introduction of a proper module system. It is better to do this through the module system than to complicate the syntax of `syntax-rules`. Rationale: So long as this doesn't break backwards compatibility, it is useful, but I question how useful.
 `Medernach`::
   As a convenience, for me this is better than (... ...) which makes macros generating macros quite unreadable (to my opinion).
 `Rush`::
   The text above seems to incorrectly indicate a #7 from the previous ballot...
 `Sussman`::
   You mean "ellipsis" not "elllipse"! An ellipse is a shape, an ellipsis is a punctuation mark.

=== #9 tail patterns in syntax-rules ===

SRFI-46 and R6RS both allow a fixed number of
tail patterns following an ellipsis in a syntax-rules
pattern:

  (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn)

R6RS further allows dotted tail patterns

  (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px)

where Px only matches a dotted list.

Do we allow either or both of these extensions?

  * '''Options:''' tail, dotted-tail, both, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]]
  * '''Results:''' '''both''', tail, wg2, both/core, both/module, tail/core, tail/module, dotted-tail/core, dotted-tail, no, dotted-tail/module, undecided
  * '''Ratios:''' 8:4, 7:3, 5:5, 9:1, 9:1, 8:2, 10:0, 10:0, 9:1, 10:0, 10:1
  * '''Rationales:'''

 `Gleckler`::
   I don't feel strongly about this issue, but the extensions seem harmless.
 `Hsu`::
   Rationale: These are very useful.
 `Medernach`::
   I don't grasp the meaning of dotted tail patterns, if we have an ellipsis how to know where to stop ? I mean this looks like not consistent with the dotted lambda notation ?
 `Rush`::
   Doesn't dotted-tail == both? It certainly seems so from the syntax used above.
 `Shinn`::
   The dotted-tail is useful, but breaks the rule of the pattern language where what you see is what you get.

== WG1 - Numerics ==

=== #20 inexact infinities ===

R6RS provides support for inexact infinities
and NaN objects.  Do we keep these, and if so
do we use the same literal syntax and arithmetic
as in R6RS?

`Yes` to keep them with the same syntax and semantics of R6RS, or
write in a separate proposal for some other syntax/semantics.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''yes''', wg2, no, undecided
  * '''Ratios:''' 12:1, 12:1, 12:1

=== #21 limited type arithmetic ===

R6RS provides libraries for limited type arithmetic on fixnums only
and flonums only (i.e. `fx+`, `fl*` etc.).  Do we want these?

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''wg2'', module, '''no''', yes, undecided
  * '''Ratios:''' 8:4, 7:5, 10:1, 9:1

=== #22 mantissa widths ===

R6RS introduced the concept of mantissa widths
as an alternative to the R5RS #s in numbers.
Do we want either or both of these?

  * '''Options:''' r5rs, r6rs, both, no, wg2, undecided
  * '''Default:''' r5rs
  * '''Voters:''' 13: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''wg2'', both, r6rs, '''r5rs''', no, undecided, module
  * '''Ratios:''' 5:5, 6:3, 6:4, 6:3, 7:3, 8:0
  * '''Rationales:'''

 `Gleckler`::
   I don't have a good enough understanding of the importance of this issue to have a strong opinion, so I'm following GJS, who is of course a great mathematician and programmer, and have chosen "both" as my first preference.
 `Rush`::
   Don't break my R5RS code & data files!

== WG1 - Reader Syntax ==

=== #11 case-sensitivity ===

Does the reader fold case by default, and if so how?

Yes to fold-case (R5RS) no to preserve case (R6RS), additional votes
to come later from specific proposals.

  * '''Options:''' yes, no, unspecified, undecided
  * '''Default:''' yes
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''no''', yes, implementation-determined, undecided, unspecified
  * '''Ratios:''' 8:7, 8:3, 8:2, 9:1
  * '''Rationales:'''

 `Harvey`::
   YES!!!!!!!!!! Oh, please yes, let's not adopt all of C's mistakes. As I said before, I can't imagine why people think bug-avoidance important enough to put up with hygienic macros, and yet want such a bug-attractor as making "foo" and "Foo" mean two different things. People have raised Unicode as an argument here, but there is a perfectly good Unicode case-folding standard; they invented it precisely so that programming languages can be international in scope without having to endorse the horror of making semantically identical glyphs turn semantically different.
 `Hsu`::
   Rationale: Case sensitivity is more popular, let's go with it. Otherwise, let implementations determine it.
 `Medernach`::
   Really I tell you preserving case is error-prone !
 `Radul`::
   Really, the read syntax should be settable by appropriate metadata on the source of the input, and case folding should be just one of many interesting options (e.g. the @-syntax in Racket).
 `Rush`::
   There may actually be no good answer to this issue. Usually I am against case-folding, but it *is* useful to have when talking about code, and relying on case to distinguish identifiers is arguably moderately evil.
 `Shinn`::
   There are many pros and cons, but all things being equal I prefer case-sensitivity if for no other reason than it is more expressive - it can be used to write case-folding macros.
 `Sussman`::
   This is a default only. The reader syntax should be allowed to be specified as metadata on the source of the input. For example, the PLT reader must be specified at the head of a file. However, for compatability and convenience the traditional default for LISP-based systems should be fold-case.

=== #15 #\foo character names ===

R6RS greatly extends the list of character names,
as well as allowing #\xNN numeric escapes for characters.
Do we allow any or all of these names?

`mnemonic` for `#\tab` and friends, `numeric` for `#\xNN` as in R6RS,
and `yes`/`both` for both.

The exact list of added names is to be decided later.

  * '''Options:''' mnemonic, numeric, both, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''both''', mnemonic, numeric, wg2, no, undecided
  * '''Ratios:''' 13:1, 12:1, 13:1, 13:0, 13:0
  * '''Rationales:'''

 `Ganz`::
   UNICODE IMPLICATIONS?
 `Shinn`::
   Conservative in the names - we don't need to specify all control characters from 0..31.

=== #13 [brackets] as (parens) ===

R6RS allows [] brackets as identical to parenthesis,
with the condition that they must balance.  Do we
accept this extension, propose some other use for
brackets, or leave them unspecified?

`Yes` for R6RS, `no` for R5RS, or write in a proposal for some other
meaning for brackets.

  * '''Options:''' yes, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''no''', yes, undecided, wg2, module
  * '''Ratios:''' 11:3, 11:1, 11:0, 11:0
  * '''Rationales:'''

 `Radul`::
   It would be better to make brackets be reader syntax for some data structure that is distinguishable from lists, and then, if desired, let LAMBDA and friends allow that data structure to be used as a formal parameter "list". See, e.g., Clojure.
 `Shinn`::
   Over my dead body! :)
 `Shivers`::
   No no no no no. You can't take something as syntactically precious as one of the two remaining unused balanced-delimiter pairs and *do nothing* with it.
 `Sussman`::
   Actually, I would like [] to be reader syntax for vector data and allow the lambda, cond, let to accept that kind of data as legitimate syntax for lists. Indeed, {} should be allowed to represent sets.

=== #14 alternate comment syntax ===

R6RS provides support for #; nested sexp comments,
and #| ... |# nested block comments.  Do we include
either or both of these?

  * '''Options:''' sexp, block, both, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''both''', sexp, block, wg2, no, undecided
  * '''Ratios:''' 10:3, 10:1, 11:1, 11:1, 11:0
  * '''Rationales:'''

 `Harvey`::
   ... but I don't feel strongly about it, and I wish the voting mechanism gave us a way to say /how much/ we care about things. I'd spend all my votes on case folding and no-r6rs-modules, of the things so far on this ballot, if I could.
 `Hsu`::
   Rationale: These are very useful.
 `Shinn`::
   I don't like or use the block comments, but I suspect I'll get out-voted on this.

=== #16 symbol escapes ===

R6RS provides character escapes in symbols of the form `\xnnnn;`,
where nnnn is 1-5 hex digits.  Do we accept this extension?  Do we
also allow |...| to escape a whole symbol or a part of one?

In all existing standards pipes are reserved and the |...| syntax is
unspecified.  In most implementations it's recognized, but there are
at least a few implementations where pipes are normal character
constituents.

  * '''Options:''' numeric, quoted, both, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''both'', wg2, numeric, quoted, no, yes, undecided
  * '''Ratios:''' 7:6, 7:3, 5:2, 7:1, 7:2, 7:1
  * '''Rationales:'''

 `Shivers`::
   There needs to be a way to readably print any symbol. STRING->SYMBOL means *any character* can appear in a symbol. So we gotta deal with it.

=== #67 string escapes ===

R6RS provides character escapes in strings of the form \xnnnn;, where
nnnn is 1-5 hex digits, as well as \n, \t etc. C-like escapes for
common control characters. Do we accept either or both of these
extensions?

  * '''Options:''' numeric, mnemonic, both, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''both''', mnemonic, numeric, wg2, no
  * '''Ratios:''' 12:1, 12:1, 12:1, 12:1

== WG1 - Strings and Chars ==

=== #24 char and string folding ===

R6RS provided operations to alter the case of strings and characters
(upcase, downcase, titlecase and foldcase) using locale-independent
Unicode mappings.  Do we provide equivalent mappings?

Note in a Unicode implementation individual character casings are
incomplete, and string case is not defined as a simple mapping of case
over the constituent characters.

Note UnicodeCowan currently provides mappings at both levels.

  * '''Options:''' strings, chars, both, no, module, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''module'', both, wg2, no, strings, undecided, chars
  * '''Ratios:''' 6:5, 6:1, 6:1, 7:1, 7:2, 7:0

=== #26 string normalization ===

R6RS provides procedures to explicitly convert
strings back and forth between the four Unicode
normalization forms.

The previous phrasing of this option was overly vague, referring to
"any form of normalization."  I've had to treat `yes` votes as
undecided for lack of a better default.  If you voted `yes` before
please choose one of the following options or write in your own
proposal.

  * agnostic - `string-ni=?' etc. provides an API of basic normalization insensitive procedures without explicitly converting the strings, analagous to `string-ci=?'
  * generic - `string-normalize` converts to a single implementation-defined normal form
  * separate - `string-compose-canonical`, `string-decompose-canonical` and `string-decompose-compatibility` gives orthogonal control over the normalization being performed
  * specific - `string-normalize-{nfd,nfc,nfkd,nfkc}` converts explicitly to the four normal forms defined in the Unicode standard

Note UnicodeCowan currently provides specific normalization
procedures.

  * '''Options:''' generic, separate, specific, agnostic, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''agnostic'', wg2, specific/module, specific, agnostic/module, agnostic/core, undecided, generic, no, separate/module, separate, generic/module, specific/core, separate/core, generic/core, seperate
  * '''Ratios:''' 6:4, 5:4, 5:4, 4:3, 5:2, 7:4, 5:2, 6:2, 5:2, 5:2, 6:1, 6:2, 6:1, 6:1, 7:0
  * '''Rationales:'''

 `Gleckler`::
   The complexity of Unicode should be, as much as possible, banished from WG1. If it is included, however, I'll follow John's lead, since he is a Unicode expert, and vote for specific/module.
 `Hsu`::
   Rationale: Keep the complexity of unicode down.
 `Rush`::
   There are some artificial dichotomies introduced in this ballot item. My preferred solution includes all of the above, actually; with agnostic/core, generic/core, separate/module and specific/module.
 `Shinn`::
   Totally inappropriate for wg1.

=== #27 string-ref/set! access time ===

R6RS suggests string-ref and string-set! work
in O(1) time, implying strings are implemented
as character arrays.  Do we reaffirm this?

`Yes` for required constant time.

  * '''Options:''' yes, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''no''', yes, wg2, undecided
  * '''Ratios:''' 8:5, 8:1, 8:3
  * '''Rationales:'''

 `Harvey`::
   Yes unless this somehow prevents Unicode strings, I guess.

=== #23 character set ===

R5RS said almost nothing about character sets.
R6RS specified full Unicode.  Do we specify a
character set, or limit the options in any way?

  * '''Proposals:'''
    * '''cowan:''' UnicodeCowan
  * '''Options:''' cowan, r5rs, wg2, undecided
  * '''Default:''' r5rs
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''cowan''', r5rs, wg2, undecided
  * '''Ratios:''' 10:1, 9:2, 10:3
  * '''Rationales:'''

 `Ganz`::
   I SUGGEST CONSIDERING RUBY'S CSI MODEL AS PER #26 ABOVE
 `Harvey`::
   Thank you John for brilliantly finding a way to make everyone happy (where "everyone" = ASCII, Unicode, case-folders).
 `Shinn`::
   Modulo string-normalization, and perhaps some details to be worked out later since it's a large proposal.

----

= WG1 Controversial Ballot Items (not yet finalized) =

== WG1 - Core ==

=== #50 Byte-Vectors ===

Several SRFIs, R6RS, and most Scheme implementations
support some sort of uniform packed integer vectors.
In particular, these are necessary for efficient
binary I/O, and for memory mapping, so WG2 will
certainly want them.

Do we provide a syntax and basic API for these in WG1?

  * '''Proposals:'''
    * '''cowan:''' BlobAPI
    * '''snellpym:''' BlobsAndSRFI4SnellPym
  * '''Options:''' cowan, snellpym, wg2, none, undecided
  * '''Default:''' none
  * '''Voters:''' 12: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' '''cowan''', wg2, undecided, no
  * '''Ratios:''' 6:4, 7:2, 7:0
  * '''Rationales:'''

 `Harvey`::
   But I'd really like a better name than "blob"!
 `Hsu`::
   I am in favor of John Cowan's small WG1 proposal provided that we change the term "blob" to "bytevector".
 `Lucier`::
   I'd like "full cowan", what he'd prefer for WG2
 `Shinn`::
   I think the proposals need more work.

=== #69 Parameters ===

Most Scheme implementations provide some form of dynamic bindings such
as those provided by SRFI-39 parameters.

  * '''Proposals:'''
    * '''cowan:''' ImmutableParametersCowan
    * '''snellpym:''' ParametersSnellPym
  * '''Options:''' cowan, snellpym, srfi-39, wg2, none, undecided
  * '''Default:''' none
  * '''Voters:''' 11: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''cowan'', srfi-39, snellpym, wg2, no, undecided
  * '''Ratios:''' 4:4, 4:2, 4:1, 4:1, 4:3
  * '''Rationales:'''

 `Harvey`::
   My instinct is to vote "no" on everything, but I'm swayed by the argument that if we don't do it we'll get some hideous monstrosity foisted on us by wg2. :-)
 `Hsu`::
   I find very little of use for a parameter that is not mutable, so I can't see a lot of benefit to Cowan's proposal, of immutable parameters, even if we provides some way to mutate them through `parameterize`. I disagree with the ParametersSnellPym proposal because it suggests to different behaviors for parameters based on whether they are mutated by themselves or with `parameterize`. I believe that we should have mutable parameters, and that there should not be a difference whether they are mutated by `parameterize` or not. I think that we should specify that parameters are thread parameters by default in the case of threads, but I am more than happy to have them mutable and have their thread behavior undefined. Programs written in WG1 will continue to run in WG2 in either case. The main issue is whether a parameter based library imported into a WG2 threaded environment will be able to know what is happening. I'd prefer to avoid it being able to tell, which means that I would like to have the threads have their own state. I am assuming of course that a form of global parameters will be made available. I should develop a proposal on this.
 `Medernach`::
   I completely agree that some other mechanism is better in order to share data between threads and separating concern is needed here.
 `Shinn`::
   I think we should have SRFI-9, with `parameterize` required to be thread-safe and direct mutation unspecified in the context of threads.

=== #32 user-defined types ===

Do we support any means of creating disjoint
user-defined types, such as in SRFI-9, SRFI-99
or the R6RS record system?

  * '''Proposals:'''
    * '''hsu:''' RecordsArcfide
    * '''rush:''' UserAggregatesRush
    * '''snellpym:''' UniqueTypesSnellPym
  * '''Options:''' hsu, rush, snellpym, srfi-9, srfi-99, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 14: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''snellpym'', srfi-9, hsu, rush, srfi-99, wg2, undecided, no
  * '''Ratios:''' 4:4, 4:4, 4:3, 4:3, 5:2, 5:3, 5:1
  * '''Rationales:'''

 `Shinn`::
   SRFI-9 doesn't extend well, as shown by SRFI-99's ugliness. A fully keyword-based approach may be better.

== WG1 - Libraries ==

=== #36 hash-tables ===

R6RS and SRFI-69 both provide hash-table interfaces.
Do we provide either of these, or try to provide
some primitives on which efficient hash-tables can
be implemented?

  * '''Options:''' srfi-69, r6rs, no, wg2, undecided
  * '''Default:''' no
  * '''Voters:''' 15: [[:WG1Ballot1Cowan|Cowan]], [[:WG1Ballot1Ganz|Ganz]], [[:WG1Ballot1Gleckler|Gleckler]], [[:WG1Ballot1Harvey|Harvey]], [[:WG1Ballot1Hsu|Hsu]], [[:WG1Ballot1Lucier|Lucier]], [[:WG1Ballot1Medernach|Medernach]], [[:WG1Ballot1Radul|Radul]], [[:WG1Ballot1Read|Read]], [[:WG1Ballot1Rush|Rush]], [[:WG1Ballot1Russel|Russel]], [[:WG1Ballot1Shinn|Shinn]], [[:WG1Ballot1Shivers|Shivers]], [[:WG1Ballot1SnellPym|SnellPym]], [[:WG1Ballot1Sussman|Sussman]]
  * '''Results:''' ''module'', undecided, wg2, no, srfi-69, r6rs
  * '''Ratios:''' 7:5, 7:5, 7:2, 7:2, 8:3
  * '''Rationales:'''

 `Ganz`::
   I'D SUGGEST CONSIDERING BALANCED TREES (AS WITH C++ STL), WHICH DO NOT REQUIRE A HASH FUNCTION.
 `Radul`::
   If we're going to specify hash tables, we must allow room for holding the keys and/or values weakly. There must be system support for this from the garbage collector --- it cannot be written in user code.
 `Shinn`::
   SRFI-69 is broken.
 `Sussman`::
   Must include weak structures and ephemerons, because these structures cannot be built with user code.
