## Vincent Manis's comments

\[These comments refer to an earlier version of [NumericTower](NumericTower.md).  --cowan]

I'd prefer to couch my reply in terms of what data types are provided by an implementation. That maps pretty well onto John's four characteristics, but it seems easier for me to think about. I'll try to match my responses to John's ++-- strings.

Of the towers John lists, the following seem worth standardizing.

1. ----: this seems appropriate in implementations for limited embedded processors.
2. --+-, --++: there seem to be two kinds of possible implementation: a Chicken-like system that provides fixnums and flonums, and a system that provides flonums only (and therefore there is no such thing as an exact integer). Lua, with default build options, works this way, as does [JavaScript](JavaScript.md). Both variations seem defensible to me. [second variation will not work, because exact numbers must exist and be disjoint from inexact ones.  --cowan](The)
3. +-+-, +-++: I believe that the implication here is that bignums are provided as well. \[Yes, but they might be very small bignums.  --cowan]
4. +++-, ++++: This adds ratios to the previous towers.

The others don't seem of much interest, even though some implementors have chosen among them.

Adding complex numbers to an implementation that doesn't support them can be done almost entirely as a library module, apart from issues such as read and write syntax. Similarly, adding ratios to an implementation that supports bignums again can be done almost entirely as a library module. (Proof: Chicken's numbers library, which John references.)

I therefore think that there are three defensible core towers, namely (a) ----, (b) --+-, and (c) +-+-, along with two library modules (ratios and complexes) that may or may not happen to be imported automatically. With careful design, it ought to be possible to provide reference implementations of these modules that would work on any implementation that supports bignums/ratios or inexact reals, respectively. This would allow the implementor to have his/her cake (smaller implementation, less work), and eat it (supporting a fuller tower).

I hope that the Report can be written in such a way that this smaller number of towers is either preferred or required. This maximizes code portability, while still making it possible to build small implementations.  \[That basically rules out bignums without flonums. --jcowan]

If the Report does allow multiple towers (which I think is a foregone conclusion), a built-in procedure with a name something like numeric-features should be required, this will allow portable code to verify that it is running on a sufficiently-capable system. numeric-features could return a list of tower criteria that the system supports. The criteria, and their symbol names, should be defined by the Report. \[`cond-expand` now provides this at compile time rather than run time.  --jcowan]
