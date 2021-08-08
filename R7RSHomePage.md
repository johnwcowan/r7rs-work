## R7RS Home Page

This is the home page for R7RS, the Revised⁷ Report on the Algorithmic Language Scheme.
This version of Scheme has been divided into a small language, suitable for educators,
researchers, and users of embedded languages; and a large language focused on the
practical needs of mainstream software development.

The report on the small language was finalized on July 6, 2013.
It is available [in PDF format](https://github.com/johnwcowan/r7rs-spec/blob/errata/spec/r7rs.pdf)
and as [LaTeX source code](https://github.com/johnwcowan/r7rs-spec/tree/errata/spec).
There are [errata](R7RSSmallErrata.md); the above link leads to a version
with all the errata applied.  A complete archive of the
working documents for the small language is available [here](https://small.r7rs.org/).

A concise definition of the first partial edition of R7RS-large, known as 
the [Red Edition](RedEdition.md), was made available in 2016.
It is a frozen copy of the Red Edition ballot plus the names of the libraries.
A [draft version](https://gitlab.com/vmanis/r7rs-large/-/blob/master/reports/red.pdf)
of all the SRFIs merged together with some editing is also available.

Similarly, the second partial edition of R7RS-large,
known as the [Tangerine Edition](TangerineEdition.md),
was made available in 2019.  It points to the SRFIs (and in one case an R6RS library)
and the numeric tower that the Tangerine Edition requires implementations to
provide.  One Red Edition library was updated in a fully backward compatible way.

The plans for future partial editions can be found at [Color Dockets](ColorDockets.md);
Kronos is the next docket to be voted on in 2021, soon to be followed by Orange.
The rest may be voted on in any order, and proposals will move between dockets.

**Note:** Many docketed proposals are stalled for lack of an implementation;
providing portable implementations for entries in the Eos, Leto, Selene, and Pan dockets
(other than those marked "Implementation") would be a great help to the project.
There is no specific order: implement whichever proposals suit you.
Or, of course, write a [new SRFI](https://srfi.schemers.org/srfi-process.html)
which can then be docketed.

For R7RS-small implementations, see the
[Implementations](ImplementationSupport.md)
page.  [Larceny](http://larcenists.org) includes all the SRFIs of the Red Edition,
though they are not yet available under their standardized names.
[Chibi](http://synthcode.com/wiki/chibi-scheme)
version 0.8 (Oxygen) also contains the entire Red Edition, plus a non-standard library
`(scheme red)` that imports and exports them all.  Other implementations will hopefully follow.

For R7RS benchmarks on many Schemes, see
[this graphic](http://ecraven.github.io/r7rs-benchmarks/benchmark.html).
Some adjustments were made to accommodate the differences between Schemes.
The source is [here](https://github.com/ecraven/r7rs-benchmarks).
