I'll keep this brief.

Scheme is awesome. It actually embodies the principle that a tiny, easy-to-understand, kernel can, through the interaction of elegant, orthogonal, primitives, express concepts far beyond the most complex languages out there - and yet still be implemented efficiently.

People who think the complexity of C++ to be justified might think that sort of thing is impossible. They might think that increasing the functionality of a piece of software means adding more features by writing more code to handle more cases.

I think that as soon as you start doing that, you have failed.

But Scheme hasn't taken off - we have several implementations, and taken together, they're quite impressive. But they duplicate so much work. So many libraries are re-implemented, because it's difficult to write nontrivial portable Scheme code.

And that's my personal goal for R7RS (beyond, of course, the implicit goal of MAKING IT AWESOME); Scheme makes the right way of doing things be the easiest way - so let's make it easier to write portable code that non-portable, by providing a core language that provides all the functionality needed to write code that doesn't have an inherent platform-dependency due to the nature of the problem it's solving.

But without harming the jewel-like perfection, nor making it hard to implement efficiently.

More about me can be found at http://www.snell-pym.org.uk/alaric/ - remind me to bring that page more up to date, though.

My ballots: wiki:WG1BallotSnellPym wiki:WG1ReBallotSnellPym

= Proposals =

 * UniqueTypesSnellPym
 * ErrorsSnellPym
 * ParametersSnellPym