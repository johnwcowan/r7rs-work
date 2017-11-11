This is a sample proposal and voting scenario using
[Ranked Pairs](http://en.wikipedia.org/wiki/Ranked_Pairs) preferential
voting.

There are 8 members, A through H, and they are debating whether and
how to add Yak Handlers to Scheme.  Yak Handlers are a fairly popular
feature among implementations, and a very minimal specification of Yak
Handlers has been given in SRFI -1, which has fairly widespread
support.

After a general discussion period of different types of Yak Handlers,
member A makes a proposal "A" for an extensive and rich set of Yak
Handlers, which is essentially exactly what his preferred
implementation has been using for years.  Member B makes an alternate
proposal "B", based on the existing library in his own preferred
implementation.  Member C then suggests both of these are too big, and
suggests we just take SRFI -1 as is "SRFI".

No further proposals seem forthcoming, and discussion continues, this
time more concretely addressing the 3 proposals and paying attention
to specific details.  Member A notices that the members are fairly
divided between A and B, but everyone would be satisfied with the
SRFI, which makes the it look like the SRFI will come out winning.
However, he truly feels his proposal is the best and doesn't want to
change it, so he makes an alternate proposal "A-Lite" which is
basically a more restricted version of proposal "A", but holding some
of the same ideas and still more featureful than the SRFI.

Discussion continues, final changes are made, and the four proposals
are put to a vote.  This is preferential voting, so members just vote
for their preferences in order, honestly - there is no strategy
involved at this point.  In our votes there is always the implicit
option "R5RS" which is the R5RS status-quo, and if a member doesn't
explicitly rank all proposals, "R5RS" will come before those not
listed.  In particular, if a member chooses to abstain, he is
effectively voting for R5RS over anything else.  In other systems an
abstained vote may not count either way, but we want to take a
conservative approach.  For the purposes of WG1, members are
encouraged to abstain if they are unsure of the feature, or simply
feel they don't understand it well enough to make a decision.

The votes are collected as follows:

Member A: **A** > **A-Lite** > **SRFI** > **B** > **R5RS**

> Member A wants his full proposal to go through, but if not would
> rather have his lesser proposal.  He prefers the SRFI to the B way
> of doing things, but definitely wants the feature no matter what so
> ranks R5RS last.

Member B: **B** > **SRFI** > **R5RS** > **A-Lite** > **A**

> Member B also votes his own proposal highest, followed by the SRFI.
> His implementation can't efficiently implement either of A's
> proposals, so he ranks R5RS (not having the feature at all), over
> both of those.

Member C: **SRFI** > **A-Lite** > **R5RS** > **A** = **B**

> Member C is very conservative, but still thinks the feature is
> desirable.  He ranks the old-standby SRFI first, followed by A-Lite,
> but if not that would rather stick with R5RS and wait for WG2 or
> later standards to implement the SRFI.

Member D: **R5RS**

> Member D is more conservative, and doesn't think the feature belongs
> in WG1 at all, so he votes R5RS over everything.

Member E: **A-Lite** > **B** > **A** > **SRFI** > **R5RS**

> Member E is more practical, and wants the feature added.  He uses
> multiple implementations, and would be happy with either A or B, but
> was convinced that A-Lite is a good compromise.

Member F: **A** > **B** > **A-Lite** > **SRFI** > **R5RS**

> Member F is also practical, and simply wants the most featureful of
> any of the proposals, so he ranks them in that order.

Member G: **B** > **A-Lite** > **A** > **SRFI**

> Member G prefers B to A in general, and would rather have A-Lite
> than A, but would take either over the minimal SRFI or R5RS.

Member H: **A-Lite** > **SRFI** > **B** > **A** > **R5RS**

> Member H is similar to member E, but would rather have the SRFI than
> the full A or B proposals.

The resulting votes look like:

```
(define votes
  '((member-a (A) (A-Lite) (SRFI) (B) (R5RS))
    (member-b (B) (SRFI) (R5RS) (A-Lite) (A))
    (member-c (SRFI) (R5RS) (A-Lite) (A B))
    (member-d (R5RS) (A B SRFI A-Lite))
    (member-e (A-Lite) (A) (B) (SRFI) (R5RS))
    (member-f (A) (B) (A-Lite) (SRFI) (R5RS))
    (member-g (B) (A-Lite) (A) (SRFI) (R5RS))
    (member-h (A-Lite) (SRFI) (B) (A) (R5RS))
    ))
```

which when tallied gives us

| **Winner** | **Loser** | **Count** |
| SRFI | R5RS | 7 |
| A-Lite | R5RS | 6 |
| B | R5RS | 6 |
| A-Lite | A | 5 |
| A-Lite | SRFI | 5 |
| A | R5RS | 5 |
| B | A | 4 |
| A | SRFI | 4 |
| A-Lite | B | 4 |
| B | SRFI | 4 |
| SRFI | A | 3 |
| SRFI | B | 3 |
| B | A-Lite | 3 |
| R5RS | A | 3 |
| A | B | 2 |
| A | A-Lite | 2 |
| SRFI | A-Lite | 2 |
| R5RS | A-Lite | 2 |
| R5RS | B | 2 |
| R5RS | SRFI | 1 |

and with the ranked pairs algorithms gives a total ranking of:

1. A-Lite
1. B
1. A
1. SRFI
1. R5RS

Now, in this case A-Lite is the winner and beats R5RS 6 out of 8 times
for a 75% approval rating.  If 75% were the cut-off, this would pass,
and we would include A-Lite in the first public draft.  However, if
the final standard is put to a public ratification vote by the
Steering Committee, they will want an 85% approval, which suggests we
might want an 85-90% approval internally for individual issues.

Keep in mind that Working Group 2 is likely to review and consider any
proposals Working Group 1 makes, but should hold separate votes if it
does so - members are likely to vote differently for inclusion in WG2
than they would for WG1.

There is one special case vote, for the module system we use, in which
the default option of "R5RS" may not be used because our charter requires
a module system and R5RS doesn't have one.  An earlier proposal on the
mailing list suggested taking the R6RS module system (suitably adapted
to the WG1 standard, and allowing REPL interaction) as the default option,
since the charter encourages R6RS compatibility (where it makes sense).
Rather than defaulting to R6RS on a lack of strong consensus to the
contrary, we may simply want to use the straight-out winner of the
module system vote.
