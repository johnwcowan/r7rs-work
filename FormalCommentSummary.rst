Near the end of the WG1 process, a Formal Comment sub-process was performed based on the sixth draft.  A total of 21 Formal Comments and one Formal Objection were received, which are listed here.  Note that the ticket numbers are links to verbatim copies of the actual comments; in each case, the first sentence here is the commenter's own summary, with editorial notes in square brackets.  For details of WG votes, see [wiki:WG1Ballot6Results].

== Not accepted, or not accepted in their entirety ==

=== Commenter may not be satisfied ===

#421: #!fold-case and `#!no-fold-case` have no final delimiter.  Adopted in principle after a vote by the WG, although a different syntax was chosen (they are delimited by whitespace).  The commenter was asked if he is satisfied, but made no reply.

#423: When does `eqv?` return `#t` for procedures? [[pointed|out inconsistencies and proposed a return to the R5RS rule]]  Rejected by a vote of the WG, which kept the R6RS rules.  The commenter was asked if he is satisfied, but made no reply.  Note:  The WG has now unanimously changed its mind and returned to the R5RS rule as part of the post-plebiscite process.

#434: List of named characters is incomplete.  Treated mostly as editorial, and adopted by the editors.  The WG voted not to adopt any specific recommended source of named characters for implementation extensions.  The commenter was asked if he is satisfied, but made no reply.

#435: Bytevectors should be called u8vectors.  Rejected by the editors, on the grounds that the WG had already voted and no new arguments had been presented.  There was considerable (and widening) dispute [[http://lists.scheme-reports.org/pipermail/scheme-reports/2012-July/002386.html|on the scheme-reports mailing list]], but the editors' view prevailed and no vote was taken.  The commenter was asked if he is satisfied, but made no reply.

#436: Generalization of `append`, `map`, and `for-each` to other sequences.  The proposed `vector-append` and `bytevector-append` procedures were adopted by a vote of the WG.  Neither the editors nor any WG member chose to file a ballot ticket for the `bytevector-map` and `bytevector-for-each` procedures, so they were never voted on by the WG.  The commenter was asked if he is satisfied, but made no reply.

#438: Inconsistency of sequence copying procedures.  Adopted by a vote of the WG.  However, the suggestions to reorder the arguments of the destructive `*-copy!` procedures and to to rename various procedures were not considered, as neither the editors nor any member chose to file ballot tickets.  The commenter was asked if he is satisfied, but made no reply.

#440: `Write` procedure is not backwards compatible.  Rejected by a vote of the WG, which adopted three procedures `write-simple` (traditional `write`), `write` (with datum labels only to break cycles), and `write-shared` (datum labels to show all shared structure, the `write-with-shared-structure` of SRFI-38).  The commenter was asked if he is satisfied, but made no reply.

#456: Adoption of the standard [R6RS] ''was'' as widespread as had been hoped.  Treated as editorial.  Recognizing the delicate nature of the issue, the editors removed the language objected to, and replaced it by new and less contentious language.  The new language was also objected to.  The editors decided to go no further.  The commenter was asked if he is satisfied, but made no reply.

=== Commenter is satisfied ===

#360: Change syntax of escaped symbols from `|<symbol element>*|` to `#"<string element>*"`.  Rejected by the editors, who concluded that there was no precedent for it, that it conflicted with the Racket lexical syntax for byte strings, and that it was most unlikely to pass a vote.  The commenter is satisfied.

#477 (a Formal Objection) Memoization is not possible in portable R7RS [[due|to the seventh draft's definition of `eqv?`]].  Adopted by a vote of the WG; the editors used different language.  The objector is satisfied.

== Accepted or accepted in principle ==

#357: The epoch of `current-second` should be 1970-01-01 00:00:00 TAI.  Adopted by a vote of the WG; see [wiki:WG1Ballot5Results] for details.

#372: `(exit #t)` should be the same as `(exit)`.  Adopted by a vote of the WG.

#414: Internal syntax definitions vs a body with definitions being a letrec*.  Treated as editorial, and adopted by the editors.

#416: `lazy` is underspecified.  Treated as editorial, and adopted by the editors.

#420: Scope of `#!fold-case` and `#!no-fold-case` [[clarify|that it is lexical, not dynamic]].  Treated as editorial, and adopted by the editors.

#424: Add write-string procedure to `(scheme base)`.  Adopted by a vote of the WG.

#426: The descriptions of the dynamic features need to be clearer and more consistent.  Treated as editorial, and adopted for the most part by the editors.  A new section was not added as had been requested, but many changes in detail were made, including to the formal semantics.

#430: Library loading rules are unclear and overly permissive.  Adopted by a vote of the WG.

#431: The continuation used when 'guard' re-raises an exception isn't specified.  Treated as editorial, and adopted by the editors.

#439: Bidirectional ports and `port-open?`.  Adopted by a vote of the WG.

#453: The denotational semantics is inadequate.  Treated as editorial and substantially adopted by the editors.  The WG voted whether to remove the formal semantics altogether or attempt to repair it, and decided on the latter.

#455: The word "dynamic environment" is largely unspecified.  Treated as editorial and substantially adopted by the editors.

