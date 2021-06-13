# Charter for working group 2

This charter has been approved by the Scheme Steering Committee, though it is not yet up at scheme-reports.org.
New paragraphs relative to the version there
are marked as such.  Having a specific timeline proved to be
unhelpful in WG1 and has been removed.  The section on coordination
with WG1 has been stripped down to a statement of compatibility.
The membership and internal process sections have been rewritten.

Because SRFI processes are open to all members of the Scheme
community, the final community vote has been removed from the process.

## Purpose

Working group 2 will develop specifications, documents, and
proofs of practical implementability for a language that embodies
the essential character of Scheme, that is large enough to address
the practical needs of mainstream software development, and that
can be extended and integrated with other systems.

The purpose of this work is to facilitate sharing of Scheme code.
One goal is to be able to reuse code written in one conforming
implementation in another conforming implementation with as little
change as possible. Another goal is for users of this work to be
able to understand each other's code based on a shared and
unambiguous interpretation of its meaning.

The language is not necessarily intended for educational, research,
or embedded use, though such uses are not prohibited.  Therefore,
it may be a "heavyweight" language compared to the language
designed by working group 1.

## Requirements and Goals

To promote extensibility, the language developed by working group 2
must include support for macros and modules in a way that is
appropriate for the language's size and goals.

When deciding which features to include in the language, working
group 2 should consider all features provided by R6RS Scheme, and
all criticisms of those features.  Insofar as practical, the
language should be backwards compatible with an appropriate
subset of the R6RS standard.

Working group 2 may also consider whether it is practical to add
new features to support networking, threads, internationalization,
foreign-function interfaces, et cetera. Working group 2 is
encouraged to propose new working groups for any new features
that it considers desirable but cannot pursue without risking
the timeliness of its primary mission.

Self consistency is an important objective, which may require
adding new features.

## Deliverable Artifacts

Working group 2 must develop written specifications for the
language. These specifications must be accompanied by concise
statements of all formal comments and objections that have been
raised by members of the working group or by the Scheme community
at large. The working group should also provide a written design
rationale, executable reference implementations, test suites, and
other artifacts that would assist with constructive debate or
increase acceptance of the language.

## Compatibility with R7RS-small

Every implementation of the specifications produced by working group 2
must be an implementation of the specifications produced by working
group 1. Every program that conforms to the specifications produced
by working group 1 (and relies on no features beyond those guaranteed
by those specifications) must also be a program that conforms to the
specifications produced by working group 2.


## Membership

**New:**
Any member of the Scheme community may become a member of the working
group with the consent of the chair, which consent must not be unreasonably
withheld.  Membership decisions made by the chair may be appealed
to the Steering Committee.  It is expected that new members will join
throughout the life of the working group.

Members of the working group should endorse the goals of the
working group and be willing and able to work toward consensus.
Working groups 1 and 2 will have some members in common.

## Publicity

All technical discussions must be made public. This requirement
can be satisfied by timely posting of email and the technical
minutes of meetings at a public web site, and by maintaining
a publicly readable mailing list devoted to working group 2's
technical discussions.

## Internal Decision Making Process

The chair of the working group is expected to develop an
internal process that allows the working group to achieve
its objectives.

**New:**
Insofar as practical, the working group should leverage the
existing [SRFI process](https://srfi.schemers.org/srfi-process.html)
to develop the standard.  Any member of the working
group can propose that a SRFI, existing or to be created, should
become part of the language.  It is expected that many existing
and new SRFIs will be added to the language in this fashion.
Sections of the R6RS standards may be treated as SRFIs for this
purpose.

**New:**
If the working group agrees, the SRFI as a whole will become
an optional part of the language, usually constituting one or more
libraries.  If the chair believes a proposal is sufficiently
uncontroversial, the SRFI may be added to the language without
consulting the working group.  Any such action by the
chair may be overridden at the request of any member, in which
case the working group must vote on the proposal.  In
appropriate circumstances, the working group may decide
explicitly to make a SRFI a required part of the language.

The working group is expected to strive for consensus on all
decisions. Where consensus cannot be achieved, the working
group may proceed on the basis of a vote, but the results of
such votes must be preserved within the public record, along
with the reasons for dissent.

**New:**
When the working group votes yes or no on a proposal, a simple
majority of the legal votes cast (ignoring abstentions) shall
determine the vote.  A process similar to the ranked-pairs
voting used by working group 1 may be used in the case of multiple
competing proposals.

## Endorsement Process

The work products developed by working group 2 will be submitted
to the Steering Committee for endorsement. The Steering Committee
will work with working group 2 to seek maximum possible timely
consensus on the work products. In considering whether to endorse
the work products, the Steering Committee will consider whether
the work products meet the charter requirements, as well as the
level of support that they enjoy.

**New:**
Given the modular nature of the standard, the working group is
encouraged to submit work products for endorsement on a rolling basis
rather than waiting for the effort to be complete.

If the Steering Committee believes that support could be increased
by revising work products in response to specific objections, then
it may request another draft/review cycle of the working group.

## Officers

The Steering Committee selects, and may replace, the working group's chair.

The working group may elect or appoint other officers as it sees fit.

## Termination

**New:**
The Steering Committee may terminate the working group on request of the
chair or on its own motion at any time.

