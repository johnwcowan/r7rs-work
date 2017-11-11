My name is Benjamin L. Russell, and I currently work as a patent
translator in Tokyo, specializing in computer science- and IT-related
specifications.  I majored in computer science in college, specializing
in algorithms, and maintain a programming language theory blog,
["Monadically Speaking:  Adventures in PLT Wonderland"](http://dekudekuplex.wordpress.com/) ("PLT" referring
to "Programming Language Theory," not to be confused with the PLT
Research Group) (also duplicated
as a [WordPress.org private blog](http://dekudekuplex.sakura.ne.jp/blog/en/)).

As a graduate of Yale University with a Bachelor of Science in Computer
Science, I have been a student of Scheme (originally, of T) since 1990.
Since then, I have been an active participant in various Scheme
discussion groups, including comp.lang.scheme, gmane.lisp.scheme.plt,
and gmane.lisp.scheme.gauche.

Although professionally, I also work as a patent translator at
Shobayashi International Patent & Trademark Office, personally, I am
actually against software patents, because I believe that they
can potentially inhibit free software (free as in "intellectually free,"
as opposed to "free beer").

Having read through many of the [opinions](http://www.r6rs.org/ratification/results.html) both for and against the
ratification of draft 5.97 as R6RS, I am convinced that the
rushed ratification of R6RS was a mistake, and that more time should
have been given to adopting a less controversial version of the draft.
In particular, I feel that R6RS violates the spirit of Scheme, as
embodied in the first sentence of every recent Scheme Report as follows:
[#BR]]
> > Programming languages should be designed not by piling feature on top [#BR]]
> > of feature, but by removing the weaknesses and restrictions that make [#BR]]
> > additional features appear necessary." [#BR]]

Specifically, I believe that the following features of R6RS go against
this spirit:

* case sensitivity

> Scheme has never been a case-sensitive language, and case
> sensitivity is a fundamental change.  Case-sensitivity causes the
> language to be more difficult to learn for beginners, especially
> children, and interferes with the role of Scheme as a pedagogical
> language for computer science.

* the size of the standard library

> While each individual library is useful, the sum of all the standard
> libraries put together is too much.  For certain other programming
> languages, such as PERL, there is a common repository of libraries
> available, coupled with a relatively small core language.  Requiring
> such a large set of libraries as part of the standard has at least
> two major disadvantages:

1) Any implementation conforming with R6RS much devote many resources to implementing those libraries, discouraging small organizations and vendors from creating an R6RS-compliant Scheme.

2) So many standard libraries render R6RS Scheme impractical for most current embedded systems, which are limited in available memory and processing power.  Since Scheme is often used as a scripting language, a small collection of standard libraries would encourage the use of Scheme for developing embedded systems; the set of standard libraries in R6RS Scheme, on the other hand, discourages such development.  Instead, Chris Hanson proposed an [alternative](http://www.r6rs.org/ratification/results.html#X78) to the current module system, as follows:

> > Rather than invent a new language to do this, it's possible to build [#BR]]
> > the linker such that the programmer can insert code to control aspects [#BR]]
> > of the linking process.  Such code could do _more_ than the current [#BR]]
> > module system does, and because of the added expressivity, it could do [#BR]]
> > so more concisely in many cases.  For example, I sometimes use a [#BR]]
> > trivial module system in which names starting with "%" are local, and [#BR]]
> > all other names are exported.  This is a trivial program to write, and [#BR]]
> > doesn't depend on the details of the names, but it can't be said at all [#BR]]
> > with the proposed module system.[#BR]]

Hanson's alternative is, I believe, much more in keeping with the spirit
of Scheme than the current module system, since it allows writing more
concise code that simultaneously allows a finer degree of control.

Recently, MIT has ceased using Scheme in its introductory 6.001 computer
science course, and has chosen to replace Scheme with Python.  Further,
Scheme has recently been criticized for not being a language capable of
coping with the modern style of software development, which focuses on
using teams of developers to use and modify existing code without fully
understanding how the existing code works, rather than having individual
developers creatively write code from scratch.

Furthermore, Scheme has been criticized for not having features demanded
by most modern languages in order to support such needs as developing
Web applications, networking, and graphical user interfaces.  Most
modern applications require some sort of interaction with the Web, which
requires communication with the external environment.  Since Scheme's
strengths lie in continuations and syntactic abstraction, rather than in
interaction with the external environment, in order for Scheme to
survive as Scheme, the language must simultaneously move in two opposite
directions:

1) Scheme must provide certain features demanded in most modern
> programming languages; specifically, some sort of package repository
> containing libraries useful for writing Web applications, networking
> tools, and applications that provide graphical user interface
> elements should be provided, and the standard should provide a basis
> for such a repository.

2) At the same time, Scheme must continue to adhere to the original
> spirit of Scheme, by avoiding "piling feature upon feature," and
> instead by "removing the weaknesses and restrictions that make
> additional features appear necessary."

While at first sight, these appear to be conflicting goals, I believe
that they can be reconciled.  What is needed is a small language that
nevertheless has a large common repository full of libraries useful for
developing Web applications, networking applications, applications with
graphical user interface elements, robotics, and the like; which
encourages individual Scheme developers to be creative in using such
features as syntactic abstraction and continuations within their
programs, while still mandating a certain style of interfacing with
other Scheme programs; and which is conducive to allowing teams of
programmers to share code without needing to understand all the details
of the behavior within, while still allowing individual programmers to
be creative in using syntactic abstractions and manipulating control
flow within.

For R7RS, I believe that some of the design goals of the Scheme
programming language should be the following:

> A) Find a way to overcome what such schools as MIT apparently
> currently see as weaknesses in Scheme vis–à–vis such other
> programming languages as Python, by supporting such needs as Web
> application development, networking, GUI support, and robotics,
> without bloating the language with a huge module system.  Build the
> linker such that the programmer can insert code to control aspects
> of the linking process.  Provide a common repository of packages for
> Scheme, similar to that currently available for Python.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;B) Find a way of reconciling the conflicting goals of encouraging programmer collaboration and encouraging creativity.  One of the reasons that MIT dropped Scheme for its introductory <BR>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;course in computer science was that Scheme did not provide any means for supporting software development by teams of programmers.  According to the <a href="http://www.cs.berkeley.edu/~bh/ssch0/preface.html">Preface</a> of _Simply Scheme: <BR>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Introducing Computer Science, Second Edition_ <a href="http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=3662">&#91;1&#93;</a>, by Brian Harvey and Matthew Wright:  <BR>

> > There are two schools of thought about teaching computer science.[#BR]]
> > We might caricature the two views this way:[#BR]]
> > [#BR]]
> > •	    	The conservative view: Computer programs have become too[#BR]]
> > large and complex to encompass in a human mind. Therefore, the job[#BR]]
> > of computer science education is to teach people how to discipline[#BR]]
> > their work in such a way that 500 mediocre programmers can join[#BR]]
> > together and produce a program that correctly meets its[#BR]]
> > specification.[#BR]]
> > [#BR]]
> > •	    	The radical view: Computer programs have become too[#BR]]
> > large and complex to encompass in a human mind. Therefore, the job[#BR]]
> > of computer science education is to teach people how to expand[#BR]]
> > their minds so that the programs can fit, by learning to think in[#BR]]
> > a vocabulary of larger, more powerful, more flexible ideas than[#BR]]
> > the obvious ones. Each unit of programming thought must have a big[#BR]]
> > payoff in the capabilities of the program.[#BR]]

> I believe that, historically, Scheme has represented "the radical
> view," while Python, at least as it is currently used in teaching
> computer science at MIT, now represents "the conservative view"
> (replacing Pascal in its historical role).

> While personally in favor of the former school of thought, I believe
> that some provision must also be made for support for the latter in
> order to provide a viable alternative to Python as a tool for
> teaching introductory computer science courses at such schools as
> MIT, yet without sacrificing support for the former.

> To this end, I belive that students should be taught to be creative
> without re-inventing the wheel.  One of the main problems with the
> latter school of thought, in the manner described by Harvey and
> Wright, is that it encourages inefficiency and silliness.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;For example, consider the following examples of code defining the square function (quoted from <a href="http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=3662">&#91;1&#93;</a>):  

> [Scheme:](In)

> {{{(define (square num)}}}[#BR]]
> {{{(* num num))}}}[#BR]]

> [Pascal:](In)

> {{{function [SquareOfWholeNumber](SquareOfWholeNumber.md)(num: integer): integer;}}}[#BR]]
> {{{begin}}}[#BR]]
> {{{SquareOfWholeNumber := num * num}}}[#BR]]
> {{{end;}}}[#BR]]
> {{{function [SquareOfDecimalNumber](SquareOfDecimalNumber.md)(num: real): real;}}}[#BR]]
> {{{begin}}}[#BR]]
> {{{SquareOfDecimalNumber := num * num}}}[#BR]]
> {{{end;}}}[#BR]]

> In Scheme, when we want to tell the computer to multiply a number by
> itself, we just tell it to do so.

> By contrast, in Pascal, when we want to tell the computer to
> multiply a number by itself, we first need to tell it how to do so
> for one type of number (integers), and then tell it how to do so for
> another type of number (reals).  The reason for this distinction,
> according to Harvey and Wright, is that proponents of the
> "500-mediocre-programmer school" (as Harvey and Wright term it) are:

> > afraid that you might write the square program with whole numbers [#BR]]
> > in mind, and then apply it to a decimal fraction by mistake. If [#BR]]
> > you're on a team with 499 other programmers, it's easy to have [#BR]]
> > failures of communication so that one programmer uses another's [#BR]]
> > program in unintended ways." [#BR]]

> However, this style of development stifles creativity and encourages
> inefficiency.  It's hard to be creative when forced to think in the
> same way as every other programmer.  This is silly.

> One solution is to alleviate the need for Scheme programmers to
> reinvent the wheel by providing means to develop a rich repository
> of packages common to all implementations of Scheme.

> One of the problems with the current state of Scheme is that while
> each implementation has its own advantages, each implementation also
> has disadvantages sufficient enough to discourage widespread use,
> and implementations cannot be combined easily.  This state of
> affairs needs to be overcome if Scheme is to prosper as a language.
> While I'm not advocating establishing a reference implementation, I
> do believe that a much wider set of cross-implementation features
> than is currently available should be established.  Instead of
> creating many implementation-specific features, Scheme programmers
> should be able to create many cross-implementation features.

> In order for this to happen, a suitable framework needs to be
> developed without bloating the language.  I believe that the
> combination of a linker with a rich cross-implementation repository
> of packages could help to address this need.

> C) Remain true to the original spirit of Scheme, by avoiding "piling
> feature upon feature," and instead by "removing the weaknesses and
> restrictions that make additional features appear necessary."
> Specifically, features of R6RS that either inhibit its use by
> beginners (such as case-sensitivity) or bloat the language (the
> module system) should be reduced and, if necessary, replaced by more
> elegant features (such as a linker).

One controversial issue, which I didn't mention in my original statement
in volunteering for working group 1, but which I am interested in
covering, is that of Unicode.  As a translator, I work with many
bilingual Japanese/English documents, and for most people whose native
language is not English, lack of Unicode is at least somewhat
detrimental to reading code.  One reason that [Gauche Scheme](http://practical-scheme.net/gauche/index.html), an implementation of
R5RS Scheme with Unicode support, is one of the most popular
implementations of Scheme in Japan is that it supports Unicode.

For example, there is an online textbook for Gauche Scheme, entitled
["Karetta | Gauche Programming (Browsing Version)"](http://karetta.jp/book-cover/gauche-hacks) (in Japanese), in which one
section, entitled "Generate HTML Easily," describes writing a script to
use the text.html-lite library to output the first three days of the
week in Japanese.  It turns out that the first three days of the week in
Japanese are not represented by "Sunday, "Monday," and "Tuesday," but by
the corresponding Japanese Kanji characters ("日" ("Nichi"), "月"
("Getsu"), and "火" ("Ka")).  Here is the commented code for the listed
function:
[#BR]]
> > (html:tr (map html:td (list "日" "月" "火"))) ;; html:tdをmapでリストに適用

This code performs a useful function, and requires Unicode support in
order to be useful.  The comment reads "apply html:td with map to the
list" in Japanese.  Notice that the comment also requires Unicode.
Without Unicode, neither the days of the week nor the comment could not
be written in Japanese, thus alienating beginning native Scheme
programmers interested in referring to the days of the week in the
vernacular who also need a line-by-line commentary, preferably in-line,
in a language that they can understand.

The same situation applies to almost any other alphabet which contains
characters not representable in ASCII:  Hebrew, Latin, Cyrillic, Greek,
Georgian, Armenian, Ethiopic, Arabic, Devanagari, Bengali, Gurmukhi,
Gujarati, Oriya, Tamil, Telugu, Kannada, Malayalam, Sinhala, Myanmar,
Khmer, Thai, Lao, Tibetan, Chinese, Hangul, and Mongolian, to name a few
examples (see [#http://homepage2.nifty.com/PAF00305/lib/unicode.html|"Unicode Test Page ♦ ユニコード テスト ページ [Test Page]]"](Unicode)).

Therefore, I believe that support for Unicode does not constitute an
instance of "piling feature upon feature," but provides functionality
necessary for internationalization.

I hope to contribute to the success of working group 1 by helping to
guide discussion, and by providing documentation of any features agreed
to by the group consensus.  This documentation could then be used as a
basis by working group 2.

-- Benjamin L. Russell

[1] Harvey, Brian and Matthew Wright.  _Simply Scheme: Introducing
Computer Science, Second Edition._ Cambridge, MA: The MIT Press,
1999. <http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=3662>.
