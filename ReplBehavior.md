From: "Øivind Binde"[[BR]]
To: John Cowan[[BR]]
Date: Sun, 14 Mar 2010 13:28:29 +0100[[BR]]
Subject: Standardize the Scheme REPL behavior and interface

First off, sorry for not introducing myself more specifically. But very short: I'm a Comp. Sci. student, and I'm primarily concerned with the end user experience I experience with most Scheme implementations. And thus I would like for Scheme to have standard on how it behaves in the repl-mode.

One can ask:[[BR]]
I.  What does the REPL interface got to do with the language standardization process?[[BR]]
II. Why do so many new Schemers prefer getting started with Dr.Scheme?

From a personal point of view, the easy to use, and getting up and running very fast (without clutter), was the main reason for choosing Dr.Scheme as my first editing environment. The cluttered editing within the REPL that Chicken (an implementation) gave me was enough to start looking for other alternatives.

So -- is there anything the other implementations can learn from Dr.Scheme?

Next question: Do you have any idea how it is for a total newbie to get started with <insert Scheme repl> and a emacs-bundle?  I found it so painful, that my hypothesis are simply put: emacs & co. are hindering Scheme adoption!

Abstract solution:

1) Provide a unified (standard) user experience at the REPL. Example: Launch it by passing along a argument flag:

mzscheme --r7rs-repl[[BR]]
or[[BR]]
csi --r7rs-repl

And they will behave the same across different implementation (!)

2) Get rid of the "social" stigma that new Schemers face when touching Lisp ground, which is: get <insert emacs bundle>. Do this by simply providing the basic editing within the REPL. And when advanced editing is required, use your favorite editor by just typing at the REPL >edit my-function-name  <edit, save, quit and then takes you back to REPL with an updated name-space>.

From my point of view, the basic end user experience within the REPL should be something along the lines of:

I)  Move around in the REPL by using the arrow-keys, without having to jump through hoops, by re-compiling etc.. today, you can use rlwrap -- if you are aware of its existence that is. (which most new users are not).

II)  Automatic indenting to happen in the REPL by default.

III)  I want to look up the name-space within the REPL, like I can do in ipython. say for instance: >(merge-sort)?? -> returns source code of the function merge-sort.

IV)  I want to be able to edit the name-space. Like: edit merge-sort -> which then takes in me into the operating systems default text-editor and allows be to edit the function.

Maybe someone thinks specifying the REPL behavior is a too far a stretch, and irrelevant to the language standard it self. But I think there might be worth considering this for providing some consistency between the different REPLs out there.  I think its important to have some consistency between the REPLs -- and ultimately I think we can get rid of the 'must learn a emacs/development environment' to play around with Scheme.

# Comments =

Aaron W. Hsu comments:

> These are some good points, but I have two major comments here.
* Most of these are about program and implementation level interface standardization, which I think is beyond the scope of either WG.
* I am strongly opposed to general attitude in favor of excessive "modes" in Schemes. I don't want to have to remember a flag every time I want standard Scheme. Rather, I want standard Scheme, and if I want the extensions, I'll add a flag then. IMO, we are defeating the idea of standards if we automatically presume that, in effect, no Scheme implementation is going to actually use the standard by default.
