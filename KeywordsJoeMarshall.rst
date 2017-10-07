[[Joe|Marshall sent this to me in private mail, and I am posting it here with his consent.]]

My personal preference is the leading colon. There are several sound
(but minor) technical arguments for this.

The first (and best) argument is that nearly every token in Scheme can be
discriminated by the leftmost character or character pair. Even aggregate
objects like lists and vectors can be discriminated by looking looking at the
leftmost character.  The syntax of the language was modified to fit a left
to right parsing scheme:  Scheme traditionally used the "if it's not a number
it's a symbol" rule, but in R3RS (I think), they changed it to the more common
`begins with a letter' rule and added exceptions for "-1+" and "1+".  This
change was to allow for deterministic left-to-right parsing.  If you've written
a few parsers, you begin to realize how wonderful it is to be able to select
the right parsing rule without having to consider various magic characters
strewn all over the input stream (especially the characters you haven't
received yet!)  (Come to think of it, Unicode has this problem because
there is no end-of-char marker.  You cannot determine if you have read an
entire character [[base|+ diacritics]] until you at least see the beginning of the next.)  Putting
a discrimination marker at the trailing end of a token just throws this entire
property out the window.

I believe this to be a sound, compelling argument.  However, I think it is a
''minor'' one.  It will complicate parsing, but not by an awful lot.  I could see
arguments in favor of trailing colons trumping this, but I can see no
''technical''
one, just an aesthetic one.

The second ''technical'' argument is that as a long-time programmer, I find it
easier to scan and read code if I can easily determine the syntactic class of
the token.  (Well, let's face it, your brain is parsing the code and
unidirectional
parsing is easier whether it is your brain or your computer.)  Again,
minor point.

In the examples given for trailing colon, they look cool:

{{{
(tcltk-frame relief: 'ridge
                borderwidth: 4
                height: "50px"
                width: "100px")
}}}

But let's take a real world example (from some code I have written in
Common Lisp)
first with trailing colons:

{{{
(call-with-repository-transaction
   repository: repository
   transaction-type:  (vm-transaction-type->core-transaction-type transaction-type)
   user-id-specifier: user-id-specifier
   reason: reason
   meta-cid-set-specifier: metaversion-cid-set-specifier
   cid-set-specifier: (lambda ()
                        (compute-cid-set-specifier repository
                                                   metaversion-cid-set-specifier
                                                   version-specifier
                                                   vpb-cid-dids-to-add
                                                   vpb-cid-dids-to-remove))

   aux-meta-cid-set-specifier: aux-metaversion-cid-set-specifier
   aux-cid-set-specifier: (lambda ()
                            (and aux-metaversion
                                 (compute-cid-set-specifier repository
                                                            aux-metaversion-cid-set-specifier
                                                            aux-version-specifier
                                                            aux-vpb-cid-dids-to-add
                                                            aux-vpb-cid-dids-to-remove)))
   receiver: (lambda (core-txn)
               (let ((vm-txn (make-instance 'vm-transaction
                                            underlying-transaction: core-txn)))
                 (case transaction-type
                   ((read-write: read-cons:) (call-creating-change-set vm-txn
change-set-type receiver))
                   (t (funcall receiver vm-txn))))))
}}}

and again with leading colons:

{{{
(call-with-repository-transaction
   :repository repository
   :transaction-type  (vm-transaction-type->core-transaction-type transaction-type)
   :user-id-specifier user-id-specifier
   :reason reason
   :meta-cid-set-specifier metaversion-cid-set-specifier
   :cid-set-specifier (lambda ()
                        (compute-cid-set-specifier repository
                                                   metaversion-cid-set-specifier
                                                   version-specifier
                                                   vpb-cid-dids-to-add
                                                   vpb-cid-dids-to-remove))

   :aux-meta-cid-set-specifier aux-metaversion-cid-set-specifier
   :aux-cid-set-specifier (lambda ()
                            (and aux-metaversion
                                 (compute-cid-set-specifier repository
                                                            aux-metaversion-cid-set-specifier
                                                            aux-version-specifier
                                                            aux-vpb-cid-dids-to-add
                                                            aux-vpb-cid-dids-to-remove)))
   :receiver (lambda (core-txn)
               (let ((vm-txn (make-instance 'vm-transaction
                                            :underlying-transaction core-txn)))
                 (case transaction-type
                   ((:read-write :read-cons) (call-creating-change-set vm-txn change-set-type receiver))
                   (t (funcall receiver vm-txn))))))
}}}

The keyword/value pairs are important as a unit.  In the first
example, the colon
indicating the keyword object is embedded somewhere in the middle of
the pair.  In the second, it is the ''very first'' non-whitespace on the
line.  So as a human reader
of the code I see ''right away'' that I have a column of keywords on the
left.  As I'm parsing this in my mind, the leading colon is doing
double duty.  It marks that the next token is a keyword, but it ''also''
marks that we are at the beginning of a keyword/value pair.  That neat
little vertical dotted line that is made up of the
colons tells me that we're going to call something with a complex API.

This is similar in effect to `bulleted' or numbered lists.  Compare this:
{{{
   Professor Fassbinder and his daughter have been kidnapped. - 1
   Someone has kidnapped them. - 2
   My hand is on fire. - 3
}}}

To this:
   1. Lather
   1. Rinse
   1. Repeat

To sum it all up, the argument for trailing colons is simply that is jarring, at
first glance, to see the colon at the front end because we've been used
to seeing it at the other end.  In light of the fact that we're using lisp, that
is not a very persuasive argument.  The technical arguments are quite minor,
admittedly, but I prefer them to the "inconvenience" of weird-looking keywords.
(and you can get used to the looks, it ''never'' gets easier to parse over time).


In any case, ''having'' keywords is far more important than what they
look like.  It would be odd to have a standard that says "you should support
keywords, but whether you support leading or trailing colons is implementation
dependent".  But that might actually be worth doing.  All major implementations
support keywords, so they ought to be mentioned as a "standard" semantic
object, it is just the syntax that differs.  At least standardize the semantics.