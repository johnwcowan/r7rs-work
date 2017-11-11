
# From SRFI-57

The traditional practice of instantiating record values with a positional constructor procedure can lead to code that is hard to read and fragile under common operations such as adding, removing, or rearranging field declarations.


----

# From: "Michael R. Blair"
[Reference](http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1989/msg00204.html)

I am concerned that the current  proposal makes it difficult for me to treat the order of slot-names in a record as irrelevant. About  record constructor:  What I  want is  a  construction procedure wherein (arg) order is  considered irrelevent.

