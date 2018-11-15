Doubly linked list procs accept ordinary conses too; cpr operations throw errors.

The empty doubly linked list is plain ().

All procedures containing "list" are changed to "dlist", like "proper-dlist".
Other procedures are prefixed with "d", like "dcons, dcar" etc.
The mark % indicates that a -reversed also exists.

Dlists can be dotted at either end or both.
Need constructor for doubly dotted dlists.

This is SRFI 1, not yet filtered.  Consing requires that the cpr/cdr of the cdr/cpr be changed.
Figure out if this actually works or if we have to prohibit consing.

##Constructors
cons% list%
xcons% cons*% make-list list-tabulate 
list-copy circular-list iota
##Predicates
pair? null?
proper-list?% circular-list? dotted-list?%
not-pair? null-list?
list=
##Selectors
car cdr cpr 
... cppadr cppppr list-ref%
first second third fourth fifth sixth seventh eighth ninth tenth (all %)
car+cdr+cpr
take%       drop%
take-right% drop-right%
take!%      drop-right!% 
split-at%   split-at!% 
last% last-pair%
##Miscellaneous: length, append, concatenate, reverse, zip & count
length length+
append%  concatenate%  reverse%
append!% concatenate!% reverse!%
append-reverse% append-reverse!%
zip unzip1 unzip2 unzip3 unzip4 unzip5 (all %)
count
##Fold, unfold & map
map for-each
fold%       unfold%       pair-fold%       reduce% 
fold-right unfold-right pair-fold-right reduce-right (all %?)
append-map% append-map!%
map!% pair-for-each% filter-map% map-in-order%
##Filtering & partitioning
filter  partition  remove
filter! partition! remove! 
##Searching (all %)
member memq memv
find find-tail 
any every
list-index
take-while drop-while take-while!
span break span! break!
##Deleting (all %)
delete  delete-duplicates 
delete! delete-duplicates!
##Association lists (all %)
assoc assq assv
alist-cons alist-copy
alist-delete alist-delete!
##Set operations on lists
lset<= lset= lset-adjoin
lset-union			lset-union!
lset-intersection		lset-intersection!
lset-difference		        lset-difference!
lset-xor			lset-xor!
lset-diff+intersection	        lset-diff+intersection!
##Primitive side-effects
set-car! set-cdr! set-cpr!
