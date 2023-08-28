(based loosely on SRFI 1)

Numbers are arbitrary.  P = primitive.  
sto = sequence type object  
43 generic procedures, 8 primitives

### Predicates

1P: sequence? sto obj -> boolean  
2: seq-empty? sto seq -> boolean  
3: seq=? stp elt= seq1 ... -> boolean

### Selectors

5P: seq-ref sto seq i -> value  
16: seq-take sto x i -> seq  
18: seq-drop sto x i -> object  
20: seq-take-right sto fseq i -> object  
22: seq-drop-right sto fseq i -> seq  
24: seq-split-at sto seq i -> [seq object]  
26: seq-last sto seq -> object  

### Miscellaneous: length, append, concatenate, reverse, zip & count

29P: seq-length sto seq -> integer  
30P: seq-append sto seq1 ... -> seq  
34: seq-reverse sto seq -> seq  
36: seq-append-reverse sto rev-head tail -> seq  
38: seq-zip sto cseq1 cseq2 ... -> seq  
44: seq-count sto pred cseq1 cseq2 -> integer

### Fold, unfold & map

46: seq-fold sto kons knil cseq1 cseq2 ... -> value  
47: seq-fold-right sto kons knil cseq1 cseq2 ... -> value  
27: seq-foldl sto kons knil cseq1 cseq2 ... -> value  
28: seq-foldr sto kons knil cseq1 cseq2 ... -> value  
48: seq-reduce sto proc ridentity seq -> value  
52P: seq-map sto proc cseq1 cseq2 ... -> seq  
54P: seq-for-each sto proc cseq1 cseq2 ... -> unspecified  
55: seq-append-map sto proc cseq1 cseq2 ... -> value  
57: seq-filter-map sto proc cseq1 cseq2 ... -> seq

### Filtering & partitioning

59P: seq-filter sto pred seq -> seq  
61: seq-partition sto pred seq -> [seq seq]  
63: seq-remove sto pred seq -> seq  

### Searching

66: seq-find sto pred cseq -> value  
67: seq-take-while sto pred cseq -> seq  
69: seq-drop-while sto pred cseq -> seq  
71: seq-span sto  pred cseq -> [seq cseq]  
73: seq-break sto pred cseq -> [seq cseq]  
75: seq-any sto pred cseq1 cseq2 ... -> value  
76: seq-every sto pred cseq1 cseq2 ... -> value  
77: seq-index sto pred cseq1 cseq2 ... -> integer or false

### Deletion

79: seq-delete sto comparator obj seq -> seq  
82: seq-delete-neighbor-dupes sto comparator seq -> seq  
81: seq-delete-duplicates sto comparator seq -> seq

### Elementwise mutators

97P: seq-set! sto seq i value -> unspecified  
98: seq-fill! sto seq fill [start [end]] -> unspecified  
53: seq-map! f sto seq1 seq2 ... -> unspecified

### Sequence type objects

100: sto? obj  
101: make-sto arg ...  
102: sto-ref

### Exceptions

103: sequence-error msg irritant ...  
104: sequence-error? obj  
105: sequence-message sequence-error  
106: sequence-irritants sequence-error

## Instances

Dictionaries: keys, values, associations
Lists  
Vectors  
Strings  
Bytevectors and other homogeneous vectors  
Ranges  
~~Texts~~  
Flexvectors  
Bitvectors  
Random-access lists  
Lazy sequences  
Streams  
Compound objects  
Doubly linked lists  
Bitwise integers?  
Arrays (with 1 dimension)
