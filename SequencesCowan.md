(based on SRFI 1)

### Predicates

1: seq? obj -> boolean
2: seq-empty?  seq -> boolean  
3: seq=? elt= seq1 ... -> boolean

### Selectors

5: seq-ref seq i -> value   
16: seq-take x i -> seq  
18: seq-drop x i -> object  
20: seq-take-right fseq i -> object  
22: seq-drop-right fseq i -> seq  
24: seq-split-at  x i -> [seq object]  
26: seq-last seq -> object  

### Miscellaneous: length, append, concatenate, reverse, zip & count

29: seq-length  seq -> integer  
30: seq-append  seq1 ... -> seq  
32: seq-concatenate  seq-of-seqs -> value  
34: seq-reverse  seq -> seq  
36: seq-append-reverse  rev-head tail -> seq  
38: seq-zip cseq1 cseq2 ... -> seq  
39: seq-unzip1 seq -> seq  
40: seq-unzip2 seq -> [seq seq]  
41: seq-unzip3 seq -> [seq seq seq]  
42: seq-unzip4 seq -> [seq seq seq seq]  
43: seq-unzip5 seq -> [seq seq seq seq seq]  
44: seq-count pred cseq1 cseq2 -> integer

### Fold, unfold & map

46: seq-fold kons knil cseq1 cseq2 ... -> value  
47: seq-fold-right kons knil cseq1 cseq2 ... -> value  
48: seq-reduce f ridentity seq -> value  
49: seq-reduce-right f ridentity seq -> value  
50: seq-unfold p f g seed [tail-gen] -> seq  
51: seq-unfold-right p f g seed [tail] -> seq  
52: seq-map proc cseq1 cseq2 ... -> seq  
54: seq-for-each proc cseq1 cseq2 ... -> unspecified  
55: seq-append-map  f cseq1 cseq2 ... -> value  
57: seq-filter-map f cseq1 cseq2 ... -> seq

### Filtering & partitioning

59: seq-filter pred seq -> seq  
61: seq-partition pred seq -> [seq seq]  
63: seq-remove pred seq -> seq  

### Searching

66: seq-find pred cseq -> value  
67: seq-take-while  pred cseq -> seq  
69: seq-drop-while pred cseq -> seq  
71: seq-span   pred cseq -> [seq cseq]  
73: seq-break  pred cseq -> [seq cseq]  
75: seq-any pred cseq1 cseq2 ... -> value  
76: seq-every pred cseq1 cseq2 ... -> value  
77: seq-index pred cseq1 cseq2 ... -> integer or false

### Deletion

79: seq-delete  x seq [=] -> seq  
81: seq-delete-duplicates  seq [=] -> seq  

### Elementwise mutators

97: seq-set! seq i value -> unspecified
98: seq-fill! seq fill [start [end]] -> unspecified
53: seq-map! f seq1 seq2 ... -> unspecified  

## Instances

Lists  
Vectors  
Strings  
Bytevectors and other homogeneous vectors  
Ranges  
Texts  
Flexvectors  
Bitvectors  
Random-access lists  
Lazy sequences  
Streams  
Compound objects  
Doubly linked lists  
Bitwise integers?  
Generators?  
Arrays?
