(based on SRFI 1)

1: ## Predicates

2: seq-empty?  seq -> boolean  
3: seq=? elt= seq1 ... -> boolean

4: ## Selectors

5: seq-ref cseq i -> value  
6: seq-first   pair -> object  
7: seq-second  pair -> object  
8: seq-third   pair -> object  
9: seq-fourth  pair -> object  
10: seq-fifth   pair -> object  
11: seq-sixth   pair -> object  
12: seq-seventh pair -> object  
13: seq-eighth  pair -> object  
14: seq-ninth   pair -> object  
15: seq-tenth   pair -> object  
16: seq-take x i -> seq  
17: seq-drop x i -> object  
18: seq-take-right fseq i -> object  
19: seq-drop-right fseq i -> seq  
20: seq-take! x i -> seq  
21: seq-drop! x i -> seq  
22: seq-drop! fseq i -> seq  
23: seq-drop-right! fseq i -> seq  
24: split-at  x i -> [seq object]  
25: split-at! x i -> [seq object]  
26: seq-last pair -> object  
27: seq-last-pair pair -> pair

28: ## Miscellaneous: length, append, concatenate, reverse, zip & count

29: seq-length  seq -> integer  
30: seq-append  seq1 ... -> seq  
31: seq-append! seq1 ... -> seq  
32: seq-concatenate  seq-of-seqs -> value  
33: seq-concatenate! seq-of-seqs -> value  
34: seq-reverse  seq -> seq  
35: seq-reverse! seq -> seq  
36: seq-append-reverse  rev-head tail -> seq  
37: seq-append-reverse! rev-head tail -> seq  
38: seq-zip cseq1 cseq2 ... -> seq  
39: seq-unzip1 seq -> seq  
40: seq-unzip2 seq -> [seq seq]  
41: seq-unzip3 seq -> [seq seq seq]  
42: seq-unzip4 seq -> [seq seq seq seq]  
43: seq-unzip5 seq -> [seq seq seq seq seq]  
44: seq-count pred cseq1 cseq2 -> integer

45: ## Fold, unfold & map

46: seq-fold kons knil cseq1 cseq2 ... -> value  
47: seq-fold-right kons knil cseq1 cseq2 ... -> value  
48: seq-reduce f ridentity seq -> value  
49: seq-reduce-right f ridentity seq -> value  
50: seq-unfold p f g seed [tail-gen] -> seq  
51: seq-unfold-right p f g seed [tail] -> seq  
52: seq-map proc cseq1 cseq2 ... -> seq  
53: seq-map! f seq1 cseq2 ... -> seq  
54: seq-for-each proc cseq1 cseq2 ... -> unspecified  
55: seq-append-map  f cseq1 cseq2 ... -> value  
56: seq-append-map! f cseq1 cseq2 ... -> value  
57: seq-filter-map f cseq1 cseq2 ... -> seq

58: ## Filtering & partitioning

59: seq-filter pred seq -> seq  
60: seq-filter!    pred seq -> seq  
61: seq-partition pred seq -> [seq seq]  
62: seq-partition! pred seq -> [seq seq]  
63: seq-remove pred seq -> seq  
64: seq-remove!    pred seq -> seq

65: ## Searching

66: seq-find pred cseq -> value  
67: seq-take-while  pred cseq -> seq  
68: seq-take-while! pred cseq -> seq  
69: seq-drop-while pred cseq -> seq  
70: seq-drop-while! pred cseq -> seq  
71: span   pred cseq -> [seq cseq]  
72: span!  pred seq  -> [seq seq]  
73: seq-break  pred cseq -> [seq cseq]  
74: seq-break! pred seq  -> [seq seq]  
75: seq-any pred cseq1 cseq2 ... -> value  
76: seq-every pred cseq1 cseq2 ... -> value  
77: seq-index pred cseq1 cseq2 ... -> integer or false

78: ## Deletion

79: seq-delete  x seq [=] -> seq  
80: seq-delete! x seq [=] -> seq  
81: seq-delete-duplicates  seq [=] -> seq  
82: seq-delete-duplicates! seq [=] -> seq

83: ## Set operations on seqs

84: seqset<= = seq1 ... -> boolean  
85: seqset= = seq1 seq2 ... -> boolean  
86: seqset-adjoin = seq elt1 ... -> seq  
87: seqset-union = seq1 ... -> seq  
88: seqset-union!             = seq1 ... -> seq  
89: seqset-intersection = seq1 seq2 ... -> seq  
90: seqset-intersection!      = seq1 seq2 ... -> seq  
91: seqset-difference = seq1 seq2 ... -> seq  
92: seqset-difference!        = seq1 seq2 ... -> seq  
93: seqset-xor = seq1 ... -> seq  
94: seqset-xor!               = seq1 ... -> seq  
95: seqset-diff+intersection = seq1 seq2 ... -> [seq seq]  
96: seqset-diff+intersection! = seq1 seq2 ... -> [seq seq]  
