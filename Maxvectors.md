A maxvector is a vector with a current length and a maximum length.

## Constructors
(make-maxvector vec current-length)  
(maxvector-append vec ...)  
(maxvector-append-subvectors [vec start end] ...)

## Predicates
(maxvector? obj)  
(maxvector-empty? vec)  
(maxvector= maxvec ...)

## Selectors
(maxvector-ref vec i)  
(maxvector-vector vec)  
(maxvector-max-length vec)  
(maxvector-current-length vec)

## Iteration
(maxvector-fold kons knil vec1 vec2 ...)  
(maxvector-fold-right kons knil vec1 vec2 ...)  
(maxvector-map f vec1 vec2 ...)  
(maxvector-map! f vec1 vec2 ...)  
(maxvector-for-each f vec1 vec2 ...)  
(maxvector-count pred? vec1 vec2 ...)

## Searching
(maxvector-index pred? vec1 vec2 ...)  
(maxvector-index-right pred? vec1 vec2 ...)  
(maxvector-skip pred? vec1 vec2 ...)  
(maxvector-skip-right pred? vec1 vec2 ...)  
(maxvector-any pred? vec1 vec2 ...)  
(maxvector-every pred? vec1 vec2 ...)  
(maxvector-partition pred? vec)
