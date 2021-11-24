(matrix? obj)

(matrix-dimensions m [fill])

(matrix-elements-in-order m)

(matrix-copy m)

(matrix-extract m old-dims new-dims)

(matrix-tile m dimensions)

(matrix-transpose m)

(matrix-sample steps)

(matrix-map proc m)

(matrix-outer-product proc vec1 vec2)

(matrix-inner-product proc1 proc2 m1 m2)

(matrix-foldl/foldr/reduce/any/every proc m)

(list->matrix dimensions list)

(lists->matrix lists)

(matrix-list list)

(matrix->lists m)

(matrix-append k . ms)

(matrix-ref m x y)

(matrix-set! m x y obj)

(matrix-reshape m dims)


