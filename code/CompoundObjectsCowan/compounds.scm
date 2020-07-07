(define-library (compounds)
                
  (import (scheme base))
  
  (export
    make-compound
    compound
    compound?
    compound-type?
    compound-subobjects
    compound-values
    compound-length
    compound-ref
    compound-map
    compound-filter
    compound-predicate
    compound-accessor
    compound-type-properties)
  
  (include "compounds-impl.scm"))
