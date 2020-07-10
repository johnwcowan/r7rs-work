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
    compound-map->list
    compound-filter
    compound-predicate
    compound-access
    compound-type-properties)
  
  (include "compounds-impl.scm"))
