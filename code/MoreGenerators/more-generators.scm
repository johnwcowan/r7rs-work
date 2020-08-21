(define-library
  (more-generators)
  
  (import 
    (scheme base)
    (scheme case-lambda)
    (srfi 1) ;; lists
    (srfi 41) ;; streams
    (srfi 158)) ;; generators
  
  (export
    accumulate-generated-values
    gdelete-duplicates
    genumerate
    gpeek
    gchain-generators
    gchoice
    generator->stream
    stream->generator)
  
  (include "more-generators-impl.scm"))
