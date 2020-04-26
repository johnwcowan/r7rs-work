(define-library (advanced-random)
  (import (scheme base)
          (scheme case-lambda)
          (scheme inexact)
          (srfi 27))

  (cond-expand
    ((library (srfi 158)) (import (srfi 158)))
    ((library (srfi 121)) (import (srfi 121))))

  (export 
    make-random-integer-generator
    make-random-u8-generator make-random-s8-generator
    make-random-u16-generator make-random-s16-generator
    make-random-u32-generator make-random-s32-generator
    make-random-u64-generator make-random-s64-generator
    make-random-boolean-generator
    make-random-char-generator
    make-random-string-generator
    make-random-real-generator
    
    make-normal-generator
    make-exponential-generator
    make-geometric-generator
    make-poisson-generator
    
    gsampling
    gweighted-sampling)
  
  (include "advanced-random.scm"))
