(define-library 
  (logistic)
  
  (import (scheme base)
          (scheme inexact)
          (srfi 1)
          (srfi 144))
  (export 
    logit
    logistic
    logsumexp
    log1mexp
    log1pexp
    logit-exp
    log-logistic)
  
  (include "logistic-impl.scm"))
