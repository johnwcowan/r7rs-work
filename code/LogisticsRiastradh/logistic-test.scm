(import
  (scheme base)
  (scheme inexact)
  (scheme cxr)
  (scheme inexact)
  (scheme complex)
  (scheme write)
  (logistic)
  (srfi 1)
  (srfi 144))

(cond-expand
  (chibi (begin
           (import (except (chibi test) test-equal))
           (define-syntax test-equal
             (syntax-rules ()
               ((_ args ...) (test args ...))))
           (define-syntax test-eqv
             (syntax-rules ()
               ((_ a b) (test-assert (eqv? a b)))))
           (define-syntax test-approximate
             (syntax-rules ()
               ((_ target value max-delta)
                (test-assert (and (<= value (+ target max-delta))
                                  (>= value (- target max-delta)))))))))
  (else (import (srfi 64))))

(define (designify0 x)
  (if (zero? x)
      (abs x)
      x))

(define (relerr e a)
  (if (or (zero? e) (infinite? e))
      (if (= a e) 0 1)
      (magnitude (/ (- e a) e))))

;;
;; LOGIT / LOGISTIC
;;
(test-group
  "logit / logistic"
 (define (test x p t)
   (test-assert (<= (relerr p (logistic x)) 1e-15))
   (if (and (not (= p 0))
            (not (= p 1)))
       (test-assert (<= (relerr x (logit p)) 1e-15)))
   (if (< p 1)
       (begin
         (test-assert (<= (relerr (- 1 p) (logistic (- x))) 1e-15))
         (if (<= 1/2 p)
             ;; In this case, 1 - p is evaluated exactly.
             (test-assert (<= (relerr (- x) (logit (- 1 p))) 1e-15))))
       (test-assert (<= (logistic (- x)) 1e-300)))
   (test-assert (<= (relerr t (log-logistic x)) 1e-15))
   (if (<= x 709)
       (test-assert (<= (relerr (inexact x) (designify0 (logit-exp t)))
                        1e-15)))
   (if (< p 1)
       (test-assert (<= (relerr (fllog1+ (flonum (- p))) (log-logistic (flonum (- x)))) 1e-15))))

 (define cases 
   (list
     (list -36.7368005696771
           1.1102230246251565e-16
           (log 1.1102230246251565e-16))
     (list -1.0000001 0.2689414017088022 (log .2689414017088022))
     (list -0.9999999 0.26894144103118883 (log .26894144103118883))
     (list -1 0.2689414213699951 (log 0.2689414213699951))
     (list -4.000000000537333e-5 .49999 (log .49999))
     (list -4.000000108916879e-9 .499999999 (log .499999999))
     (list 0 1/2 (log 1/2))
     (list 3.999999886872274e-9 .500000001 (log .500000001))
     (list 8.000042667076279e-3 .502 (log .502))
     (list +0.9999999 0.7310585589688111 (log 0.7310585589688111))
     (list +1 0.7310585786300049 (log 0.7310585786300049))
     (list +1.0000001 0.7310585982911977 (log 0.7310585982911977))
     ;; Would like to do +/-710 but we get inexact result traps.
     (list +708 1 -3.307553003638408e-308)
     (list -708 3.307553003638408e-308 -708)
     (list +1000 1. -0.)
     (list -1000 0. -1000.)))

 (for-each
   (lambda (testcase) 
     (test (car testcase)
           (cadr testcase)
           (caddr testcase))) 
   cases))

;;
;; LOGSUMEXP
;;
(test-begin "logsumexp")
(test-group
  "values"
  (define cases 
    (list
      (list (iota 1000) 999.45867514538713)
      (list '(999 1000) 1000.3132616875182)
      (list '(-1000 -1000) (+ -1000 (log 2)))
      (list '(0 0) (log 2))))

  (for-each 
    (lambda (testcase)
      (define l (car testcase))
      (define s (cadr testcase))
      (test-assert (<= (relerr s (logsumexp l)) 1e-15)))
    cases))

(test-group
  "edges"
  (define cases
    (list
      (list '() -inf.0)
      (list '(1) 1)
      (list '(1/2) 1/2)
      (list '(-1) -1)
      (list '(-1/2) -1/2)
      (list '(-1000) -1000)
      (list '(-1000.) -1000.)
      (list (list -inf.0) -inf.0)
      (list (list -inf.0 1) 1.)
      (list (list 1 -inf.0) 1.)
      (list (list +inf.0) +inf.0)
      (list (list +inf.0 1) +inf.0)
      (list (list 1 +inf.0) +inf.0)
      (list (list -inf.0 -inf.0) -inf.0)
      (list (list +inf.0 +inf.0) +inf.0)))

  (for-each
    (lambda (testcase)
      (define l (car testcase))
      (define s (cadr testcase))
      (test-eqv (logsumexp l) s))
    cases))

(test-group 
  "nan"
  (define cases
    (list
      (list (list -inf.0 +inf.0))
      (list (list +inf.0 -inf.0))
      (list (list 1 -inf.0 +inf.0))
      (list (list -inf.0 +inf.0 1))
      (list (list +nan.0))
      (list (list +inf.0 +nan.0))
      (list (list -inf.0 +nan.0))
      (list (list 1 +nan.0))
      (list (list +nan.0 +inf.0))
      (list (list +nan.0 -inf.0))
      (list (list +nan.0 1))))

  (for-each
    (lambda (testcase)
      (define l (car testcase))
      (test-assert (nan? (logsumexp l))))
    cases))
(test-end)

;;
;; LOG1MEXP
;;
(test-begin "log1mexp")
(test-group 
  "invalid"
  (define cases
    (list
      (list fl-epsilon)
      (list 1.)
      (list 2.)
      (list +inf.0)))
  (for-each
    (lambda (testcase)
      (define x (car testcase))
      (test-assert (nan? (log1mexp x))))
    cases))
(test-group
  "nan"
  (define cases
    (list
      (list +nan.0)
      (list -nan.0)))
  (for-each
    (lambda (testcase)
      (define x (car testcase))
      (test-eqv (log1mexp x) x))
    cases))
(test-group
  "values"
  (define cases
    (list
      (list 0 -inf.0)
      (list 0. -inf.0)
      (list -0. -inf.0)
      (list -1e-17 -39.1439465808987777)
      (list -0.69 -0.696304297144056727)
      (list (- (log 2)) (- (log 2)))
      (list -0.70 -0.686341002808385170)
      (list -708 -3.30755300363840783e-308)
      (list -746 -0.)
      (list -inf.0 -0.)))
  (for-each
    (lambda (testcase)
      (define x (car testcase))
      (define y (cadr testcase))
      (test-assert (<= (relerr y (log1mexp x)) 1e-15)))
    cases))
(test-end)

;;
;; LOG1PEXP
;;
(test-begin "log1pexp")
(test-group
  "values"
  (define cases
    (list
      (list -inf.0 0.)
      (list -1000 0.)
      (list -708 3.30755300363840783e-308)
      (list -38 3.13913279204802960e-17)
      (list -37 8.53304762574406580e-17)
      (list -36 2.31952283024356914e-16)
      (list 0 (log 2))
      (list 17 17.0000000413993746)
      (list 18 18.0000000152299791)
      (list 19 19.0000000056027964)
      (list 33 33.0000000000000071)
      (list 34 34.)
      (list 35 35.)
      (list 36 36.)
      (list 37 37.)
      (list 709 709.)
      (list 710 710.)
      (list 1000 1000.)
      (list +inf.0 +inf.0)))
  (for-each 
    (lambda (testcase)
      (define x (car testcase))
      (define y (cadr testcase))
      (test-assert (<= (relerr y (log1pexp x)) 1e-15)))
    cases))
(test-group
  "invalid"
  (define cases
    (list
      (list +nan.0)
      (list -nan.0)))
  (for-each
    (lambda (testcase)
      (define x (car testcase))
      (test-eqv (log1pexp x) x))
    cases))
(test-end)
