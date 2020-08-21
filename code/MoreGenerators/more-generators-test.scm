(import (scheme base)
        (srfi 41)
        (srfi 158)
        (more-generators))

(cond-expand
  (chibi (begin
           (import (except (chibi test) test-equal))
           (define-syntax test-equal
             (syntax-rules ()
               ((_ args ...) (test args ...))))))
  (else (import (srfi 64))))

(test-begin "More generators")

(test-group 
  "accumulate-generated-values"
  (define expect '(1 2 3 4))
  (define actual
    (accumulate-generated-values
      (list-accumulator)
      (generator 1 2 3 4)))
  (test-equal expect actual))

(test-group
  "gdelete-duplicates"
  (define expect '(a b c d))
  (define actual
    (generator->list 
      (gdelete-duplicates 
        (list->generator 
          '(a a b c a a a d c)))))
  (test-equal expect actual))

(test-group
  "genumerate"
  (define expect '((0 . a) (1 . b) (2 . c) (3 . d)))
  (define actual
    (generator->list
      (genumerate (generator 'a 'b 'c 'd))))
  (test-equal expect actual))

(test-group
  "gpeek peek"
  (define g (generator 1 2 3 4))
  (define g* (gpeek g))
  (g* 'poke 0)
  (test-equal 0 (g*))
  (test-equal 1 (g* 'peek))
  (test-equal
    '(1 2 3 4)
    (generator->list g*))
  #t)

(test-group
  "gchain-generators"
  (define g
    (gchain-generators
      (lambda () (make-range-generator 0))
      (lambda (g) (gfilter (lambda (n) (= 0 (modulo n 2))) g))
      (lambda (g) (gtake g 4))))
  
  (define expected '(0 2 4 6))
  (define actual (generator->list g))
  (test-equal expected actual))

(test-group
  "gchoice"
  (define g 
    (gchoice 
      (generator 0 1 0 2)
      (circular-generator 1)
      (circular-generator 2)
      (circular-generator 3)))
  
  (define expected '(1 2 1 3))
  (define actual (generator->list g))
  (test-equal expected actual))

(test-group
  "generator->stream"
  (define g (generator 1 2 3))
  (define expected '(1 2 3))
  (define actual (stream->list (generator->stream g)))
  (test-equal expected actual))

(test-group
  "stream->generator"
  (define s (stream 1 2 3))
  (define expected '(1 2 3))
  (define actual (generator->list (stream->generator s)))
  (test-equal expected actual))

(test-end)
