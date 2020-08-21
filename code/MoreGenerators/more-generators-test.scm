(import (scheme base)
        (scheme case-lambda)
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

(define test-g-equal
  (case-lambda
    ((g-expect g-actual)
     (test-equal
       (generator->list g-expect)
       (generator->list g-actual)))
    ((g-expect g-actual take-count)
     (test-g-equal
       (gtake g-expect take-count)
       (gtake g-actual take-count)))))

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
  ;; test normal case
  (test-g-equal (generator 'a 'b 'c 'd) 
                (gdelete-duplicates
                  (generator 'a 'a 'b 'c 'a 'a 'a 'd 'c)))
  
  ;; test empty
  (test-g-equal (generator)
                (gdelete-duplicates (generator)))
  
  ;; test infinite case with take
  (test-g-equal (generator 'a 'b 'c 'd)
                (gdelete-duplicates 
                    (gtake (circular-generator 'a 'b 'c 'd) 5))))

(test-group
  "genumerate"
  
  ;; test normal case
  (test-g-equal (generator '(0 . a) '(1 . b) '(2 . c))
                (genumerate (generator 'a 'b 'c)))
  
  ;; test empty
  (test-g-equal (generator)
                (genumerate (generator)))
  
  ;; infinite case with take
  (test-g-equal (generator '(0 . a) '(1 . b) '(2 . c))
                (genumerate (circular-generator 'a 'b 'c))
                3)
  )

(test-group
  "gpeek peek"
  
  ;; test do nothing
  (test-g-equal
    (generator 1 2 3 4)
    (gpeek (generator 1 2 3 4)))
  
  ;; test peek
  (test-g-equal
    (generator 2 3 4)
    (let* ((g (generator 1 2 3 4))
           (g* (gpeek g)))
      ;; can peek many times, same result
      (test-equal 1 (g* 'peek))
      (test-equal 1 (g* 'peek))
      
      (test-equal 1 (g*))
      g*))
  
  ;; test poke
  (test-g-equal
    (generator 2 3 4)
    (let* ((g (generator 2 3 4))
           (g* (gpeek g)))
      ;; can poke many times, last poke saved
      (g* 'poke 0)
      (g* 'poke 1)
      
      (test-equal 1 (g*))
      g*))
  
  ;; test peek then poke
  (test-g-equal
    (generator 1 3 4)
    (let* ((g (generator 2 3 4))
           (g* (gpeek g)))
      (test-equal 2 (g* 'peek))
      (g* 'poke 1)
      g*))
  
  ;; test poke then peek
  (test-g-equal
    (generator 1 2 3 4)
    (let* ((g (generator 2 3 4))
           (g* (gpeek g)))
      (g* 'poke 1)
      (test-equal 1 (g* 'peek))
      g*)))

(test-group
  "gchain-generators"
  
  (test-g-equal
    (generator 1 2 3 4)
    (gchain-generators
      (lambda () (make-range-generator 1))
      (lambda (g) (gtake g 4))))
  
  (test-g-equal
    (generator 1 2 3 4)
    (gchain-generators
      (lambda () (generator 1 2 3 4)))))

(test-group
  "gchoice"
  
  ;; test normal
  (test-g-equal
    (generator 1 2 1 3)
    (gchoice
      (generator 0 1 0 2)
      (circular-generator 1)
      (circular-generator 2)
      (circular-generator 3)))
  
  ;; test exhausted source
  (test-g-equal
    (generator 1 2 3)
    (gchoice
      (generator 0 0 0 0 0 1 1 2)
      (generator 1)
      (generator 2)
      (generator 3))))

(test-group
  "generator->stream"
  (define (test-stream-equal str1 str2)
    (if (stream-null? str1)
        (test-assert (stream-null? str2))
        (begin
          (test-equal (stream-car str1) (stream-car str2))
          (test-stream-equal (stream-cdr str1) (stream-cdr str2)))))

  ;; test normal
  (test-stream-equal
    (stream 1 2 3)
    (generator->stream (generator 1 2 3)))
  
  ;; test infinite with take 
  (test-stream-equal
    (stream 1 2 3)
    (stream-take 3 (generator->stream (circular-generator 1 2 3)))))

(test-group
  "stream->generator"
  ;; test normal
  (test-g-equal 
    (generator 1 2 3)
    (stream->generator (stream 1 2 3)))
  
  ;; test infinite with take
  (test-g-equal
    (circular-generator 1 2 3)
    (stream->generator (stream-constant 1 2 3))
    20))

(test-end)
