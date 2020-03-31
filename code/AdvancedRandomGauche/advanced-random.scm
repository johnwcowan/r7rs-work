(define-library
  (advanced-random)
  (import (scheme base)
          (scheme case-lambda)
          (scheme inexact)
          (srfi 27)
          (srfi 158))
  (export 
    make-random-integer-generator
    make-random-u8-generator 
    make-random-s8-generator 
    make-random-u16-generator 
    make-random-s16-generator 
    make-random-u32-generator 
    make-random-s32-generator 
    make-random-u64-generator 
    make-random-s64-generator 
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
  
  (begin

    ;;
    ;; Primitive randoms
    ;;
    
    (define make-random-integer-generator
      (case-lambda
        ((low-bound up-bound) 
         (make-random-integer-generator default-random-source low-bound up-bound))
        ((rand-src low-bound up-bound)
         (when (not (random-source? rand-src))
           (error "expected random-source"))
         (when (not (integer? low-bound))
           (error "expected integer"))
         (when (not (integer? up-bound))
           (error "expected integer"))
         (let ((rand-int-proc (random-source-make-integers rand-src))
               (range (- up-bound low-bound)))
           (lambda ()
             (+ low-bound (rand-int-proc range)))))))

    ;private
    (define (make-int-generator-maker low-bound up-bound)
      (case-lambda
        (() (make-random-integer-generator low-bound up-bound))
        ((s) (make-random-integer-generator s low-bound up-bound))))

    (define make-random-u8-generator (make-int-generator-maker 0 256)) 
    (define make-random-s8-generator (make-int-generator-maker -128 128)) 
    (define make-random-u16-generator (make-int-generator-maker 0 65536)) 
    (define make-random-s16-generator (make-int-generator-maker -32768 32768)) 
    (define make-random-u32-generator (make-int-generator-maker 0 (expt 2 32))) 
    (define make-random-s32-generator (make-int-generator-maker (- (expt 2 31)) (expt 2 31))) 
    (define make-random-u64-generator (make-int-generator-maker 0 (expt 2 64)))
    (define make-random-s64-generator (make-int-generator-maker (- (expt 2 63)) (expt 2 63)))

    (define make-random-boolean-generator
      (case-lambda
        (() (make-random-boolean-generator default-random-source))
        ((s)
         (let ((int-gen (make-random-integer-generator s 0 2)))
          (lambda ()
            (zero? (int-gen)))))))

    (define make-random-char-generator 
      (case-lambda
        ((str)
         (make-random-char-generator default-random-source str))
        ((rand-src str)
         (when (not (random-source? rand-src))
           (error "expected random-source"))
         (when (not (string? str))
           (error "expected string"))
         (let* ((int-gen (make-random-integer-generator rand-src 0 (string-length str))))
           (lambda ()
             (string-ref str (int-gen)))))))

    (define make-random-string-generator
      (case-lambda 
        ((k str) (make-random-string-generator default-random-source k str))
        ((s k str)
         (let ((char-gen (make-random-char-generator s str))
               (int-gen (make-random-integer-generator s 0 k))) 
           (lambda () 
             (generator->string char-gen (int-gen)))))))

    (define make-random-real-generator
      (case-lambda
        ((low-bound up-bound)
         (make-random-real-generator default-random-source low-bound up-bound))
        ((rand-src low-bound up-bound)
         (let* ((rand-int-proc (random-source-make-integers rand-src))
                (steps (expt 2 32))
                (rand-real-proc (lambda ()
                                  (/ (inexact (rand-int-proc steps))
                                     (- steps 1))))
                (range (- up-bound low-bound)))
          (lambda ()
            (+ low-bound (* (rand-real-proc) range)))))))

    ;;
    ;; Non-uniform distributions
    ;;
    
    ;; TODO import from somewhere?
    (define PI 3.1415926535897932)
    
    (define make-normal-generator
      (case-lambda
        (()
         (make-normal-generator default-random-source 0.0 1.0))
        ((arg1)
         (cond 
           ((random-source? arg1)
            (make-normal-generator arg1 0.0 1.0))
           (else (make-normal-generator default-random-source arg1 1.0))))
        ((arg1 arg2)
         (cond
           ((and (random-source? arg1)
                 (number? arg2))
            (make-normal-generator arg1 arg2 1.0))
           ((and (number? arg1)
                 (number? arg2))
            (make-normal-generator default-random-source arg1 arg2))
           (else (error "expected random-source and mean, or mean and standard deviation"))))
        ((rand-src mean deviation)
         (let ((rand-real-proc (random-source-make-reals rand-src)))
          (lambda ()
            ;;Box-Muller
            (let ((r (sqrt (* -2 (log (rand-real-proc)))))
                  (theta (* 2 PI (rand-real-proc))))
              (+ mean (* deviation r (sin theta)))))))))
    
    (define make-exponential-generator
      (case-lambda
        ((mean)
         (make-exponential-generator default-random-source mean))
        ((rand-src mean)
         (let ((rand-real-proc (random-source-make-reals rand-src)))
          (lambda ()
           (- (* mean (log (rand-real-proc)))))))))
    
    (define make-geometric-generator
      (case-lambda
        ((p)
         (make-geometric-generator default-random-source p))
        ((rand-src p)
         (let ((c (/ (log (- 1.0 p))))
               (rand-real-proc (random-source-make-reals rand-src)))
          (lambda ()
            (ceiling (* c (log (rand-real-proc)))))))))

    (define make-poisson-generator 
      (case-lambda
        ((L)
         (make-poisson-generator default-random-source L))
        ((rand-src L)
         (let ((rand-real-proc (random-source-make-reals rand-src)))
          (if (< L 36)
              (make-poisson/small rand-real-proc L)
              (make-poisson/large rand-real-proc L)))))) 
    
    ;private
    (define (make-poisson/small rand-real-proc L)
      (lambda ()
        (do ((exp-L (exp (- L)))
             (k 0 (+ k 1))
             (p 1.0 (* p (rand-real-proc))))
            ((<= p exp-L) (- k 1)))))
    
    ;private
    (define (make-poisson/large rand-real-proc L)
      (let* ((c (- 0.767 (/ 3.36 L)))
             (beta (/ PI (sqrt (* 3 L))))
             (alpha (* beta L))
             (k (- (log c) L (log beta))))
        (define (loop)
          (let* ((u (rand-real-proc))
                 (x (/ (- alpha (log (/ (- 1.0 u) u))) beta))
                 (n (exact (floor (+ x 0.5)))))
            (if (< n 0)
                (loop)
                (let* ((v (rand-real-proc))
                       (y (- alpha (* beta x)))
                       (t (+ 1.0 (exp y)))
                       (lhs (+ y (log (/ v (* t t)))))
                       (rhs (+ k (* n (log L)) (- (log-of-fact n)))))
                  (if (<= lhs rhs)
                      n
                      (loop))))))
        loop))
    
    ;private
    ;log(n!) table for n 1 to 256. Vector, where nth index corresponds to log((n+1)!)
    ;Computed on first invocation of `log-of-fact` 
    (define log-fact-table #f)
    
    ;private
    ;computes log-fact-table
    ;log(n!) = log((n-1)!) + log(n)
    (define (make-log-fact-table!)
       (define table (make-vector 256))
       (vector-set! table 0 0)
       (do ((i 1 (+ i 1)))
           ((> i 255) #t)
           (vector-set! table i (+ (vector-ref table (- i 1))
                                   (log (+ i 1)))))
       (set! log-fact-table table))
    
    ;private
    ;returns log(n!)
    ;adapted from https://www.johndcook.com/blog/2010/08/16/how-to-compute-log-factorial/
    (define (log-of-fact n)
      (when (not log-fact-table)
        (make-log-fact-table!))
      (cond 
        ((<= n 1) 0)
        ((<= n 256) (vector-ref log-fact-table (- n 1)))
        (else (let ((x (+ n 1)))
               (+ (* (- x 0.5)
                     (log x))
                  (- x)
                  (* 0.5
                     (log (* 2 PI)))
                  (/ 1.0 (* x 12.0)))))))
   
    
    (define (gsampling . args)
      (cond
        ((null? args) (gsampling* default-random-source '()))
        ((random-source? (car args)) (gsampling* (car args) (cdr args)))
        (else (gsampling* default-random-source args))))
    
    ;private
    (define (gsampling* s generators-lst)
      (let ((gen-vec (list->vector generators-lst))
            (rand-int-proc (random-source-make-integers s)))
        
           ;remove exhausted generator at index
           (define (remove-gen index)
             (define new-vec (make-vector (- (vector-length gen-vec) 1)))
             ;when removing anything but first, copy all elements before index
             (when (> index 0)
               (vector-copy! new-vec 0 gen-vec 0 index))
             ;when removing anything but last, copy all elements after index
             (when (< index (- (vector-length gen-vec) 1))
               (vector-copy! new-vec index gen-vec (+ 1 index)))
             (set! gen-vec new-vec))
           
           ;randomly pick generator. If it's exhausted remove it, and pick again
           ;returns value (or eof, if all generators are exhausted) 
           (define (pick)
             (let* ((index (rand-int-proc (vector-length gen-vec)))
                    (gen (vector-ref gen-vec index))
                    (value (gen)))
               (if (eof-object? value)
                   (begin
                     (remove-gen index)
                     (if (= (vector-length gen-vec) 0)
                         (eof-object)
                         (pick)))
                   value)))
           
           (lambda ()
             (if (= 0 (vector-length gen-vec))
                 (eof-object)
                 (pick)))))
    
    (define (gweighted-sampling . args)
      (cond
        ((null? args) (gweighted-sampling* default-random-source '()))
        ((random-source? (car args)) (gweighted-sampling* (car args) (group-weights-with-generators (cdr args))))
        (else (gweighted-sampling* default-random-source (group-weights-with-generators args)))))
    
    ;private
    (define (group-weights-with-generators objs)
      (let loop ((objs objs)
                 (pairs '()))
        (cond
          ((null? objs) (reverse pairs))
          (else (begin
                 (when (null? (cdr objs))
                   (error "Uneven amount of arguments provided"))
                 (when (not (number? (car objs)))
                   (error "Expected number"))
                 (when (< (car objs) 0)
                   (error "Weight cannot be negative"))
                 (loop (cddr objs)
                       (cons (cons (car objs) (cadr objs)) 
                             pairs)))))))
    
    ;private
    (define (gweighted-sampling* s weight+generators-lst)
      (let ((weight-sum (apply + (map car weight+generators-lst)))
            (rand-real-proc (random-source-make-reals s)))
           
           ;randomly pick generator. If it's exhausted remove it, and pick again.
           ;returns value (or eof, if all generators are exhausted) 
           (define (pick)
             (let* ((roll (* (rand-real-proc) weight-sum))
                    (picked+rest-gens (pick-weighted-generator roll weight+generators-lst))
                    (picked-gen (car picked+rest-gens))
                    (value ((cdr picked-gen))))
               (if (eof-object? value)
                   (begin
                     (set! weight+generators-lst (cdr picked+rest-gens))
                     (set! weight-sum (apply + (map car weight+generators-lst)))
                     (if (null? weight+generators-lst)
                         (eof-object)
                         (pick)))
                   value)))
           
           (lambda ()
             (if (null? weight+generators-lst)
                 (eof-object)
                 (pick)))))
    
    ;private
    ;returns pair, where car is picked generator, and cdr is list of rest generators in preserved order
    (define (pick-weighted-generator roll weight+gen-lst)
      (let loop ((sum 0)
                 (weight+gen-lst weight+gen-lst)
                 (picked-gen #f)
                 (rest-gen-rev '()))
        (if (null? weight+gen-lst) 
            (cons picked-gen (reverse rest-gen-rev))
            (let* ((w+g (car weight+gen-lst)))
             (if (or picked-gen
                     (< (+ sum (car w+g)) roll))
                 (loop (+ sum (car w+g))
                       (cdr weight+gen-lst)
                       picked-gen
                       (cons w+g rest-gen-rev))
                 (loop (+ sum (car w+g))
                       (cdr weight+gen-lst)
                       w+g
                       rest-gen-rev))))))))
