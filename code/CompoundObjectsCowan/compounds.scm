(define-record-type <compound-object>
  (raw-compound-object subobjs)
  compound-object?
  (raw-object-subobjects subobjs))

(define (compound-object-subobjects obj)
  (if (compound-object? obj)
    (raw-object-subobjects obj)
    (list obj)))

(define (make-compound-object subobjs)
  (raw-compound-object (assemble-subobjects subobjs '())))

(define (assemble-subobjects in out)
  (let loop ((in in) (out out))
    (if (null? in)
      (reverse out)
      (loop
        (if (null? in)
          (reverse out)
          (loop (cdr in) (append (assemble-subobjects (car in)) out)))))))

(define (compound-object . subobjs)
  (make-compound-object subobjs))

(define (compound-object obj)
  (length (compound-object-subobjects obj)))

(define (compound-object-ref obj k)
  (list-ref (compound-object-subobjects obj) k))

(define (compound-object-map mapper obj)
  (raw-compound-object (map mapper (compound-object-subobjects obj))))

(define (filter pred list)
  (let loop ((list list) (result '()))
    (cond
      ((null? list)
       (reverse result))
      ((pred (car list))
       (loop (cdr list) (cons (car list) result)))
      (else
       (loop (cdr list) result)))))

(define (compound-object-filter pred obj)
  (raw-compound-object (filter pred (compound-object-subobjects obj))))

(define (make-compound-predicate pred)
  (lambda (obj)
    (let loop ((in (compound-object-subobjects obj)))
      (cond
        ((null? in)
         #f)
        ((pred obj)
         => (lambda (x) x))
        (else
          (loop (cdr in)))))))

(define (make-compound-accessor pred accessor default)
  (lambda (obj)
    (let loop ((in (compound-object-subobjects obj)))
      (cond
        ((null? in)
         default)
        ((pred (car in))
         (accessor (car in)))
        (else
         (loop (cdr in)))))))

