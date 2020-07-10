(define-record-type <compound-object>
  (raw-compound-object subobjs)
  compound?
  (subobjs raw-object-subobjects))

(define (compound-subobjects obj)
  (if (compound? obj)
    (raw-object-subobjects obj)
    (list obj)))

(define (make-compound subobjs)
  (raw-compound-object (assemble-subobjects subobjs)))

(define (alist? lst)
  (cond
    ((null? lst) #t)
    ((not (pair? lst)) #f)
    (else (let ((entry (car lst))
                (rest (cdr lst)))
            (and (pair? entry)
                 (symbol? (car entry))
                 (alist? rest))))))

(define (compound-type? obj)
  (and (pair? obj)
       (symbol? (car obj))
       (alist? (cdr obj))))

(define (assemble-subobjects in)
  (let loop ((in in) 
             (out '()))
    (if (null? in)
      (reverse out)
      (loop (cdr in)
            (if (compound? (car in))
                (append (reverse (compound-subobjects (car in))) out)
                (cons (car in) out))))))

(define (compound . subobjs)
  (make-compound subobjs))

(define (compound-values obj)
  (apply values (compound-subobjects obj)))

(define (compound-length obj)
  (if (compound? obj)
      (length (raw-object-subobjects obj))
      1))

(define (compound-ref obj k)
  (list-ref (compound-subobjects obj) k))

(define (compound-map mapper obj)
  (make-compound (compound-map->list mapper obj)))

(define (compound-map->list mapper obj)
  (map mapper (compound-subobjects obj)))

(define (filter pred list)
  (let loop ((list list) (result '()))
    (cond
      ((null? list)
       (reverse result))
      ((pred (car list))
       (loop (cdr list) (cons (car list) result)))
      (else
       (loop (cdr list) result)))))

(define (compound-filter pred obj)
  (raw-compound-object (filter pred (compound-subobjects obj))))

(define (compound-predicate pred obj)
  (let loop ((in (compound-subobjects obj)))
   (cond
     ((null? in)
      #f)
     ((pred obj)
      => (lambda (x) x))
     (else
       (loop (cdr in))))))

(define (compound-access pred accessor obj default)
  (let loop ((in (compound-subobjects obj)))
   (cond
     ((null? in)
      default)
     ((pred (car in))
      (accessor (car in)))
     (else
       (loop (cdr in))))))

(define (compound-type-properties sym obj)
  (define (pred subobj)
    (and (compound-type? subobj)
         (equal? sym (car subobj))))
  (cond
    ((compound? obj) (compound-access pred cdr obj #f))
    ((pred obj) (cdr obj))
    (else #f)))
