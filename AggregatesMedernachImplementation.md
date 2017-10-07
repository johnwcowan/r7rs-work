
{{{

;; (require srfi/23)

;; Ok on Gambit, Guile, Racket

;; Beware that fields specified as mutables are vectors of length 1 containing the actual value.

(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype <designation> (<fieldname-spec> ...))
     (begin
       (define-syntax <info>
         (syntax-rules (create designate fields)
           ((<info> designate) <designation>)
           ((<info> fields) '(<fieldname-spec> ...))))
       (define-datatype-loop (<designation>) <designation> (<fieldname-spec> ...) () ())))))

(define-syntax define-datatype-loop
  (syntax-rules (mutable)
    ((define-datatype-loop <ancestor-list>
       <designation>
       ((mutable <fieldname>) <other> ...)
       (<mutable-field> ...)
       (<field> ...))
     (define-datatype-loop <ancestor-list>
       <designation>
       (<other> ...)
       (<mutable-field> ... <fieldname>)
       (<field> ... <fieldname>)))
    
    ((define-datatype-loop <ancestor-list>
       <designation>
       ((immutable <fieldname>) <other> ...)
       (<mutable-field> ...)
       (<field> ...))
     (define-datatype-loop <ancestor-list>
       <designation>
       (<other> ...)
       (<mutable-field> ... )
       (<field> ... <fieldname>)))

    ;; By default fields are immutables (except if they contain a
    ;; mutable value, else we should go for a copy on access semantic
    ;; and that's too bad)
    ((define-datatype-loop <ancestor-list>
       <designation>
       (<fieldname> <other> ...)
       (<mutable-field> ...)
       (<field> ...))
     (define-datatype-loop <ancestor-list>
       <designation>
       (<other> ...)
       (<mutable-field> ...)
       (<field> ... <fieldname>)))
    
    ((define-datatype-loop <ancestor-list>
       <designation>
       ()
       (<mutable-field> ...) 
       (<field> ...))
     (define-datatype-helper <ancestor-list>
       <designation>
       (<mutable-field> ...)
       (<field> ...)))))


(define-syntax define-datatype-helper
  (syntax-rules ()
    ((define-datatype-helper (<ancestor> ...) <designation> (<mutable-field> ...) (<fieldname> ...))
     
     (let* ((datatype-tag (vector <ancestor> ...))
            (datatype-depth (- (vector-length datatype-tag) 1)))
       (letrec ( ;; We currently lack a way to create disjoint datatypes
                (aggregate cons)
                (aggregate? pair?)
                (aggregate->tag car)
                (aggregate->content cdr)

                ;; (datatype-subtype? (lambda (tag1 tag2) (eqv? tag1 tag2)))

                (datatype-subtype?
                 (lambda (tag)
                   (and (vector? tag)
                        (< datatype-depth (vector-length tag))
                        (eqv? (vector-ref tag datatype-depth)
                              <designation>))))
                
                (make-aggregate
                 (lambda (<fieldname> ...)
                   ;; it contains:
                   ;; - the datatype tag associated to datatype (+ inheritance structure)
                   ;; - values of all fields
                   (let ((<mutable-field> (vector <mutable-field>)) ...)
                     ;; We have to enclose 'values' with a thunk because of bigloo, larceny and racket which forbid to store values
                     (aggregate datatype-tag (lambda () (values <fieldname> ...))))))
             
                (aggregate-switch
                 (lambda (aggregate-case else-case)
                   (lambda (obj)                     
                     (cond
                      ;; First: check if it is an aggregate kind
                      ((not (aggregate? obj)) (error "Not an aggregate: " obj))
                      ;; Second: check if it is of corresponding datatype type or a subtype
                      ((not (datatype-subtype? (aggregate->tag obj))) (else-case obj))
                      (else
                       (call-with-values (aggregate->content obj)
                         (lambda (<fieldname> ... . extension)
                           ;; A little trick to easily allow inheritance
                           (aggregate-case <fieldname> ...)))))))))
         
         (lambda ()
           (values make-aggregate
                   aggregate-switch
                   ;; One could easily creates his own mutators like this:
                   (list (list '<mutable-field>
                               (lambda (obj val)
                                 ((aggregate-switch
                                   (lambda (<fieldname> ...) (vector-set! <mutable-field> 0 val))
                                   (lambda (obj) (error "Not of corresponding type: " obj)))
                                  obj)))
                         ... ))))))))

(define-syntax datatype-info->designation
  (syntax-rules ()
  ((datatype-info->designation datatype-info)
   (datatype-info designate))))

(define-syntax datatype-info->fields
  (syntax-rules ()
    ((datatype-info->fields datatype-info)
     (datatype-info fields))))

(define (create-aggregate-functions datatype) datatype)

}}}
