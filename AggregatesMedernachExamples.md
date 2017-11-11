## Examples

Could be used with test implementation at [AggregatesMedernachImplementation](AggregatesMedernachImplementation.md) and [AggregatesMedernachInheritanceImplementation](AggregatesMedernachInheritanceImplementation.md).

```

(define-syntax multi-define
   (syntax-rules ()   
     ((multi-define (<var> ...) <list>)
      (begin
        (begin
          (define <var> (car <list>))
          (define <list> (cdr <list>)))
        ...))))      
     
(define-syntax define-values-with
   (syntax-rules () 
     ((define-values-with (<var> ...) <thunk>)
      (begin
        (define var-list (call-with-values <thunk> list))
        (multi-define (<var> ...) var-list)))))

(define-datatype null-type null-type-info "NULL" ())

(define-values-with (make-null null-switch null-mutators)
  (create-aggregate-functions null-type))

(define-datatype pair-type pair-info "PAIR" (first second))

(define-values-with (make-pair pair-switch pair-mutators)
  (create-aggregate-functions pair-type))

;; With my-car, my-cdr for instance:

(define (my-car obj)
  ((pair-switch (lambda (first second) first) error)
  obj))
 
(define (my-cdr obj)
  ((pair-switch (lambda (first second) second) error)
   obj))

(define mypair (make-pair 'one 'two))

(and (eq? 'one (my-car mypair))
     (eq? 'two (my-cdr mypair))
     (display "Ok")(newline)
     (display (datatype-info->designation pair-info)) (newline)
     (display (datatype-info->fields pair-info)) (newline))

```

```
;; 3d point example

(define-datatype point3d-type point3d-info
  "3d point" ((mutable X) (mutable Y) (mutable Z)))

(define-values-with (make-point3d point3d-switch point3d-mutators)
  (create-aggregate-functions point3d-type))

(define point3d-set-X! (cadr (assoc 'X point3d-mutators)))
(define point3d-set-Y! (cadr (assoc 'Y point3d-mutators)))
(define point3d-set-Z! (cadr (assoc 'Z point3d-mutators)))
 
(define (point3d-length X Y Z)
  (let ((x (vector-ref X 0))
        (y (vector-ref Y 0))
        (z (vector-ref Z 0)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(define (point3d-scale alpha)
  (lambda (X Y Z)
    (let ((x (vector-ref X 0))
          (y (vector-ref Y 0))
          (z (vector-ref Z 0)))
      (make-point3d (* alpha x) (* alpha y) (* alpha z)))))

(define p3d (make-point3d 3 4 5))

((point3d-switch point3d-length error) p3d) ;; 7.07...
((point3d-switch (point3d-scale -2) error) p3d) ;; [[-6|-8 -10]]

(point3d-set-Y! p3d -1)

(display ((point3d-switch list error) p3d)) ;; (3 -1 5)

```

```
(define-syntax variant-case
  (syntax-rules (else)
    ((variant-case <obj>)
     (error "variant-case: all case exhausted " <obj>))
    
    ((variant-case <obj>
        (else <body> ...))
     (begin <body> ...))
    
    ((variant-case <obj>
        (<aggregate-switch> (<var> ...) <body> ...)
        rest ...)
     ((<aggregate-switch>
       (lambda (<var> ...) <body> ...)
       (lambda (<obj>) (variant-case <obj> rest ...)))
      <obj>))))


;; Binary tree example

(define-datatype bin-leaf-type bin-leaf-info "Binary tree leaf" (Data))

(define-values-with (make-bin-leaf bin-leaf-switch bin-leaf-mutators)
  (create-aggregate-functions bin-leaf-type))

(define-datatype bin-node-type bin-node-info "Binary tree node" (Data Left Right))
  
(define-values-with (make-bin-node bin-node-switch bin-node-mutators)
  (create-aggregate-functions bin-node-type))

; variant-case example

(define (tree->list bin-tree)
  (variant-case bin-tree
    (bin-node-switch (data left right)
      (list data (tree->list left) (tree->list right)))
    (bin-leaf-switch (data)
      data)
    (else (error "Not a bin-tree: " bin-tree))))

(define (map-tree fun bin-tree)
  (variant-case bin-tree
    (bin-node-switch (data left right)
      (make-bin-node (fun data)
                     (map-tree fun left)
                     (map-tree fun right)))
    (bin-leaf-switch (data)
      (make-bin-leaf (fun data)))
    (else (error "Not a bin-tree: " bin-tree))))

(define mytree
  (make-bin-node 'a
                 (make-bin-node 'b
                                (make-bin-leaf 'c)
                                (make-bin-leaf 'd))
                 (make-bin-node 'e
                                (make-bin-leaf 'f)
                                (make-bin-leaf 'g))))

(display (tree->list (map-tree symbol->string mytree)))
(newline)

```

```
;; Unforgeable aggregate with built-in assertion checking.
(define-syntax create-aggregate-with-assertion
  (syntax-rules ()
    ((create-aggregate-with-assertion <datatype-designation> (<field> ...) <assertion>)
     (let ()
       (define-datatype type type-info <datatype-designation> (<field> ...))
       (call-with-values (create-aggregate-functions type)
         (lambda (maker switch mutators)
           (values (lambda (<field> ...)
                     (if (<assertion> <field> ...)
                         (maker <field> ...)
                         (error "Assertion failed: "
                                (list <datatype-designation> <field> ...))))
                   switch)))))))

(display "Interval: ")
(display 
 (call-with-values (lambda () (create-aggregate-with-assertion "Interval" (left right) <))
   (lambda (maker switch)
     ((switch list error) (maker 2 4))))) ;; Ok
(newline)

(display "Interval: ")
(display 
 (call-with-values (lambda () (create-aggregate-with-assertion "Interval" (left right) <))
   (lambda (maker switch)
     ((switch list error) (maker 4 2))))) ;; error
    
(newline)

```
