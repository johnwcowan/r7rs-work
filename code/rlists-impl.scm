(define-record-type kons
  ;(make-kons size tree rest)
  kons?
  (size kons-size)
  (tree kons-tree)
  (rest kons-rest))

(define-record-type node
  (make-node val left right)
  node?
  (val node-val)
  (left node-left)
  (right node-right))

;; Nat -> Nat
(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))
  
;; [Tree X] -> X
(define (tree-val t)
  (if (node? t) 
      (node-val t)
      t))

;; [X -> Y] [Tree X] -> [Tree Y]
(define (tree-map f t)
  (if (node? t)
      (make-node (f (node-val t))
                 (tree-map f (node-left t))
                 (tree-map f (node-right t)))
      (f t)))

;; [X -> Y] [Tree X] -> unspecified
(define (tree-for-each f t)
  (if (node? t)
      (begin (f (node-val t))
             (tree-for-each f (node-left t))
             (tree-for-each f (node-right t)))
      (f t)))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> [Tree R]
(define (tree-map/n f ts)
  (let recr ((ts ts))
    (if (and (pair? ts)
             (node? (car ts)))
        (make-node (apply f (map node-val ts))
                   (recr (map node-left ts))
                   (recr (map node-right ts)))
        (apply f ts))))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> unspecified
(define (tree-for-each/n f ts)
  (let recr ((ts ts))
    (if (and (pair? ts)
             (node? (car ts)))
        (begin (apply f (map node-val ts))
               (recr (map node-left ts))
               (recr (map node-right ts)))
        (apply f ts))))

;; Nat [Nat -> X] -> [Tree X]
;; like build-list, but for complete binary trees
(define (build-tree i f) ;; i = 2^j-1
  (let rec ((i i) (o 0))
    (if (= 1 i) 
        (f o)
        (let ((i/2 (half i)))
          (make-node (f o)
                     (rec i/2 (add1 o))
                     (rec i/2 (+ 1 o i/2)))))))

;; Consumes n = 2^i-1 and produces 2^(i-1)-1.
;; Nat -> Nat
(define (half n)
  (arithmetic-shift n -1))

;; Nat X -> [Tree X]
(define (tr:make-tree i x) ;; i = 2^j-1
  (let recr ((i i))
    (if (= 1 i) 
        x
        (let ((n (recr (half i))))
          (make-node x n n)))))

;; Nat [Tree X] Nat [X -> X] -> X [Tree X]
(define (tree-ref/update mid t i f)
  (cond ((zero? i)
         (if (node? t) 
             (values (node-val t)
                     (make-node (f (node-val t))
                                (node-left t)
                                (node-right t)))
             (values t (f t))))
        ((<= i mid)
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (node-left t) 
                                                (sub1 i) 
                                                f)))
           (values v* (make-node (node-val t) t* (node-right t)))))
        (else           
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (node-right t) 
                                                (sub1 (- i mid)) 
                                                f)))
           (values v* (make-node (node-val t) (node-left t) t*))))))

;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
;; [Tree X] Nat Nat -> X
;; invariant: (= mid (half (sub1 (tree-count t))))
(define (tree-ref/a t i mid) 
  (cond ((zero? i) (tree-val t))
        ((<= i mid) 
         (tree-ref/a (node-left t) 
                     (sub1 i) 
                     (half (sub1 mid))))
        (else 
         (tree-ref/a (node-right t) 
                     (sub1 (- i mid)) 
                     (half (sub1 mid))))))

;; Nat [Tree X] Nat -> X
;; invariant: (= size (tree-count t))
(define (tree-ref size t i)
  (if (zero? i)
      (tree-val t)
      (tree-ref/a t i (half (sub1 size)))))

;; Nat [Tree X] Nat [X -> X] -> [Tree X]
(define (tree-update size t i f)
  (let recr ((mid (half (sub1 size))) (t t) (i i))
    (cond ((zero? i)
           (if (node? t)
               (make-node (f (node-val t))
                          (node-left t)
                          (node-right t))
               (f t)))
          ((<= i mid)
           (make-node (node-val t) 
                      (recr (half (sub1 mid))
                            (node-left t) 
                            (sub1 i)) 
                      (node-right t)))
          (else
           (make-node (node-val t) 
                      (node-left t) 
                      (recr (half (sub1 mid))
                            (node-right t) 
                            (sub1 (- i mid))))))))

;; ------------------------
;; Random access lists

;; [RaListof X]
(define rnull (quote ()))

;; [Any -> Boolean]
(define rpair? kons?)

;; [Any -> Boolean]
(define rnull? null?)

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(define (rcons x ls)
  (if (kons? ls)
      (let ((s (kons-size ls)))
        (if (and (kons? (kons-rest ls))
                 (= (kons-size (kons-rest ls))
                    s))
            (make-kons (+ 1 s s) 
                       (make-node x 
                                  (kons-tree ls)
                                  (kons-tree (kons-rest ls)))
                       (kons-rest (kons-rest ls)))
            (make-kons 1 x ls)))
      (make-kons 1 x ls)))


;; [RaPair X Y] -> X Y
(define rcar+cdr 
  (lambda (p)
    (assert (kons? p))
    (if (node? (kons-tree p))
        (let ((s* (half (kons-size p))))
          (values (tree-val (kons-tree p))
                  (make-kons s* 
                             (node-left (kons-tree p))
                             (make-kons s*
                                        (node-right (kons-tree p))
                                        (kons-rest p)))))
        (values (kons-tree p) (kons-rest p)))))

;; [RaPair X Y] -> X
(define (rcar p)
  (call-with-values (lambda () (rcar+cdr p))
                    (lambda (car cdr) car)))

;; [RaPair X Y] -> Y
(define (rcdr p)
  (call-with-values (lambda () (rcar+cdr p))
                    (lambda (car cdr) cdr)))

;; [RaListof X] Nat [X -> X] -> X [RaListof X]
(define (rlist-ref/update ls i f)
  ;(assert (< i (rlength ls)))
  (let recr ((xs ls) (j i))
    (if (< j (kons-size xs))
        (let-values (((v* t*) 
                      (tree-ref/update (half (sub1 (kons-size xs))) 
                                       (kons-tree xs) j f)))
          (values v* (make-kons (kons-size xs) 
                                t* 
                                (kons-rest xs))))
        (let-values (((v* r*) 
                      (recr (kons-rest xs) 
                            (- j (kons-size xs)))))
          (values v* (make-kons (kons-size xs) 
                                (kons-tree xs) 
                                r*))))))

;; [RaListof X] Nat [X -> X] -> [RaListof X]
(define (rlist-update ls i f)
  ;(assert (< i (rlength ls)))
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s) 
          (make-kons s (tree-update s (kons-tree xs) j f) (kons-rest xs))
          (make-kons s (kons-tree xs) (recr (kons-rest xs) (- j s)))))))

;; [RaListof X] Nat X -> (values X [RaListof X])
(define (rlist-ref/set ls i v)
  (rlist-ref/update ls i (lambda (_) v)))

;; X ... -> [RaListof X]
(define (rlist . xs)
  (fold-right rcons rnull xs))

;; Nat X -> [RaListof X]
(define make-rlist
  (case-lambda
   ((k) (make-rlist k 0))
   ((k obj)
    (let loop ((n k) (a rnull))
      (cond ((zero? n) a)
            (else 
             (let ((t (largest-skew-binary n)))
               (loop (- n t)
                     (make-kons t (tr:make-tree t obj) a)))))))))

;; A Skew is a Nat 2^k-1 with k > 0.

;; Skew -> Skew
(define (skew-succ t) (add1 (arithmetic-shift t 1)))

;; Computes the largest skew binary term t <= n.
;; Nat -> Skew
(define (largest-skew-binary n)
  (if (= 1 n) 
      1
      (let* ((t (largest-skew-binary (half n)))
             (s (skew-succ t)))
        (if (> s n) t s))))  

;; [Any -> Boolean]
;; Is x a PROPER list?
(define (rlist? x)
  (or (rnull? x)
      (and (kons? x)
           (rlist? (kons-rest x)))))

(define rcaar (lambda (ls) (rcar (rcar ls))))
(define rcadr (lambda (ls) (rcar (rcdr ls))))
(define rcddr (lambda (ls) (rcdr (rcdr ls))))
(define rcdar (lambda (ls) (rcdr (rcar ls))))
  
(define rcaaar (lambda (ls) (rcar (rcar (rcar ls)))))
(define rcaadr (lambda (ls) (rcar (rcar (rcdr ls)))))
(define rcaddr (lambda (ls) (rcar (rcdr (rcdr ls)))))
(define rcadar (lambda (ls) (rcar (rcdr (rcar ls)))))
(define rcdaar (lambda (ls) (rcdr (rcar (rcar ls)))))
(define rcdadr (lambda (ls) (rcdr (rcar (rcdr ls)))))
(define rcdddr (lambda (ls) (rcdr (rcdr (rcdr ls)))))
(define rcddar (lambda (ls) (rcdr (rcdr (rcar ls)))))

(define rcaaaar (lambda (ls) (rcar (rcar (rcar (rcar ls))))))
(define rcaaadr (lambda (ls) (rcar (rcar (rcar (rcdr ls))))))
(define rcaaddr (lambda (ls) (rcar (rcar (rcdr (rcdr ls))))))
(define rcaadar (lambda (ls) (rcar (rcar (rcdr (rcar ls))))))
(define rcadaar (lambda (ls) (rcar (rcdr (rcar (rcar ls))))))
(define rcadadr (lambda (ls) (rcar (rcdr (rcar (rcdr ls))))))
(define rcadddr (lambda (ls) (rcar (rcdr (rcdr (rcdr ls))))))
(define rcaddar (lambda (ls) (rcar (rcdr (rcdr (rcar ls))))))
(define rcdaaar (lambda (ls) (rcdr (rcar (rcar (rcar ls))))))
(define rcdaadr (lambda (ls) (rcdr (rcar (rcar (rcdr ls))))))
(define rcdaddr (lambda (ls) (rcdr (rcar (rcdr (rcdr ls))))))
(define rcdadar (lambda (ls) (rcdr (rcar (rcdr (rcar ls))))))
(define rcddaar (lambda (ls) (rcdr (rcdr (rcar (rcar ls))))))
(define rcddadr (lambda (ls) (rcdr (rcdr (rcar (rcdr ls))))))
(define rcddddr (lambda (ls) (rcdr (rcdr (rcdr (rcdr ls))))))
(define rcdddar (lambda (ls) (rcdr (rcdr (rcdr (rcar ls))))))

;; [RaList X] -> Nat
(define (rlength ls)
  ;(assert (rlist? ls))
  (let recr ((ls ls))
    (if (kons? ls)
        (+ (kons-size ls) (recr (kons-rest ls)))
        0)))

(define (make-foldl empty? first rest)
  (letrec ((f (lambda (cons empty ls)
                (if (empty? ls) 
                    empty
                    (f cons
                       (cons (first ls) empty) 
                       (rest ls))))))
    f))

(define (make-foldr empty? first rest)
  (letrec ((f (lambda (cons empty ls)
                (if (empty? ls) 
                    empty
                    (cons (first ls)
                          (f cons empty (rest ls)))))))
    f))

;; [X Y -> Y] Y [RaListof X] -> Y
(define rfoldl/1 (make-foldl rnull? rcar rcdr))
(define rfoldr/1 (make-foldr rnull? rcar rcdr))

;; [RaListof X] ... -> [RaListof X]
(define (rappend . lss)
  (cond ((null? lss) rnull)
        (else (let recr ((lss lss))
                (cond ((null? (cdr lss)) (car lss))
                      (else (rfoldr/1 rcons
                                        (recr (cdr lss))
                                        (car lss))))))))

;; [RaListof X] -> [RaListof X]
(define (rreverse ls)
  (rfoldl/1 rcons rnull ls))

;; [RaListof X] Nat -> [RaListof X]
(define (rlist-tail ls i)
  (let loop ((xs ls) (j i))
    (cond ((zero? j) xs)
          (else (loop (rcdr xs) (sub1 j))))))

;; [RaListof X] Nat -> X
;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
(define (rlist-ref ls i)
  ;(assert (< i (rlength ls)))
  (let loop ((xs ls) (j i))
    (if (< j (kons-size xs))
        (tree-ref (kons-size xs) (kons-tree xs) j)
        (loop (kons-rest xs) (- j (kons-size xs))))))

;; [RaListof X] Nat X -> [RaListof X]
(define (rlist-set ls i v)
  (let-values (((_ l*) (rlist-ref/set ls i v))) l*))

;; [X ... -> y] [RaListof X] ... -> [RaListof Y]
;; Takes advantage of the fact that map produces a list of equal size.
(define rmap
  (case-lambda 
    ((f ls)
     (let recr ((ls ls))
       (if (kons? ls)
           (make-kons (kons-size ls) 
                      (tree-map f (kons-tree ls)) 
                      (recr (kons-rest ls)))
           rnull)))
    ((f . lss)
     ;(check-nary-loop-args 'rmap (lambda (x) x) f lss)
     (let recr ((lss lss))
       (cond ((rnull? (car lss)) rnull)
             (else
              ;; IMPROVE ME: make one pass over lss.
              (make-kons (kons-size (car lss))
                         (tree-map/n f (map kons-tree lss))
                         (recr (map kons-rest lss)))))))))


;; [X ... -> Y] [RaListof X] ... -> unspecified
(define rfor-each
  (case-lambda 
    ((f ls)
     (when (kons? ls)
       (tree-for-each f (kons-tree ls))
       (rfor-each f (kons-rest ls))))
    ((f . lss)
     ;(check-nary-loop-args 'rmap (lambda (x) x) f lss)
     (let recr ((lss lss))
       (when (rpair? (car lss))
         (tree-map/n f (map kons-tree lss))
         (recr (map kons-rest lss)))))))

;; [RaListof X] -> [Listof X]
(define (rlist->list x)
  (rfoldr/1 cons '() x))

;; [Listof X] -> [RaListof X]
(define (list->rlist x)
  (fold-right rcons '() x))

;; This code based on code written by Abdulaziz Ghuloum
;; http://ikarus-scheme.org/pipermail/ikarus-users/2009-September/000595.html
(define get-cached
  (let ((h (make-hash-table eq?)))
    (lambda (x)
      (define (f x)
        (cond
         ((pair? x) (rcons (f (car x)) (f (cdr x))))
         ((vector? x) (vector-map f x))
         (else x)))
      (cond
       ((not (or (pair? x) (vector? x))) x)
       ((hash-table-ref h x #f))
       (else
        (let ((v (f x)))
          (hash-table-set! h x v)
          v))))))

(define-syntax rquote
  (syntax-rules ()
    ((rquote datum) (get-cached 'datum)))) 
