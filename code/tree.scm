;; also git clone https://idiomdrottning.org/tree.scm
;; Copyright 2020 Idiomdrottning, MIT licensed. See LICENSE in the repo.
;; For implementation notes, see README in the repo.

(import (srfi-1) (srfi-8) (srfi-26) (srfi 42) (srfi 69) (srfi 71))

(define (atom? elt) (not (pair? elt)))

(define (tree-walk-preorder proc tree)
  (proc tree)
  (when (pair? tree)
   (for-each
    (cut tree-walk-preorder proc <>)
    tree)))

(define (tree-walk-postorder proc tree)
  (when (pair? tree)
   (for-each
    (cut tree-walk-postorder proc <>)
    tree))
  (proc tree))

(define (spair? tree)
  (if (pair? tree) tree '()))

(define (tree-walk-breadth-first proc tree)
  (proc tree)
  (let desc ((tree tree))
    (when (pair? tree)
      (for-each proc tree)
      (desc (append-map spair? tree)))))

(define (tree? tree)
  (let ((ht (make-hash-table eq?)))
    (call-with-current-continuation
     (lambda (break)
       (tree-walk-preorder
	(lambda (a)
	  (when (pair? a)
	   (if (hash-table-exists? ht a)
	       (break #f)
	       (hash-table-set! ht a #t))))
	tree)
       #t))))

(define (tree=? same? tree1 tree2)
  (call-with-current-continuation
   (lambda (break)
     (let desc ((tree1 tree1) (tree2 tree2))
       (cond
	((and (pair? tree1) (pair? tree2))
	 (for-each desc tree1 tree2))
	((same? tree1 tree2) #t)
	(else (break #f))))
     #t)))

(define (tree-map proc tree)
  (cond
   ((null? tree) '())
   ((atom? tree) (proc tree))
   (else (map (cut tree-map proc <>) tree))))

(define (tree-copy tree)
  (if (atom? tree) tree (map tree-copy tree)))

(define (tree-count pred tree)
  (let ((c 0))
    (tree-walk-preorder
     (lambda (node)
       (when (pred node)
	 (set! c (+ 1 c))))
     tree)
    c))

(define (tree-size tree)
  (tree-count atom? tree))

(define (tree-any? pred tree)
  (call-with-current-continuation
   (lambda (break)
     (tree-walk-preorder
      (lambda (node)
	(when (pred node)
	  (break #t)))
      tree) #f)))

(define (tree-every? pred tree)
  (not (tree-any? (compose not pred) tree)))

(define (tree-find pred tree default)
  (call-with-current-continuation
   (lambda (break)
     (tree-walk-preorder
      (lambda (node)
	(when (pred node)
	  (break node)))
      tree) default)))

(define (tree-find-equal? tree needle)
  (tree-find (cut equal? needle <>) tree #f))

(define (tree-atoms-any? pred tree)
  (call-with-current-continuation
   (lambda (break)
     (tree-walk-preorder
      (lambda (node)
	(when (and (atom? node) (pred node))
	  (break #t)))
      tree) #f)))

(define (tree-atoms-every? pred tree)
  (not (tree-atoms-any? (compose not pred) tree)))

(define (invert-tree tree)
  (let ((inversion (make-hash-table eqv?)))
    (hash-table-set! inversion tree (list #f 0 #f))
    (let desc ((tree tree) (num 1))
      (do-ec
       (:list subtree (index i) tree)
       (if (pair? subtree))
       (begin
	 (hash-table-set! inversion subtree (list tree num i))
	 (desc subtree (add1 num)))))
    inversion))

(define tree-parent (compose first hash-table-ref))
(define tree-depth (compose second hash-table-ref))
(define tree-local-position (compose third hash-table-ref))
(define tree-contains? hash-table-exists?)

(define (tree-c-commands? inversion commanding commanded)
  (let ((parent (tree-parent inversion commanding)))
    (if (= 1 (count pair? parent))
	(tree-c-commands? inversion parent commanded)
	(tree-any? (cut eq? commanded <>) parent))))

(define (tree-path inversion subtree)
  (if
   (tree-contains? inversion subtree)
   (cons subtree
	 (let desc ((parent (tree-parent inversion subtree)))
	   (if parent
	       (cons parent (desc (tree-parent inversion parent)))
	       '())))
   #f))

(define (list-replace lis old new)
  (if (eq? lis old)
      new
      (map
       (lambda (elt) (if (eq? elt old) new elt))
       lis)))

(define (tree-replace-on-path path subtree newnode)
  (fold
   (lambda (lis old)
     (set! newnode (list-replace lis old newnode))
     lis)
   subtree
   (cdr path))
  newnode)

(define (tree-replace-on-path-with-values path subtree newnode)
  (if (null? path) (values subtree newnode)
      (values
       (fold
	(lambda (lis old)
	  (set! newnode (list-replace lis old newnode))
	  lis)
	subtree
	(cdr path))
       newnode)))

(define (tree-replace inversion subtree newnode)
  (tree-replace-on-path (tree-path inversion subtree) subtree newnode))

(define (tree-add inversion subtree newnode)
  (tree-replace inversion subtree (append subtree (list newnode))))

(define (tree-insert inversion subtree index newnode)
  (tree-replace inversion subtree
		(receive (before after)
		    (split-at subtree index)
		  (append before (list newnode) after))))

(define (tree-prune inversion subtree)
  (let ((parent (tree-parent inversion subtree)))
    (if parent
     (tree-replace inversion parent
                   (remove (cut eq? subtree <>) parent))
     '())))

(define (find-shared-tail lisa lisb)
  (let* ((lena (length lisa))
	 (lenb (length lisb)))
    (let desc
	((lisa (if (> lena lenb) (take-right lisa lenb) lisa))
	 (lisb (if (< lena lenb) (take-right lisb lena) lisb)))
      (if (eq? (car lisa) (car lisb))
	  lisa
	  (desc (cdr lisa) (cdr lisb))))))

(define (tree-move inversion subtree newparent)
  (let* ((old-parent (tree-parent inversion subtree))
	 (full-old-parent-path (tree-path inversion old-parent))
	 (full-add-path (tree-path inversion newparent))
	 (shared-tail (find-shared-tail full-old-parent-path full-add-path))
	 (lenst (length shared-tail))
	 (old-parent prune-new
	  (tree-replace-on-path-with-values
	   (drop-right full-old-parent-path lenst)
	   old-parent (remove (cut eq? subtree <>) old-parent)))
	 (newparent parent-with-child
	  (tree-replace-on-path-with-values
	   (drop-right full-add-path lenst)
	   (if (memq newparent full-old-parent-path)
	    (tree-prune (invert-tree newparent) subtree)
	    newparent)
	   (append newparent (list subtree)))))
    (tree-replace-on-path
     shared-tail
     (car shared-tail)
     (list-replace
      (list-replace
       (car shared-tail)
       old-parent
       prune-new)
      newparent parent-with-child))))

(define (composite-tree-move inversion subtree newparent)
  (tree-add (invert-tree (tree-prune inversion subtree))
	    (if (memq newparent (tree-path inversion subtree))
		(tree-prune (invert-tree newparent) subtree)
		newparent)
	    subtree))

(define (display2 thing port)
  (if (null? port)
      (display thing)
      (display thing (car port))))

(define (tree-display-atoms tree separator . port)
  (define (spacer)
    (set! spacer
	  (lambda () (display2 separator port))))
  (tree-walk-breadth-first
   (lambda (node)
     (when (atom? node)
       (spacer)
       (display2 node port)))
   tree))
