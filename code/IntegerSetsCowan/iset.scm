;;;; iset.scm -- integer sets
;;
;; Copyright (c) 2004-2006 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BIT-VECTORS
;;
;; Bit-vectors provide an abstract interface to bitwise operations
;; typically done with integers.  They may in fact be implemented as
;; integers on implementations with bignums, or they may be implemented
;; by other means such as vectors which may be more efficient.  These
;; vectors are meant to be used as flags and sets, not integer values,
;; and thus are operations are ones-complement (there are no negative
;; bit-vectors).
;;
;; The following procedures can be used to create and test bit-vectors:
;;
;; (make-bit-vector size)    ; a 'zero' bit-vector, with a hint that we
;;                           ; wish to use SIZE bits
;; (make-bit-vector size #t) ; same as above but initialize the all bit
;;                           ; elements to #t (= the integer 2^SIZE-1)
;; (integer->bit-vector n)   ; create a bit-vector with bits initialized
;;                           ; to the corresponds bits in fixnum N
;; (bit-vector-copy bv)      ; make a copy of the bit-vector BV
;;
;; (bit-vector? obj)         ; #t iff OBJ is a valid bit-vector, which
;;                           ; is not necessarily a disjoint type
;; (bit-vector-empty? bv)    ; #t iff BV has no bits set (the bit-vector
;;                           ; equivalent of the ZERO? procedure)
;; (bit-vector-full? bv to)  ; #t iff BV has all bits lower than TO set
;;                           ; (the low end is 2^i-1)
;;
;; The individual bits in the vector are accessed and set as boolean
;; values:
;;
;; (bit-vector-ref bv i)     ; #t if the I-th bit in BV is set, else #f
;; (bit-vector-set bv i x)   ; return a new copy of BV with the I-th bit
;;                           ; set to boolean x (off iff X is #f)
;; (bit-vector-set! bv i x)  ; in-place update version of the above, is
;;                           ; allowed but not required to mutate BV
;;
;; The following procedures are direct analogues of the corresponding
;; SRFI-33 bitwise operations:
;;
;; (bit-vector-length bv)    ; integer-length
;;                           ;   the index of the largest bit set in BV
;; (bit-vector-count bv)     ; bit-count
;;                           ;   the number of set bits in BV
;; (bit-vector-shift bv n)   ; arithmetic-shift
;; (bit-vector-and bv ...)   ; bitwise-and
;; (bit-vector-ior bv ...)   ; bitwise-ior
;; (bit-vector-xor bv ...)   ; bitwise-xor
;; (bit-vector-eqv bv ...)   ; bitwise-eqv
;; (bit-vector-nand bv ...)  ; bitwise-nand
;; (bit-vector-nor bv ...)   ; bitwise-nor
;;
;; The following in-place update equivalents are also available, which
;; are allowed, but not required, to mutate their first argument only:
;;
;; (bit-vector-shift! bv n)
;; (bit-vector-and! bv ...)
;; (bit-vector-ior! bv ...)
;; (bit-vector-xor! bv ...)
;; (bit-vector-eqv! bv ...)
;; (bit-vector-nand! bv ...)
;; (bit-vector-nor! bv ...)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGER SETS
;;
;; An integer set is a set of exact integers optimized for minimal space
;; usage and fast membership lookup.  General set operations are
;; provided based on the character set operations found in SRFI-14.
;;
;; Creating isets:
;;
;; (make-iset)     ; an empty integer set
;; (make-iset n)   ; a set of the single integer N
;; (make-iset n m) ; a set of the range of all integers from N-M inclusive
;;
;; The following procedures are provided as direct analogs of the
;; SRFI-14 procedures, accepting and returning isets and integers in
;; place of char-sets and characters:
;;
;; Creating isets:
;;
;; (iset-copy is)            ; a new copy of IS
;; (iset n ...)              ; an iset containing the elements N...
;; (list->iset ls [base-is]) ; an iset containing all the integers in
;;                           ; list LS, union BASE-IS if provided
;; (list->iset! ls base-is)  ; same as above, allowed but not required to
;;                           ; modify base-is
;;
;; Querying isets:
;;
;; (iset-size is)          ; return the # of elements in IS
;; (iset-contains? is n)   ; test N for membership in IS
;; (iset->list is)         ; returns a list of all integers in IS
;; (iset-any pred is)      ; apply PRED to every element of IS, returning
;;                         ; the first element it finds (order unspecified)
;; (iset-every pred is)    ; returns #t if every element of IS satisfies
;;                         ; the predicate PRED (order unspecified)
;;
;; Predicates:
;;
;; (iset? obj)     ; #t iff obj is an integer set
;; (iset= is ...)  ; #t iff all arguments are equivalent integer sets
;; (iset<= is ...) ; #t iff the arguments are monotonically increasing sets
;; (iset>= is ...) ; #t iff the arguments are monotonically decreasing sets
;;
;; Iteration:
;;
;; (iset-fold kons knil is)       ; char-set-fold
;; (iset-unfold f p g [base-is])  ; char-set-unfold
;; (iset-unfold! f p g base-is)   ; char-set-unfold!
;; (iset-for-each proc is)        ; char-set-for-each
;; (iset-map proc is)             ; char-set-for-each
;; (iset-filter pred is [bas-is]) ; char-set-filter
;; (iset-filter! pred is base-is) ; char-set-filter!
;;
;; Cursors:
;;
;; (iset-cursor iset)
;; (iset-ref iset cursor)
;; (iset-cursor-next iset cursor)
;; (end-of-iset? iset)
;;
;; Set operations:
;;
;; (iset-adjoin is n ...)         ; char-set-adjoin
;; (iset-delete is n ...)         ; char-set-delete
;;
;; (iset-adjoin! is n ...)        ; char-set-adjoin!
;; (iset-delete! is n ...)        ; char-set-delete!
;;
;; (iset-union is1 ...)                  ; char-set-union
;; (iset-intersection is1 ...)           ; char-set-intersection
;; (iset-difference is1 is2 ...)         ; char-set-difference
;; (iset-xor is1 ...)                    ; char-set-xor
;; (iset-diff+intersection is1 is2 ...)  ; char-set-diff+intersection
;;
;; (iset-union! is1 ...)                 ; char-set-union!
;; (iset-intersection! is1 ...)          ; char-set-intersection!
;; (iset-difference! is1 is2 ...)        ; char-set-difference!
;; (iset-xor! is1 ...)                   ; char-set-xor!
;; (iset-diff+intersection! is1 is2 ...) ; char-set-diff+intersection!

(module iset
 (
  ;; bit-vectors
  make-bit-vector integer->bit-vector bit-vector-copy
  bit-vector? bit-vector-empty? bit-vector-full?
  bit-vector-ref bit-vector-set bit-vector-set!
  bit-vector-count bit-vector-length bit-vector-shift bit-vector-shift!
  bit-vector-and bit-vector-and! bit-vector-ior bit-vector-ior!
  bit-vector-xor bit-vector-xor! bit-vector-eqv bit-vector-eqv!
  bit-vector-nand bit-vector-nand! bit-vector-nor bit-vector-nor!
  ;; isets
  make-iset iset iset? iset-copy list->iset list->iset! iset->list
  iset= iset<= iset>= iset-start iset-end iset-bits iset-left iset-right
  set-iset-start! set-iset-end! set-iset-bits! set-iset-left! set-iset-right!
  iset-empty? iset-contains? iset-adjoin iset-adjoin! iset-delete iset-delete!
  iset-cursor iset-ref iset-cursor-next end-of-iset?
  iset-fold iset-unfold iset-unfold! iset-for-each iset-map
  iset-filter iset-filter! iset-every iset-any iset-size
  iset-union! iset-union iset-intersection! iset-intersection
  iset-difference! iset-difference iset-xor! iset-xor
  iset-diff+intersection! iset-diff+intersection
  ;; low-level utilities
  %make-iset iset-dump iset-write-code
  iset-balance iset-balance! iset-optimize iset-optimize!
  )

 (import scheme (chicken base) (chicken port) (chicken fixnum) (chicken bitwise) (chicken format) srfi-4)
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; u8vector utils

(cond-expand
 ((and chicken compiling)
  (declare
   (foreign-declare 
    "#define C_u8peek(b, i) C_fix(((unsigned char *)C_data_pointer(b))[ C_unfix(i) ])
     #define C_u8poke(b, i, x) ((((unsigned char *)C_data_pointer(b))[ C_unfix(i) ] = C_unfix(x)), C_SCHEME_UNDEFINED)"))
  (define (u8-ref v i)
    (##core#inline "C_u8peek" (##core#inline "C_slot" v 1) i))
  (define (u8-set! v i x)
    (##core#inline "C_u8poke" (##core#inline "C_slot" v 1) i x)))
 (else
  (define-inline (u8-ref v i) (u8vector-ref v i))
  (define-inline (u8-set! v i x) (u8vector-set! v i x))))

;; from SRFI-43 vector-copy!
(define (u8vector-copy! dst off src . opt)
  (let-optionals* opt ((start 0) (end (u8vector-length src)))
    (do ((i start (+ i 1))
         (j off (+ j 1)))
        ((= i end))
      (u8vector-set! dst j (u8vector-ref src i)))))

;; from SRFI-43 vector-fill!
(define (u8vector-fill! vec fill . opt)
  (let-optionals* opt ((start 0) (end (u8vector-length vec)))
    (do ((i start (+ i 1)))
        ((= i end))
      (u8vector-set! vec i fill))))

;; shift elements of the vector, padding on either end with 0
(define (u8vector-shift! vec n)
  (let ((len (u8vector-length vec)))
    (if (positive? n)
      (do ((from (- len n 1) (- from 1))
           (to (- len 1) (- to 1)))
          ((< from 0) ; zero pad
           (u8vector-fill! vec 0 0 (+ to 1)))
        (u8vector-set! vec to (u8vector-ref vec from)))
      (do ((from (- n) (+ from 1))
           (to 0 (+ to 1)))
          ((= from len) ; zero pad
           (u8vector-fill! vec 0 to))
        (u8vector-set! vec to (u8vector-ref vec from))))))

(define-syntax u8vector-map2!
  (syntax-rules ()
    ((u8vector-map2! proc a b len)
     (do ((i 0 (fx+ i 1)))
         ((fx= i len) a)
       (u8-set! a i (proc (u8-ref a i) (u8-ref b i)))))))

(define-syntax u8vector-map!
  (syntax-rules ()
    ((u8vector-map! proc pad a rest)
     (cond
      ((not (u8vector? a))
       (error "u8vector-map!: not a u8vector" a))
      (else
       (let lp ((a a)
                (a-len (u8vector-length a))
                (ls rest))
         (cond
          ((null? ls)
           a)
          ((not (u8vector? (car ls)))
           (error "u8vector-map!: not a u8vector" (car ls)))
          (else
           (let* ((b (car ls))
                  (b-len (u8vector-length b)))
             (cond
              ((fx> b-len a-len)
               (let ((a2 (make-u8vector b-len 0)))
                 (u8vector-copy! a2 0 a)
                 (lp a2 b-len ls)))
              (else
               (u8vector-map2! proc a b b-len)
               (if (fx< b-len a-len) (pad a b-len a-len))
               (lp a a-len (cdr ls)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitwise-operators (single-byte specific versions)

(define (bit-count i) ; from SRFI-33, logcount in CL
  (u8vector-ref
   '#u8(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4 1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5
        1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5 2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
        1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5 2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
        2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
        1 2 2 3 2 3 3 4 2 3 3 4 3 4 4 5 2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6
        2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
        2 3 3 4 3 4 4 5 3 4 4 5 4 5 5 6 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7
        3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7 4 5 5 6 5 6 6 7 5 6 6 7 6 7 7 8)
   i))

;; these all return non-negative values in [0..255]

(define-inline (byte-not x)
  (- #b11111111 x))

(define-inline (byte-eqv a b)
  (byte-not (fxxor a b)))

(define-inline (byte-nand a b)
  (byte-not (fxand a b)))

(define-inline (byte-nor a b)
  (byte-not (fxior a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-vectors (pass & return #t/#f instead of 1/0)

(define (make-bit-vector size . o)
  (let* ((fill (if (and (pair? o) (car o)) #b11111111 0))
         (len (quotient (+ size 7) 8))
         (res (make-u8vector len fill)))
    (if (zero? fill)
      res
      (let ((off (remainder size 8)))
        (if (not (zero? off))
          (u8vector-set! res (- len 1) (- (arithmetic-shift 1 off) 1)))
        res))))

(define (integer->bit-vector n)
  (let lp ((n n) (ls '()))
    (if (zero? n)
      (list->u8vector (reverse ls))
      (lp (quotient n 256) (cons (remainder n 256) ls)))))

(define (bit-vector-copy vec)
  (let ((len (u8vector-length vec)))
    (if (zero? len)
      (u8vector)
      (subu8vector vec 0 len))))

;; grow to allow setting the i-th element (size = i+1)
(define (bit-vector-grow! vec i)
  (let ((bytes (quotient (+ i 8) 8))
        (len (u8vector-length vec)))
    (if (<= bytes len)
      vec
      (let ((res (make-u8vector bytes 0)))
        (u8vector-copy! res 0 vec)
        res))))

(define (bit-vector-grow vec i)
  (let ((bytes (quotient (+ i 8) 8))
        (len (u8vector-length vec)))
    (if (<= bytes len)
      (bit-vector-copy vec)
      (let ((res (make-u8vector bytes 0)))
        (u8vector-copy! res 0 vec)
        res))))

(define bit-vector? u8vector?)

(define (bit-vector-ref vec i)
  (let ((byte (quotient i 8))
        (off (remainder i 8)))
    (and (< byte (u8vector-length vec))
         (not (zero? (bitwise-and (u8vector-ref vec byte)
                                  (arithmetic-shift 1 off)))))))

(define (bit-vector-set! vec i x)
  (let ((byte (quotient i 8))
        (off (remainder i 8))
        (len (u8vector-length vec)))
    (cond
      ((< byte len)
       (u8vector-set! vec byte
                      (if x
                        (bitwise-ior (u8vector-ref vec byte)
                                     (arithmetic-shift 1 off))
                        (bitwise-and (u8vector-ref vec byte)
                                     (bitwise-not (arithmetic-shift 1 off)))))
       vec)
      ((not x) vec)
      (else (bit-vector-set! (bit-vector-grow vec i) i x)))))

(define (bit-vector-set vec i x)
  (if (or (not x) (< (quotient i 8) (u8vector-length vec)))
    (bit-vector-set! (bit-vector-copy vec) i x)
    (bit-vector-set! (bit-vector-grow vec i) i x)))

(define (bit-vector-length vec)
  (let lp ((i (- (u8vector-length vec) 1)))
    (if (negative? i)
      0
      (let ((x (u8vector-ref vec i)))
        (if (zero? x)
          (lp (- i 1))
          (+ (* 8 i) (integer-length x)))))))

(define (bit-vector-count vec) ; # of 1's
  (let lp ((i (- (u8vector-length vec) 1))
           (acc 0))
    (if (< i 0)
      acc
      (lp (- i 1) (+ acc (bit-count (u8vector-ref vec i)))))))

(define (u8vector-not! v lo hi)
  (do ((i lo (+ i 1)))
      ((= i hi))
    (u8-set! v i (byte-not (u8-ref v i)))))

(define (bit-vector-and! a . args)
  (u8vector-map! fxand (lambda (a lo hi) (u8vector-fill! a 0 lo hi)) a args))
(define (bit-vector-and a . args)
  (apply bit-vector-and! (bit-vector-copy a) args))

(define (bit-vector-andc2! a . args)
  (u8vector-map! (lambda (i j) (fxand i (fxnot j))) (lambda (a lo hi) a) a args))
(define (bit-vector-andc2 a . args)
  (apply bit-vector-andc2! (bit-vector-copy a) args))

(define (bit-vector-ior! a . args)
  (u8vector-map! fxior (lambda (a lo hi) #f) a args))
(define (bit-vector-ior a . args)
  (apply bit-vector-ior! (bit-vector-copy a) args))

(define (bit-vector-xor! a . args)
  (u8vector-map! fxxor (lambda (a lo hi) #f) a args))
(define (bit-vector-xor a . args)
  (apply bit-vector-xor! (bit-vector-copy a) args))

(define (bit-vector-eqv! a . args)
  (u8vector-map! byte-eqv u8vector-not! a args))
(define (bit-vector-eqv a . args)
  (apply bit-vector-eqv! (bit-vector-copy a) args))

(define (bit-vector-nand! a . args)
  (u8vector-map! byte-nand (lambda (a lo hi) (u8vector-fill! a 0 lo hi)) a args))
(define (bit-vector-nand a . args)
  (apply bit-vector-nand! (bit-vector-copy a) args))

(define (bit-vector-nor! a . args)
  (u8vector-map! byte-nor u8vector-not! a args))
(define (bit-vector-nor a . args)
  (apply bit-vector-nor! (bit-vector-copy a) args))

;; shift in place w/o resizing
(define (bit-vector-shift-in-place! vec n)
  (if (not (zero? n))
    (let ((len (u8vector-length vec)))
      (cond
       ((= len 1)
        (u8vector-set! vec 0 (bitwise-and
                              #b11111111
                              (arithmetic-shift (u8vector-ref vec 0) n))))
       ((positive? n)
        (let* ((byte (quotient n 8))
               (off (remainder n 8))
               (start (- len byte 1)))
          (cond
            ((zero? off)
             (u8vector-shift! vec byte))
            ((>= byte len)
             (u8vector-fill! vec 0))
            (else
             (let* ((lo-mask (- (arithmetic-shift 1 (- 8 off)) 1))
                    (lo-shift off)
                    (hi-mask (byte-not lo-mask))
                    (hi-shift (- off 8))
                    (get-lo
                     (lambda (x)
                       (arithmetic-shift (bitwise-and x lo-mask) lo-shift)))
                    (get-hi
                     (lambda (x)
                       (arithmetic-shift (bitwise-and x hi-mask) hi-shift))))
               (u8vector-set! vec (- len 1) (get-lo (u8vector-ref vec start)))
               (let lp ((from (- start 1)) (to (- len 1)))
                 (if (negative? from)
                   (when (>= to 0)
                     (u8vector-fill! vec 0 0 to))
                   (let* ((from-val (u8vector-ref vec from))
                          (lo (get-lo from-val))
                          (hi (get-hi from-val)))
                     (when (positive? to)
                       (u8vector-set! vec (- to 1) lo))
                     (u8vector-set! vec to (bitwise-ior hi (u8vector-ref vec to)))
                     (lp (- from 1) (- to 1))))))))))
       (else
        (let* ((byte (quotient (- n) 8))
               (off (remainder (- n) 8))
               (s-byte (+ byte 1))
               (s-off (- 8 off))
               (save (u8vector-ref vec byte)))
          ;; shift negative by 1+bytes
          (u8vector-shift! vec (- s-byte))
          ;; shift positive by complement of off
          (bit-vector-shift-in-place! vec s-off)
          ;; reset lo byte
          (u8vector-set!
           vec
           0
           (bitwise-ior (u8vector-ref vec 0)
                        (bitwise-and
                         (arithmetic-shift save (- off))
                         (- (arithmetic-shift 1 s-off) 1)))))))))
  ;; return the vector for convenience
  vec)

(define (bit-vector-shift! vec n)
  (if (positive? n)
    (bit-vector-shift-in-place!
     (bit-vector-grow! vec (+ -1 n (bit-vector-length vec)))
     n)
    (bit-vector-shift-in-place! vec n)))

(define (bit-vector-shift vec n)
  (if (positive? n)
    (bit-vector-shift-in-place!
     (bit-vector-grow vec (+ -1 n (bit-vector-length vec)))
     n)
    (bit-vector-shift-in-place! (bit-vector-copy vec) n)))

(define (range->bit-vector start end)
  (make-bit-vector (+ 1 (- end start)) #t))

(define (bit-vector-empty? vec)
  (let lp ((i (- (u8vector-length vec) 1)))
    (or (< i 0)
        (and (zero? (u8vector-ref vec i))
             (lp (- i 1))))))

(define (bit-vector-full? vec to)
  (let ((limit (quotient to 8))
        (off (remainder to 8))
	(len (u8vector-length vec)))
    (let lp ((i 0))
      (if (= i limit)
	  (or (zero? off)
	      (and (< i len)
		   (let ((mask (- (arithmetic-shift 1 off) 1)))
		     (= mask (bitwise-and mask (u8vector-ref vec i))))))
	  (and (< i len)
	       (= #b11111111 (u8vector-ref vec i))
	       (lp (+ i 1)))))))

;; debugging aid

; (define (bit-vector->string vec)
;   (with-output-to-string
;     (lambda ()
;       (let ((len (u8vector-length vec)))
;         (do ((i 0 (+ i 1)))
;             ((= i len))
;           (display (number->string (u8vector-ref vec i) 2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isets

(define-record-type <iset>
  (%make-iset start end bits left right)
  iset?
  (start iset-start set-iset-start!)
  (end   iset-end   set-iset-end!)
  (bits  iset-bits  set-iset-bits!)
  (left  iset-left  set-iset-left!)
  (right iset-right set-iset-right!))

(define-constant *bits-thresh* 128)  ; within 128 we join into a bit-vector
(define-constant *bits-max* 512)     ; don't make bit-vectors larger than this

(define (make-iset . opt)
  (let-optionals* opt ((start 0)
                       (end start)
                       (bits (if (pair? opt) #f (make-bit-vector 1 #f))))
    (%make-iset start end bits #f #f)))

(define (iset . args)
  (list->iset args))

(define (list->iset! ls iset)
  (for-each (lambda (i) (iset-adjoin1! iset i)) ls)
  iset)

(define (list->iset ls . opt)
  (list->iset! ls (if (pair? opt) (iset-copy (car opt)) (make-iset))))

(define (iset-copy iset)
  (and iset
       (%make-iset
        (iset-start iset)
        (iset-end iset)
        (and-let* ((bits (iset-bits iset))) (bit-vector-copy bits))
        (iset-copy (iset-left iset))
        (iset-copy (iset-right iset)))))

(define (iset-copy-node iset)
  (%make-iset (iset-start iset) (iset-end iset) (iset-bits iset) #f #f))

(define (iset-set-node! a b)
  (set-iset-start! a (iset-start b))
  (set-iset-end! a (iset-end b))
  (set-iset-bits! a (iset-bits b)))

; (define (iset-swap-nodes! a b)
;   (let ((tmp (iset-copy-node a)))
;     (iset-set-node! a b)
;     (iset-set-node! b tmp)))

(define-inline (iset2<= a b)
  (iset-every (lambda (i) (iset-contains? b i)) a))

(define (iset<= . args)
  (or (null? args)
      (let ((rest (cdr args)))
        (or (null? rest)
            (and (iset2<= (car args) (car rest))
                 (apply iset<= rest))))))

(define (iset>= . args)
  (apply iset<= (reverse args)))

(define (iset= . args)
  (and (apply iset<= args)
       (apply iset>= args)))

(define (iset-empty? iset)
  (and (iset? iset)
       (cond ((iset-bits iset) => bit-vector-empty?) (else #f))
       (let ((l (iset-left iset))) (or (not l) (iset-empty? l)))
       (let ((r (iset-right iset))) (or (not r) (iset-empty? r)))))

(define (iset-contains? iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
        (and-let* ((left (iset-left is))) (lp left))
        (let ((end (iset-end is)))
          (if (> n end)
            (and-let* ((right (iset-right is))) (lp right))
            (let ((bits (iset-bits is)))
              (or (not bits)
                  (bit-vector-ref bits (- n start))))))))))

(define (iset-max-end iset)
  (cond ((iset-right iset) => iset-max-end)
        (else (iset-end iset))))

(define (iset-min-start iset)
  (cond ((iset-left iset) => iset-min-start)
        (else (iset-start iset))))

(define (iset-insert-left! iset new)
  (let ((left (iset-left iset)))
    (if (and left (< (iset-end new) (iset-start left)))
      (set-iset-right! new left)
      (set-iset-left! new left)))
  (set-iset-left! iset new))

(define (iset-insert-right! iset new)
  (let ((right (iset-right iset)))
    (if (and right (< (iset-end new) (iset-start right)))
      (set-iset-right! new right)
      (set-iset-left! new right)))
  (set-iset-right! iset new))

(define (iset-squash-bits! iset)
  (and-let* ((bits (iset-bits iset)))
    (if (bit-vector-full? bits (- (iset-end iset) (iset-start iset)))
      (set-iset-bits! iset #f))))

(define (iset-node-clear! iset)
  (if (iset-bits iset)
      (u8vector-fill! (iset-bits iset) 0)
      (set-iset-bits! iset
                      (make-bit-vector (- (iset-end iset) (iset-start iset) -1)
                                       #f))))

;; start and/or end are inside the node, split into:
;;   1. node before start, if any
;;   2. node between start and end
;;   3. node after end, if any
(define (iset-node-split node start end)
  (list (and (< (iset-start node) start)
             (iset-node-extract node (iset-start node) (- start 1)))
        (iset-node-extract node start end)
        (and (> (iset-end node) end)
             (iset-node-extract node (+ end 1) (iset-end node)))))

(define (iset-node-extract node start end)
  (cond
   ((iset-bits node)
    => (lambda (node-bits)
         (let* ((bits
                 (bit-vector-and
                  (bit-vector-shift node-bits (- (iset-start node) start))
                  (range->bit-vector start end)))
                (new-end (min end (+ start (bit-vector-length bits)))))
           (%make-iset start new-end bits #f #f))))
   (else
    (%make-iset (max start (iset-start node))
                (min end (iset-end node))
                #f #f #f))))

(define (iset-adjoin1! iset n)
  (cond
    ((iset-empty? iset)
     (set-iset-start! iset n)
     (set-iset-end! iset n)
     (set-iset-bits! iset #f))
    (else
     (let ((start (iset-start iset))
           (end (iset-end iset))
           (bits (iset-bits iset)))
       (cond
         ((< n start)
          (let ((s-diff (- start n)))
            (if (and-let* ((left (iset-left iset))
                           (m-end (iset-max-end left)))
                  (or (<= n m-end)
                      (< (- n m-end) s-diff)))
              (iset-adjoin1! (iset-left iset) n)
              (cond
                ((and (< s-diff *bits-thresh*)
                      (< (- end n) *bits-max*))
                 (set-iset-start! iset n)
                 (let ((bits2 (bit-vector-shift
                               (or bits (range->bit-vector start end))
                               s-diff)))
                   (bit-vector-set! bits2 0 #t)
                   (set-iset-bits! iset bits2)
                   (iset-squash-bits! iset)))
                (else (iset-insert-left! iset (make-iset n)))))))
         ((> n end)
          (let ((e-diff (- n end)))
            (if (and-let* ((right (iset-right iset))
                           (m-start (iset-min-start right)))
                  (or (>= n m-start)
                      (> (- n m-start) e-diff)))
              (iset-adjoin1! (iset-right iset) n)
              (cond
                ((and (< e-diff *bits-thresh*)
                      (< (- n start) *bits-max*))
                 (set-iset-end! iset n)
                 (set-iset-bits! iset (bit-vector-set (or bits (range->bit-vector start end))
                                                      (- n start)
                                                      #t))
                 (iset-squash-bits! iset))
                (else (iset-insert-right! iset (make-iset n)))))))
         (bits
          (bit-vector-set! bits (- n start) #t)
          (iset-squash-bits! iset)))))))

(define (iset-adjoin-node! a b)
  (cond
    ((iset-empty? a)
     (set-iset-start! a (iset-start b))
     (set-iset-end! a (iset-end b))
     (set-iset-bits! a (iset-bits b)))
    ((not (iset-empty? b))
     (let ((a-start (iset-start a))
           (a-end (iset-end a))
           (a-bits (iset-bits a))
           (b-start (iset-start b))
           (b-end (iset-end b))
           (b-bits (iset-bits b)))
       (cond
         ;;         aaaa...
         ;; ...bbbb
         ((< b-end a-start)
          (let ((near-diff (- a-start b-end))
                (start-diff (- a-start b-start))
                (far-diff (- a-end b-start)))
            (if (and-let* ((left (iset-left a))
                           (m-end (iset-max-end left)))
                  (or (< b-end m-end)
                      (< (- b-end m-end) near-diff)))
              (iset-adjoin-node! (iset-left a) b)
              (cond
                ((and (< near-diff *bits-thresh*)
                      (< far-diff *bits-max*))
                 (set-iset-start! a b-start)
                 (let ((bits (bit-vector-shift
                              (or a-bits (range->bit-vector a-start a-end))
                              start-diff))
                       (lo-bits (or b-bits (range->bit-vector b-start b-end))))
                   (set-iset-bits! a (bit-vector-ior! bits lo-bits))
                   (iset-squash-bits! a)))
                (else (iset-insert-left! a (iset-copy-node b)))))))
         ;; ...aaaa
         ;;         bbbb...
         ((> b-start a-end)
          (let ((near-diff (- b-start a-end))
                (start-diff (- b-start a-start))
                (far-diff (- b-end a-start)))
            (if (and-let* ((right (iset-right a))
                           (m-start (iset-min-start right)))
                  (or (> b-start m-start)
                      (> (- b-start m-start) near-diff)))
              (iset-adjoin-node! (iset-right a) b)
              (cond
                ((and (< near-diff *bits-thresh*)
                      (< far-diff *bits-max*))
                 (set-iset-end! a b-end)
                 (set-iset-bits!
                  a
                  (bit-vector-ior!
                   (or a-bits (range->bit-vector a-start a-end))
                   (bit-vector-shift
                    (or b-bits (range->bit-vector b-start b-end))
                    start-diff)))
                 (iset-squash-bits! a))
                (else (iset-insert-right! a (iset-copy-node b)))))))
         ;; aaaa...
         ;;   bbbb...
         ((> b-start a-start)
          (set-iset-end! a (max a-end b-end))
          (when (or a-bits b-bits)
            (set-iset-bits!
             a
             (bit-vector-ior!
              (or a-bits (range->bit-vector a-start a-end))
              (bit-vector-shift
               (or b-bits (range->bit-vector b-start b-end))
               (- b-start a-start))))
            (iset-squash-bits! a)))
         ;;   aaaa...
         ;; bbbb...
         ((< b-start a-start)
          (set-iset-start! a b-start)
          (set-iset-end! a (max a-end b-end))
          (when (or a-bits b-bits)
            (set-iset-bits!
             a
             (bit-vector-ior!
              (bit-vector-shift
               (or a-bits (range->bit-vector a-start a-end))
               (- a-start b-start))
              (or b-bits (range->bit-vector b-start b-end))))
            (iset-squash-bits! a)))
         ;; aaaa...
         ;; bbbb...
         (else
          (set-iset-end! a (max a-end b-end))
          (when (or a-bits b-bits)
            (set-iset-bits!
             a
             (bit-vector-ior!
              (or a-bits (range->bit-vector a-start a-end))
              (or b-bits (range->bit-vector b-start b-end))))
            (iset-squash-bits! a))))
       ))))

(define (iset-adjoin! iset . ls)
  (list->iset! ls iset))

(define (iset-adjoin iset . ls)
  (list->iset ls iset))

;; delete directly in this node
(define (%iset-delete1! iset n)
  (let ((start (iset-start iset))
        (end (iset-end iset))
        (bits (iset-bits iset)))
    (cond
      (bits
       (bit-vector-set! bits (- n start) #f))
      ((= n start)
       (if (= n end)
         (set-iset-bits! iset '#u8())
         (set-iset-start! iset (+ n 1))))
      ((= n end)
       (set-iset-end! iset (- n 1)))
      (else
       (set-iset-end! iset (- n 1))
       (iset-insert-right! iset (make-iset (+ n 1) end))))))

(define (iset-delete1! iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
        (and-let* ((left (iset-left is)))
          (lp left))
        (let ((end (iset-end is)))
          (if (> n end)
            (and-let* ((right (iset-right is)))
              (lp right))
            (%iset-delete1! is n)))))))

(define (iset-delete! iset . args)
  (for-each (lambda (i) (iset-delete1! iset i)) args)
  iset)

(define (iset-delete iset . args)
  (apply iset-delete! (iset-copy iset) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iteration

(define (iset-fold-node kons knil iset)
  (let lp ((is iset) (acc knil))
    (let* ((acc2 (cond ((iset-left is) => (lambda (left) (lp left acc)))
                       (else acc)))
           (acc3 (kons is acc2)))
      (cond ((iset-right is) => (lambda (right) (lp right acc3)))
            (else acc3)))))

(define (iset-fold kons knil iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (if bits
         (let ((limit (+ 1 (- end start))))
           (let lp ((i 0) (acc acc))
             (if (= i limit)
               acc
               (lp (+ i 1)
                   (if (bit-vector-ref bits i)
                     (kons (+ i start) acc)
                     acc)))))
         (let lp ((i start) (acc acc))
           (if (> i end)
             acc
             (lp (+ i 1) (kons i acc)))))))
   knil iset))

(define (iset->node-list a)
  (reverse (iset-fold-node cons '() a)))

(define (iset-unfold f p g seed . opt) 
  (let ((base-is (if (pair? opt) (iset-copy (car opt)) (make-iset))))
    (iset-unfold! f p g seed base-is)))

(define (iset-unfold! f p g seed base-is)
  (let lp ((seed seed))
    (if (p seed)
      base-is
      (begin
        (iset-adjoin1! base-is (f seed))
        (lp (g seed))))))

(define (iset-for-each-node proc iset)
  (let lp ((is iset))
    (and-let* ((left (iset-left is)))
      (lp left))
    (proc is)
    (and-let* ((right (iset-right is)))
      (lp right))))

(define (iset-for-each proc iset)
  (iset-for-each-node
   (lambda (is)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (if bits
         (let ((limit (+ 1 (- end start))))
           (do ((i 0 (+ i 1)))
               ((= i limit))
             (if (bit-vector-ref bits i)
               (proc (+ i start)))))
         (do ((i start (+ i 1)))
             ((> i end))
           (proc i)))))
   iset))

(define (iset-map proc iset)
  (let ((res (make-iset)))
    (iset-for-each (lambda (i) (iset-adjoin1! res (proc i))) iset)
    res))

(define (iset-filter! proc iset base-is)
  (iset-for-each (lambda (i) (if (proc i) (iset-adjoin1! base-is i))) iset)
  base-is)

(define (iset-filter proc iset . opt)
  (iset-filter! proc iset (if (pair? opt) (iset-copy (car opt)) (make-iset))))

(define (iset->list iset)
  (let ((res '()))
    (iset-for-each (lambda (i) (set! res (cons i res))) iset)
    (reverse res)))

(define (iset-any proc iset)
  (call-with-current-continuation
   (lambda (return)
     (iset-for-each (lambda (i) (cond ((proc i) => return))) iset))))

(define (iset-every proc iset)
  (call-with-current-continuation
   (lambda (return)
     (iset-for-each (lambda (i) (if (not (proc i)) (return #f))) iset)
     #t)))

(define (iset-size iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (+ acc
          (if bits
            (bit-vector-count bits)
            (+ 1 (- end start))))))
   0 iset))

(define (iset-cursor iset)
  (call-with-current-continuation
   (lambda (return)
     (iset-for-each
      (lambda (i)
        (call-with-current-continuation
         (lambda (inside)
           (return (lambda (command)
                     (case command
                       ((ref) i)
                       ((next) (inside #t))
                       ((end?) #f)))))))
      iset)
     (lambda (command)
       (case command
         ((ref next) (error "past end of cursor"))
         ((end?) #t))))))

(define (iset-ref iset cur) (cur 'ref))
(define (iset-cursor-next iset cur) (cur 'next))
(define (end-of-iset? cur) (cur 'end?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these could be _much_ better optimized

(define (iset-union2! a b)
  (iset-for-each-node
   (lambda (is)
     (iset-adjoin-node! a is))
   b))

(define (iset-union! . args)
  (let-optionals* args ((a #f) (b #f) rest)
    (cond
      (b
       (iset-union2! a b)
       (apply iset-union! a rest))
      (a a)
      (else (make-iset)))))

(define (iset-union . args)
  (if (null? args)
    (make-iset)
    (apply iset-union! (iset-copy (car args)) (cdr args))))

(define (iset-intersection2! a b)
  (let lp ((nodes-a (iset->node-list a))
           (nodes-b (iset->node-list b)))
    (cond
     ((null? nodes-a)
      a)
     ((null? nodes-b)
      (iset-node-clear! (car nodes-a))
      (set-iset-right! (car nodes-a) #f)
      a)
     ((> (iset-start (car nodes-b)) (iset-end (car nodes-a)))
      (iset-node-clear! (car nodes-a))
      (lp (cdr nodes-a) nodes-b))
     ((> (iset-start (car nodes-a)) (iset-end (car nodes-b)))
      (lp nodes-a (cdr nodes-b)))
     (else
      (let* ((a (car nodes-a))
             (b (car nodes-b))
             (a-ls (iset-node-split a (iset-start b) (iset-end b)))
             (overlap (cadr a-ls))
             (right (car (cddr a-ls)))
             (b-ls (iset-node-split b (iset-start overlap) (iset-end overlap)))
             (b-overlap (cadr b-ls))
             (b-right (car (cddr b-ls))))
        (set-iset-start! a (iset-start overlap))
        (set-iset-end! a (iset-end overlap))
        (if (iset-bits b-overlap)
            (let ((a-bits (or (iset-bits overlap)
                              (range->bit-vector (iset-start a) (iset-end a))))
                  (b-bits (iset-bits b-overlap)))
              (set-iset-bits! a (bit-vector-and a-bits b-bits)))
            (set-iset-bits! a (iset-bits overlap)))
        (if right
            (iset-insert-right! a right))
        (lp (if right (cons right (cdr nodes-a)) (cdr nodes-a))
            (if b-right (cons b-right (cdr nodes-b)) (cdr nodes-b))))))))

(define (iset-intersection! a . args)
  (let-optionals* args ((b #f) rest)
    (cond
      (b
       (iset-intersection2! a b)
       (apply iset-intersection! a rest))
      (else a))))

(define (iset-intersection a . args)
  (apply iset-intersection! (iset-copy a) args))

(define (iset-difference2! a b)
  (let lp ((nodes-a (iset->node-list a))
           (nodes-b (iset->node-list b)))
    (cond
     ((null? nodes-a) a)
     ((null? nodes-b) a)
     ((> (iset-start (car nodes-b)) (iset-end (car nodes-a)))
      (lp (cdr nodes-a) nodes-b))
     ((> (iset-start (car nodes-a)) (iset-end (car nodes-b)))
      (lp nodes-a (cdr nodes-b)))
     (else
      (let* ((a (car nodes-a))
             (b (car nodes-b))
             (a-ls (iset-node-split a (iset-start b) (iset-end b)))
             (left (car a-ls))
             (overlap (cadr a-ls))
             (right (car (cddr a-ls)))
             (b-ls (iset-node-split b (iset-start overlap) (iset-end overlap)))
             (b-overlap (cadr b-ls))
             (b-right (car (cddr b-ls))))
        (if left
            (iset-insert-left! a left))
        (set-iset-start! a (iset-start overlap))
        (set-iset-end! a (iset-end overlap))
        (if (not (iset-bits b-overlap))
            (iset-node-clear! a)
            (let ((a-bits (or (iset-bits overlap)
                              (range->bit-vector (iset-start a) (iset-end a))))
                  (b-bits (iset-bits b-overlap)))
              (set-iset-bits! a (bit-vector-andc2 a-bits b-bits))))
        (if right
            (iset-insert-right! a right))
        (lp (if right (cons right (cdr nodes-a)) (cdr nodes-a))
            (if b-right (cons b-right (cdr nodes-b)) (cdr nodes-b))))))))

(define (iset-difference! a . args)
  (if (null? args)
    a
    (begin
      (iset-difference2! a (car args))
      (apply iset-difference! a (cdr args)))))

(define (iset-difference a . args)
  (apply iset-difference! (iset-copy a) args))

(define (iset-diff+intersection! . args)
  (let ((diff (apply iset-difference args)))
    (values diff (apply iset-intersection! args))))

(define (iset-diff+intersection . args)
  (values (apply iset-difference args) (apply iset-intersection args)))

(define (iset-xor . args)
  (iset-difference! (apply iset-union args) (apply iset-intersection args)))

(define (iset-xor! . args)
  (let ((union (apply iset-union args)))
    (iset-difference! union (apply iset-intersection! args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimizing representation

(define (iset-balance iset)
  (let ((nodes '()))
    (iset-for-each-node
     (lambda (is) (set! nodes (cons (iset-copy-node is) nodes)))
     iset)
    (let reduce ((nodes (reverse nodes)))
      (let ((len (length nodes)))
        (case len
          ((0) #f)
          ((1) (car nodes))
          (else
           (let ((mid (quotient len 2)))
             (let lp ((i 0) (ls nodes) (left '()))
               (if (= i mid)
                 (let ((res (car ls)))
                   (set-iset-left! res (reduce (reverse left)))
                   (set-iset-right! res (reduce (cdr ls)))
                   res)
                 (lp (+ i 1) (cdr ls) (cons (car ls) left)))))))))))

(define (iset-balance! iset)
  (iset-balance iset))

;; safe to insert left since we've already visited all left ndoes
(define (iset-node-replace! is nodes)
  (when (pair? nodes)
    (iset-set-node! is (car nodes))
    (let loop ((is is) (ls (cdr nodes)))
      (when (pair? ls)
        (iset-insert-left! is (car ls))
        (loop (iset-left is) (cdr ls))))))

(define (iset-node-split-ranges! is ranges)
  (let ((start (iset-start is))
        (end (iset-end is))
        (bits (iset-bits is)))
    (let loop ((ls (reverse ranges)) (nodes '()) (last 0))
      (if (pair? ls)
        (let ((lo (caar ls)) (hi (cdar ls)))
          (loop (cdr ls)
                (cons (make-iset (+ start (* 8 lo)) (+ start (* 8 hi) -1))
                      (if (< last lo)
                        (cons (make-iset (+ start (* 8 last))
                                         (+ start (* 8 lo) -1)
                                         (subu8vector bits last lo))
                              nodes)
                        nodes))
                hi))
        (iset-node-replace!
         is
         (if (< (+ start (* 8 last)) end)
           (cons (make-iset (+ start (* 8 last))
                            end
                            (subu8vector bits last (u8vector-length bits)))
                 nodes)
           nodes))))))

(define (iset-optimize-node! is span)
  (iset-squash-bits! is)
  (and-let* ((bits (iset-bits is))
             (len (u8vector-length bits)))
    (letrec
        ((full
          (lambda (i since ranges)
            (cond
              ((or (>= i len) (not (= 255 (u8vector-ref bits i))))
               (if (>= (- i since) span)
                 (sparse (+ i 1) (cons (cons since i) ranges))
                 (sparse (+ i 1) ranges)))
              (else
               (full (+ i 1) since ranges)))))
         (sparse
          (lambda (i ranges)
            (cond
              ((>= i len)
               (if (pair? ranges)
                 (iset-node-split-ranges! is ranges)))
              ((= 255 (u8vector-ref bits i))
               (full (+ i 1) i ranges))
              (else
               (sparse (+ i 1) ranges))))))
      (sparse 0 '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimize consecutive runs of 255 into ranges.  a single ranged node
;; takes up 5 slots (20 bytes on a 32-bit machine, 40 on 64-bit), so it
;; is more space efficient to switch to a range at this point.  Ranges
;; are faster because there is no need to perform bit-vector tests,
;; however they introduce more nodes which could require a longer
;; traversal.

(define (iset-optimize! iset . opt)
  (let ((span (if (pair? opt) (car opt) 20)))
    (iset-for-each-node (lambda (is) (iset-optimize-node! is span)) iset)
    iset))

(define (iset-optimize iset . opt)
  (apply iset-optimize! (iset-copy iset) opt))

;; write an efficient expression which evaluates to the iset
(define (iset-write-code is . opt)
  (let-optionals* opt ((pretty? #f) (p (current-output-port)))
    (let ((acc (if pretty?
                 (lambda (x) (string-append x "    "))
                 (lambda (x) x)))
          (sep (if pretty? "\n" " ")))
      (let loop ((is is) (indent ""))
        (if is
          (begin
            (fprintf p "~A(%make-iset ~A ~A" indent
                     (iset-start is) (iset-end is))
            (cond ((iset-bits is) => (lambda (bits) (fprintf p " '~A" bits)))
                  (else (display " #f" p)))
            (cond
              ((or (iset-left is) (iset-right is))
               (display sep p)
               (loop (iset-left is) (acc indent))
               (display sep p)
               (loop (iset-right is) (acc indent)))
              (else
               (display " #f #f")))
            (display ")" p))
          (fprintf p "~A#f" indent))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debugging aids

(define (iset-dump iset)
  (with-output-to-string
    (lambda ()
      (display "<iset:")
      (iset-for-each-node
       (lambda (is)
         (let ((start (iset-start is))
               (end (iset-end is))
               (bits (iset-bits is)))
           (cond
             (bits (printf " [~A-~A: ~S]" start end bits))
             ((> end start) (printf " [~A-~A]" start end))
             (else (printf " ~A" start)))))
       iset)
      (display ">"))))

; (define (iset-compact? iset)
;   (call-with-current-continuation
;    (lambda (return)
;      (let ((p-start #f)
;            (p-end #f))
;        (iset-for-each-node
;         (lambda (is)
;           (let ((start (iset-start iset))
;                 (end (iset-end iset)))
;             (if (and p-start
;                      (> (- start p-end) *bits-thresh*)
;                      (< (- end p-start) *bits-max*))
;               (return #f))
;             (set! p-start start)
;             (set! p-end end)))
;         iset))
;      #t)))
)
