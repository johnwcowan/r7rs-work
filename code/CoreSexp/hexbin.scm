

(define-module (hex)
  #:use-module ((rnrs bytevectors)
		#:select (make-bytevector
			  bytevector-length
			  string->utf8
			  utf8->string
			  (bytevector-u16-native-set! . bv-u16-set!)
			  (bytevector-u16-native-ref . bv-u16-ref)
			  (bytevector-u8-set! . bv-u8-set!)
			  (bytevector-u8-ref . bv-u8-ref)))
  #:use-module (srfi srfi-11)  ;; let-values
  #:use-module (srfi srfi-2)   ;; and-let*
  #:use-module (srfi srfi-60)  ;; bitwise
  #:export (bin->hex
            hex->bin))

;; pick the best way to integer divide by 8 or 2, given that n is
;; known to be a non-negative integer and that the result should round
;; down (so any of floor-quotient, truncate-quotient or
;; euclidean-quotient, or a right shift, would be acceptable).

(define-syntax div8
  (syntax-rules ()
    ((_ n) (ash n -3))))

(define-syntax mod8
  (syntax-rules ()
    ((_ n) (logand n 7))))

(define-syntax div-and-mod8
  (syntax-rules ()
    ((_ n) (let ((nn n))
	     (values (ash nn -3)
		     (logand nn 7))))))

(define-syntax div2
  (syntax-rules ()
    ((_ n) (ash n -1))))

(define-syntax 2*
  (syntax-rules ()
    ((_ n) (ash n 1))))

(define-syntax numeric-for
  (syntax-rules ()
    ((_ (var start limit) body ...)
     (let ((nlimit limit))
       (let loop ((var start))
	 (when (< var nlimit)
	   body ...
	   (loop (1+ var))))))))


(define hex-chars-lc
  (string->utf8 "\
000102030405060708090a0b0c0d0e0f\
101112131415161718191a1b1c1d1e1f\
202122232425262728292a2b2c2d2e2f\
303132333435363738393a3b3c3d3e3f\
404142434445464748494a4b4c4d4e4f\
505152535455565758595a5b5c5d5e5f\
606162636465666768696a6b6c6d6e6f\
707172737475767778797a7b7c7d7e7f\
808182838485868788898a8b8c8d8e8f\
909192939495969798999a9b9c9d9e9f\
a0a1a2a3a4a5a6a7a8a9aaabacadaeaf\
b0b1b2b3b4b5b6b7b8b9babbbcbdbebf\
c0c1c2c3c4c5c6c7c8c9cacbcccdcecf\
d0d1d2d3d4d5d6d7d8d9dadbdcdddedf\
e0e1e2e3e4e5e6e7e8e9eaebecedeeef\
f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))

(define hex-chars-uc
  (string->utf8 "\
000102030405060708090A0B0C0D0E0F\
101112131415161718191A1B1C1D1E1F\
202122232425262728292A2B2C2D2E2F\
303132333435363738393A3B3C3D3E3F\
404142434445464748494A4B4C4D4E4F\
505152535455565758595A5B5C5D5E5F\
606162636465666768696A6B6C6D6E6F\
707172737475767778797A7B7C7D7E7F\
808182838485868788898A8B8C8D8E8F\
909192939495969798999A9B9C9D9E9F\
A0A1A2A3A4A5A6A7A8A9AAABACADAEAF\
B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF\
C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF\
D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF\
E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF\
F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF"))


;; add an optional "uppercase" arg?

(define (bin->hex bin)
  (let* ((bin-len (bytevector-length bin))
	 (hex (make-bytevector (2* bin-len) 0)))
    (numeric-for (i 0 bin-len)
      (bv-u16-set! hex
		   (2* i)
		   (bv-u16-ref hex-chars-lc
			       (2* (bv-u8-ref bin i)))))
    hex))


(define chars-hex
  (letrec ((vec (make-vector 1760 #f))
	   (add-entry (lambda (i n)
			(let-values (((idx1 idx2) (div-and-mod8 (- n #x3000))))
			  (unless (vector-ref vec idx1)
			    (vector-set! vec idx1 (make-vector 8 #f)))
			  (vector-set! (vector-ref vec idx1) idx2 i)))))
    (numeric-for (i 0 256)
      (let* ((lc (bv-u16-ref hex-chars-lc (2* i)))
	     (uc (bv-u16-ref hex-chars-uc (2* i)))
	     (xc1 (bitwise-merge #xff00 lc uc))
	     (xc2 (bitwise-merge #x00ff lc uc)))
	(add-entry i lc)
	(add-entry i uc)
	(add-entry i xc1)
	(add-entry i xc2)))
    vec))


(define (hex->bin hex)
  (let* ((hex-len (bytevector-length hex))
	 (bin-len (div2 hex-len)))
    (unless (even? hex-len)
      (error "Length of hex string must be even"))
    (let ((bin (make-bytevector bin-len 0)))
      (numeric-for (i 0 bin-len)
	(unless (and-let* ((hexv (- (bv-u16-ref hex (2* i)) #x3000))
			   ((> hexv #x0000))
			   ((< hexv #x3700))
			   (idx1 (div8 hexv))
			   (idx2 (mod8 hexv))
			   (t (vector-ref chars-hex idx1))
			   (n (vector-ref t idx2)))
		  (bv-u8-set! bin i n)
		  #t)
	  (error "Invalid character in hex string")))
      bin)))




