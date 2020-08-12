(define fl1-sqrt1/2 (fl- 1. (flsqrt 0.5)))

(define (fllog1+-guarded x)
  (if (fl<? (flabs x) fl1-sqrt1/2)
      (fllog1+ x)
      (fllog (fl+ 1. x))))

(define fl-log-of-2 (fllog 2.))
(define flulp-of-one fl-epsilon)
(define fl-error-bound (fl/ flulp-of-one 2.))
(define fllog-error-bound (fllog fl-error-bound))

(define logit-boundary-lo	;logistic(-1)
  (fl/ (flexp -1.) (fl+ 1. (flexp -1.))))
(define logit-boundary-hi	;logistic(+1)
  (fl/ 1. (fl+ 1. (flexp -1.))))

(define logit-exp-boundary-lo		;log logistic(-1)
  (fl- (fllog (fl+ 1. (flexp +1.)))))
(define logit-exp-boundary-hi		;log logistic(+1)
  (fl- (fllog (fl+ 1. (flexp -1.)))))

(define (fl-expm1-guarded x)
  (if (fl<? (flabs x) fl-log-of-2)
      (flexp-1 x)
      (fl- (flexp x) 1.)))

(define (rejigger x worst good?)
  ;; We could use bisection but the initial approximations should be
  ;; good enough that this search should take at most a couple steps.
  (define (good x i)
    (if (>= i 10)
        (error "Floating-point parameters are hosed, can't find 'em!"))
    (let ((x* (fladjacent x worst)))
     (if (good? x*) (good x* (+ i 1)) x)))
  (define (bad x i)
    (if (>= i 10)
        (error "Floating-point parameters are hosed, can't find 'em!"))
    (let ((x* (fladjacent x (fl- worst))))
     (if (good? x*) x* (bad x* (+ i 1)))))
  (if (good? x) (good x 0) (bad x 0)))

(define fl-significand-digits-base-2 (- 1 (flinteger-exponent fl-epsilon)))

(define fl-normal-exponent-max (flinteger-exponent fl-greatest))
(define fl-normal-exponent-min (- 1 fl-normal-exponent-max))

(define fl-subnormal-exponent-min
  (- fl-normal-exponent-min
     (- fl-significand-digits-base-2 1)))

;;TODO 2 here is the radix of floating point. Check if should be taken from somewhere instead
(define fl-smallest-positive-subnormal
  (expt 2. fl-subnormal-exponent-min)) 

(define fl-least-subnormal-exponent-base-e
  (rejigger 
    (fl- (fllog fl-smallest-positive-subnormal) (fllog 2.))
    (fl- fl-greatest)
    (lambda (x) (not (flzero? (flexp x))))))

;;; Logit function: log p/(1 - p).  Defined on [0,1].  Maps a
;;; direct-space probability in [0,1] to a log-odds-space probability
;;; in [-\infty, +\infty].  Inverse of logistic.
;;;
;;; Ill-conditioned near 1/2 and 1; the identity logit(1 - p) =
;;; -logit(p) and the function (logit1/2+ p0) = (logit (+ 1/2 p0)) may
;;; help to rearrange a computation for p in [1/(1 + e), 1 - 1/(1 +
;;; e)].
;;;
;;; This implementation gives relative error bounded by 10 eps.

(define (logit p)
  (unless (real? p)
    (error "expected real"))
  (unless (<= 0 p 1)
    (error "expected parameter to be within [0; 1]"))
  (cond 
    ((nan? p) p)		;Propagate NaN.
	;; logit, continued: p in [0, 1]
	((<= logit-boundary-lo p logit-boundary-hi)
	 ;; Since p = 2p/2 <= 1 <= 2*2p = 4p, the floating-point
	 ;; evaluation of 1 - 2p is exact; the only error arises from
	 ;; division and fllog1+.	 First, note that if logistic(-1) <= p
	 ;; <= logistic(+1), (1 - 2p)/p lies in the bounds of Lemma 4.
	 ;;
	 ;; If division has relative error d0 and fllog1+ has relative
	 ;; error d1, the outcome is
	 ;;
	 ;;	-(1 + d1) log(1 + (1 - 2p) (1 + d0)/p)
	 ;;	= -(1 + d1) (1 + d') log(1 + (1 - 2p)/p)
	 ;;	= -(1 + d1 + d' + d1 d') log(1 + (1 - 2p)/p).
	 ;;
	 ;; where |d'| < 8|d0| by Lemma 4.  The relative error is then
	 ;; bounded by
	 ;;
	 ;;	|d1 + d' + d1 d'|
	 ;;	<= |d1| + 8|d0| + 8|d1 d0|
	 ;;	<= 9 eps + 8 eps^2.
	 ;;
	 (- (fllog1+ (flonum (/ (- 1 (* 2 p)) p)))))
	(else
	 ;; If - has relative error d0, / has relative error d1, and
	 ;; log has relative error d2, then
	 ;;
	 ;;	(1 + d2) log((1 + d0) p/[(1 - p)(1 + d1)])
	 ;;	= (1 + d2) [log(p/(1 - p)) + log((1 + d0)/(1 + d1))]
	 ;;	= log(p/(1 - p)) + d2 log(p/(1 - p))
	 ;;	  + (1 + d2) log((1 + d0)/(1 + d1))
	 ;;	= log(p/(1 - p))*[1 + d2 +
	 ;;	    + (1 + d2) log((1 + d0)/(1 + d1))/log(p/(1 - p))]
	 ;;
	 ;; Since 0 <= p < logistic(-1) or logistic(+1) < p <= 1, we
	 ;; have |log(p/(1 - p))| > 1.  Hence, provided |d0|, |d1| <
	 ;; 1/8, this error is bounded by
	 ;;
	 ;;	|d2 + (1 + d2) log((1 + d0)/(1 + d1))/log(p/(1 - p))|
	 ;;	<= |d2| + |(1 + d2) log((1 + d0)/(1 + d1))/log(p/(1 - p))|
	 ;;	<= |d2| + |(1 + d2) log((1 + d0)/(1 + d1))|
	 ;;	<= |d2| + 4|(1 + d2) (d0 - d1)|, by Lemma 3,
	 ;;	<= |d2| + 4|d0 - d1 + d2 d0 - d1 d0|
	 ;;	<= |d2| + 4|d0| + 4|d1| + 4|d2 d0| + 4|d1 d0|
	 ;;	<= 9 eps + 8 eps^2.
	 ;;
	 (log (/ p (- 1 p))))))

;;; Logistic function: 1/(1 + e^{-x}) = e^x/(1 + e^x).	Maps a
;;; log-odds-space probability in [-\infty, +\infty] into a
;;; direct-space probability in [0,1].	Inverse of logit.
;;;
;;; Ill-conditioned for large x; the identity logistic(-x) = 1 -
;;; logistic(x) and the function (logistic-1/2 x) = (- (logistic x)
;;; 1/2) may help to rearrange a computation.
;;;
;;; This implementation gives relative error bounded by 7 eps.

(define (logistic x)
  (unless (real? x)
    (error "expected real parameter"))
  (cond 
    ((nan? x) x)		;Propagate NaN.
	((< x fl-least-subnormal-exponent-base-e)
	 ;; e^x/(1 + e^x) < e^x < smallest positive subnormal.
	 0.)
	((<= x fllog-error-bound)
	 ;; e^x < eps, so
	 ;;
	 ;;	|e^x - e^x/(1 + e^x)|/|e^x/(1 + e^x)|
	 ;;	<= |1 - 1/(1 + e^x)|*|1 + e^x|
	 ;;	 = |(1 + e^x - 1)/(1 + e^x)|*|1 + e^x|
	 ;;	 = |e^x/(1 + e^x)|*|1 + e^x|
	 ;;	 = |e^x|
	 ;;	 < eps.
	 ;;
	 (exp x))
	((<= x (- fllog-error-bound))
	 ;; e^{-x} > 0, so 1 + e^{-x} > 1, and 0 < 1/(1 + e^{-x}) < 1;
	 ;; further, since e^{-x} < 1 + e^{-x}, we also have 0 <
	 ;; e^{-x}/(1 + e^{-x}) < 1.  Thus, if exp has relative error
	 ;; d0, + has relative error d1, and / has relative error d2,
	 ;; then we get
	 ;;
	 ;;	(1 + d2)/[(1 + (1 + d0) e^{-x})(1 + d1)]
	 ;;	= (1 + d2)/[1 + e^{-x} + d0 e^{-x}
	 ;;			+ d1 + d1 e^{-x} + d0 d1 e^{-x}]
	 ;;	= (1 + d2)/[(1 + e^{-x})(1 + d0 e^{-x}/(1 + e^{-x})
	 ;;				    + d1/(1 + e^{-x})
	 ;;				    + d0 d1 e^{-x}/(1 + e^{-x}))].
	 ;;	= (1 + d2)/[(1 + e^{-x})(1 + d')]
	 ;;	= [1/(1 + e^{-x})] (1 + d2)/(1 + d')
	 ;;
	 ;; where
	 ;;
	 ;;	d' = d0 e^{-x}/(1 + e^{-x})
	 ;;	     + d1/(1 + e^{-x})
	 ;;	     + d0 d1 e^{-x}/(1 + e^{-x}).
	 ;;
	 ;; By Lemma 2 this relative error is bounded by
	 ;;
	 ;;	2|d2 - d'|
	 ;;	 = 2|d2 - d0 e^{-x}/(1 + e^{-x})
	 ;;		- d1/(1 + e^{-x})
	 ;;		- d0 d1 e^{-x}/(1 + e^{-x})|
	 ;;	<= 2|d2| + 2|d0 e^{-x}/(1 + e^{-x})|
	 ;;		+ 2|d1/(1 + e^{-x})|
	 ;;		+ 2|d0 d1 e^{-x}/(1 + e^{-x})|
	 ;;	<= 2|d2| + 2|d0| + 2|d1| + 2|d0 d1|
	 ;;	<= 6 eps + 2 eps^2.
	 ;;
	 (/ 1 (+ 1 (exp (- x)))))
	(else
	 ;; If x > -log eps, then e^{-x} < eps, so the relative error
	 ;; of 1 from 1/(1 + e^{-x}) is
	 ;;
	 ;;	|1/(1 + e^{-x}) - 1|/|1/(1 + e^{-x})|
	 ;;	 = |e^{-x}/(1 + e^{-x})|/|1/(1 + e^{-x})|
	 ;;	 = |e^{-x}|
	 ;;	<= eps.
	 ;;
     1
	 ;(error "inexact result1")
     
     )))


;;; log(e^x + e^y + ...)
;;;
;;; Caller can minimize error by passing inputs ascending from -inf to
;;; +inf.

(define (logsumexp l)
  ;; Cases:
  ;;
  ;; 1. No inputs.  Empty sum is zero, so yield log(0) = -inf.
  ;;
  ;; 2. One input.  Computation is exact.  Preserve it.
  ;;
  ;; 3. NaN among the inputs: invalid operation; result is NaN.
  ;;
  ;; 2. Maximum is +inf, and
  ;;    (a) the minimum is -inf: inf - inf = NaN.
  ;;    (b) the minimum is finite: sum overflows, so +inf.
  ;;
  ;; 3. Maximum is -inf: all inputs are -inf, so -inf.
  ;;
  ;; Most likely all the inputs are finite, so prioritize that case by
  ;; checking for an infinity first -- if there is a NaN, the usual
  ;; computation will propagate it.
  ;;
  ;; Overflow is not possible because everything is normalized to be
  ;; below zero.  Underflow can be safely ignored because it can't
  ;; change the outcome: even if you had 2^64 copies of the largest
  ;; subnormal in the sum, 2^64 * largest subnormal < 2^-900 <<<
  ;; epsilon = 2^-53, and at least one addend in the sum is 1 since we
  ;; compute e^{m - m} = e^0 = 1.
  ;;
  (let ((m (reduce max #f l)))
    (cond ((not (pair? l)) -inf.0)
	  ((not (pair? (cdr l))) (car l))
	  ((and (infinite? m)
		(not (= (- m) (reduce min #f l)))
		(not (any nan? l)))
	   m)
	  (else
	   (+ m
		  (log (reduce + 0 (map (lambda (x) (exp (- x m))) l))))))))


;;; log(1 - e^x), defined only on negative x.  Useful for computing the
;;; complement of a probability in log-space.

(define (log1mexp x)
  (unless (real? x)
    (error "expected real parameter"))
  ;; It is hard to imagine that this function maps any rational
  ;; numbers to rational numbers.  (XXX Proof?)
  ;;
  (let ((x (flonum x)))
    ;; Carve up the interval: arrange for the intermediate quantity
    ;; to always lie in [-1/2, +1/2], since in +/-[1/2, 1] we have
    ;; only fixed-point precision.
    ;;
    (cond ((fl<? x (fl- fl-log-of-2))
	   ;; Let d0 be the error of exp, and d1 the error of fllog1+.
	   ;; Since x <= log(1/2), we have e^x <= 1/2 = 1 - 1/2, and
	   ;; thus 1/2 <= 1 - e^x, so that by Lemma 4,
	   ;;
	   ;;	(1 + d1) log(1 - (1 + d0) e^x)
	   ;;	= (1 + d1) (1 + d') log(1 - e^x)
	   ;;
	   ;; for |d'| < 8|d0|.  Consequently, the relative error is
	   ;; bounded by
	   ;;
	   ;;	|d1| + |d'| + |d1| |d'|
	   ;;	<= 9 eps + 8 eps^2.
	   ;;
	   (fllog1+-guarded (fl- (flexp x))))
	  ((fl<? x 0.)
	   ;; Let d0 be the error of flexp-1, and d1 the error of log.
	   ;; We have:
	   ;;
	   ;;	(1 + d1) log((1 + d0) (1 - e^x))
	   ;;	= (1 + d1) [log(1 + d0) + log(1 - e^x)]
	   ;;	= (1 + d1) [1 + log(1 + d0)/log(1 - e^x)] log(1 - e^x)
	   ;;
	   ;; Since log(1/2) <= x, we have 1/2 <= e^x, or 1 - e^x <=
	   ;; 1 - 1/2 = 1/2, so that log(1 - e^x) < -1/2; hence
	   ;; |1/log(1 - e^x)| <= |1/(-1/2)| = 2.  Finally, |log(1 +
	   ;; d0)| <= 2 |d0| as long as |d0| <= eps < 1/2, so when we
	   ;; collect terms in the relative error, we find it is
	   ;; bounded by
	   ;;
	   ;;	|d1| + 2 |d0/log(1 - e^x)| + 2 |d1 d0/log(1 - e^x)|
	   ;;	<= |d1| + 4 |d0| + 4 |d1 d0|
	   ;;	<= 5 eps + 4 eps^2.
	   ;;
	   (fllog (fl- (fl-expm1-guarded x))))
	  ((flzero? x)
	   ;; Negative infinity.
	   (fl/ -1. 0.))
	  ((fl<=? x +inf.0)
	   ;; Invalid operation.
	   (fl/ 0. 0.))
	   ;; Propagate NaN.
      ((nan? x) x)
	  (else
        (error ""))))) ;;TODO error message


;;; log(1 + e^x)

(define (fllog1+exp x)
  (unless (real? x)
    (error "expected real parameter"))
  ;; It is hard to imagine that this function maps any rational
  ;; numbers to rational numbers.  (XXX Proof?)
  ;;
  (let ((x (flonum x)))
    ;; Carve up the interval to avoid overflow in any intermediate
    ;; quantities and to save computation.
    ;;
    (cond ((fl<? x fl-least-subnormal-exponent-base-e)
	   ;; log(1 + e^x) < e^x < smallest positive subnormal
	   (error "")) ;;TODO error message
	  ((fl<=? x fllog-error-bound)
	   ;; 0 < e^x < eps < 1, so log(1 + e^x) >= e^x/2, and the
	   ;; Taylor series of log(1 + y) converges absolutely at y =
	   ;; e^x, so
	   ;;
	   ;;	|e^x - log(1 + e^x)|/|log(1 + e^x)|
	   ;;	<= |e^x - log(1 + e^x)|/(e^x/2)
	   ;;	 = 2|e^x - \sum_{i=1}^\infty (-1)^{i+1} (e^x)^i/i|/e^x
	   ;;	 = 2|-\sum_{i=2}^\infty (-1)^{i+1} (e^x)^i/i|/e^x
	   ;;	 = 2|-\sum_{i=2}^\infty (-1)^{i+1} (e^x)^{i-1}/i|
	   ;;	 = 2|-\sum_{i=1}^\infty (-1)^{i+2} (e^x)^i/(i + 1)|
	   ;;	 = 2|\sum_{i=1}^\infty (-1)^{i+1} (e^x)^i/(i + 1)|
	   ;;	 = 2|e^x/2 + \sum_{i=2}^\infty (-1)^{i+1} (e^x)^i/(i + 1)|
	   ;;	<= 2|e^x/2|
	   ;;	<= eps.
	   ;;
	   (flexp x))
	  ((fl<=? x (fl/ fllog-error-bound -2.))
	   ;; Let d0 be the error of exp, and d1 the error of fllog1+;
	   ;; then by Lemma 4,
	   ;;
	   ;;	(1 + d1) log(1 + (1 + d0) e^x)
	   ;;	= (1 + d1) (1 + d') log(1 + e^x)
	   ;;
	   ;; for |d'| <= 8|d0|, so the relative error is bounded by
	   ;;
	   ;;	|d1 + d' + d1 d'|
	   ;;	<= |d1| + |d'| + |d1 d'|
	   ;;	<= |d1| + 8|d0| + 8|d0 d1|
	   ;;	<= 9 eps + 8 eps^2
	   ;;
	   ;; provided e^x does not overflow.
	   ;;
	   (fllog1+-guarded (flexp x)))

	  ;; fllog1+exp, continued: x >= log(1/sqrt(eps)) so far
	  ((fl<=? x (fl- fllog-error-bound))
	   ;; If x >= log(1/sqrt(eps)), then e^{-2x} <= eps.
	   ;; Write
	   ;;
	   ;;	log(1 + e^x)
	   ;;	 = log(e^x (1 + e^{-x}))
	   ;;	 = log(e^x) + log(1 + e^{-x})
	   ;;	 = x + log(1 + e^{-x}).
	   ;;
	   ;; From the alternating Taylor series of log(1 + y) at y =
	   ;; e^{-x}, this lies between
	   ;;
	   ;;	x + e^{-x}
	   ;;
	   ;; and
	   ;;
	   ;;	x + e^{-x} - e^{-2x}/2,
	   ;;
	   ;; which are both greater than 1, so the relative error is
	   ;; bounded by the larger of
	   ;;
	   ;;	|x + e^{-x} - (x + e^{-2x}/2)|/|x + e^{-x}|
	   ;;	 = |e^{-2x}/2|/|x + e^{-x}|
	   ;;	<= eps/2.
	   ;;
	   ;; and
	   ;;
	   ;;	|x + e^{-x} - (x + e^{-2x}/2)|/|x + e^{-x} + e^{-2x}/2|
	   ;;	 = |e^{-2x}/2|/|x + e^{-x} + e^{-2x}/2|
	   ;;	<= eps/2,
	   ;;
	   ;; since in both cases the denominator is >=1.  In this
	   ;; range, e^{-x} cannot underflow.
	   ;;
	   (fl+ x (flexp (fl- x))))
      ;propagate NaN
      ((nan? x) x)
	  (else
        (error "inexact result2"))))) ;;TODO error wording


;;; logit(e^t) = log e^t/(1 - e^t)
;;;
;;; Inverse of log logistic.

(define (logit-exp t)
  (unless (real? t)
    (error "expected real parameter"))
  (cond 
    ((nan? t) t)		;Propagate NaN.
	((<= t fllog-error-bound)
	 ;; e^t < eps, so since log(e^t/(1 - e^t)) = t - log(1 - e^t),
	 ;; and |log(1 - e^t)| < 1 < |t|, we have
	 ;;
	 ;;	|t - log(e^t/(1 - e^t))|/|log(e^t/(1 - e^t))|
	 ;;	 = |log(1 - e^t)|/|t - log(1 - e^t)|
	 ;;	<= |log(1 - e^t)|
	 ;;	<= 1/(1 - e^t)
	 ;;	<= 2|e^t|
	 ;;	<= 2 eps.
	 ;;
	 t)
	((<= logit-exp-boundary-lo t logit-exp-boundary-hi)
	 ;; We can use the identity
	 ;;
	 ;;	log(e^t/(1 - e^t))
	 ;;	= -log((1 - e^t)/e^t)
	 ;;	= -log(1 + (1 - e^t)/e^t - 1)
	 ;;	= -log(1 + (1 - e^t - e^t)/e^t)
	 ;;	= -log(1 + (1 - 2 e^t)/e^t)
	 ;;
	 ;; to compute this with fllog1+.
	 ;;
	 ;; Since e^t = 2 e^t/2 <= 1 < 2*2 e^t = 4 e^t, 1 - 2 e^t is
	 ;; without additional error beyond that in e^t.  Further,
	 ;; |e^t/(1 - 2 e^t)| <= 2.  The intermediate division is
	 ;;
	 ;;	(1 - 2 (1 + d0) e^t) (1 + d1)/[(1 + d0) e^t]
	 ;;	= (1 - 2 e^t - 2 d0 e^t) (1 + d1)/[(1 + d0) e^t]
	 ;;	= (1 - 2 e^t) (1 - 2 d0 e^t/(1 - 2 e^t)) (1 + d1)
	 ;;	    / [(1 + d0) e^t]
	 ;;	= [(1 - 2 e^t)/e^t]
	 ;;	  * (1 - 2 d0 e^t/(1 - 2 e^t)) (1 + d1)/(1 + d0)
	 ;;	= [(1 - 2 e^t)/e^t]
	 ;;	  * (1 + d1 - (1 + d1) 2 d0 e^t/(1 - 2 e^t))
	 ;;	  / (1 + d0).
	 ;;
	 ;; By Lemma 2, the relative error d' of the intermediate division
	 ;; is bounded by
	 ;;
	 ;;	2|d0 - d1 + (1 + d1) 2 d0 e^t/(1 - 2 e^t)|
	 ;;	<= 2|d0| + 2|d1| + 2|d0 (1 + d1) e^t/(1 - 2 e^t)|
	 ;;	<= 2|d0| + 2|d1| + 4|d0 (1 + d1)|
	 ;;	 = 2|d0| + 2|d1| + 4|d0 + d0 d1)|
	 ;;	<= 2|d0| + 2|d1| + 4|d0| + 4|d0 d1|
	 ;;	<= 8 eps + 4 eps^2.
	 ;;
	 ;; By Lemma 4, the relative error of using fllog1+ is compounded
	 ;; by no more than 8|d'|, so the relative error of the result
	 ;; is bounded by
	 ;;
	 ;;	|d2| + |d'| + |d2 d'|
	 ;;	<= eps + 8 eps + 4 eps^2 + eps*(6 eps + 4 eps^2)
	 ;;	 = 9 eps + 10 eps^2 + 4 eps^3.
	 ;;
	 (let ((e^t (exp t)))
	   (- (fllog1+ (flonum (/ (- 1 (* 2 e^t)) e^t))))))

	;; logit-exp, continued: t <= -log(1 + e) or -log(1 + 1/e) <= t
	(else
	 ;; We use the identity
	 ;;
	 ;;	log(e^t/(1 - e^t))
	 ;;	= -log((1 - e^t)/e^t)
	 ;;	= -log(e^{-t} - 1)
	 ;;
	 ;; to compute this with flexp-1.
	 ;;
	 ;;	-(1 + d0) log((1 + d1) (e^{-t} - 1))
	 ;;	= -(1 + d0) [log(e^{-t} - 1) + log(1 + d1)]
	 ;;	= -[(1 + d0) log(e^{-t} - 1) + (1 + d0) log(1 + d1)]
	 ;;	= -[log(e^{-t} - 1) + d0 log(e^{-t} - 1)
	 ;;		+ (1 + d0) log(1 + d1) log(e^{-t} - 1)/log(e^{-t} - 1)]
	 ;;	= -log(e^{-t} - 1)
	 ;;	  * (1 + d0 + (1 + d0) log(1 + d1)/log(e^{-t} - 1))
	 ;;
	 ;; If t <= -log(1 + e), then log(e^{-t} - 1) >= 1; similarly,
	 ;; if t >= -log(1 + 1/e), then log(e^{-t} - 1) <= -1.	Hence,
	 ;; in both cases, |log(e^{-t} - 1)| >= 1, so that
	 ;;
	 ;;	|d0 + (1 + d0) log(1 + d1)/log(e^{-t} - 1)|
	 ;;	<= |d0| + |(1 + d0) log(1 + d1)/log(e^{-t} - 1)|
	 ;;	<= |d0| + |(1 + d0) log(1 + d1)|
	 ;;	<= |d0| + |log(1 + d1)| + |d0 log(1 + d1)|
	 ;;	<= |d0| + |1/(1 - |d1|)| + |d0/(1 - d1)|
	 ;;	<= |d0| + 2|d1| + 2|d0 d1|
	 ;;	<= 3 eps + 2 eps^2.
	 ;;
	 (- (log (flexp-1 (- t)))))))


;;; log logistic(x) = log (1/(1 + e^{-x})) = -log (1 + e^{-x})
;;;
;;; This is the log density of the logistic distribution.

(define (log-logistic x)
  (unless (real? x)
    (error "expected real parameter"))
  (- (log1pexp (- x))))


;;; log(1 + e^x)

(define (log1pexp x)
  (unless (real? x)
    (error "expected real parameter"))
  ;; It is hard to imagine that this function maps any rational
  ;; numbers to rational numbers.  (XXX Proof?)
  ;;
  (let ((x (flonum x)))
    ;; Carve up the interval to avoid overflow in any intermediate
    ;; quantities and to save computation.
    ;;
    (cond ((fl<? x fl-least-subnormal-exponent-base-e)
	   ;; log(1 + e^x) < e^x < smallest positive subnormal
	   0.)
	  ((fl<=? x fllog-error-bound)
	   ;; 0 < e^x < eps < 1, so log(1 + e^x) >= e^x/2, and the
	   ;; Taylor series of log(1 + y) converges absolutely at y =
	   ;; e^x, so
	   ;;
	   ;;	|e^x - log(1 + e^x)|/|log(1 + e^x)|
	   ;;	<= |e^x - log(1 + e^x)|/(e^x/2)
	   ;;	 = 2|e^x - \sum_{i=1}^\infty (-1)^{i+1} (e^x)^i/i|/e^x
	   ;;	 = 2|-\sum_{i=2}^\infty (-1)^{i+1} (e^x)^i/i|/e^x
	   ;;	 = 2|-\sum_{i=2}^\infty (-1)^{i+1} (e^x)^{i-1}/i|
	   ;;	 = 2|-\sum_{i=1}^\infty (-1)^{i+2} (e^x)^i/(i + 1)|
	   ;;	 = 2|\sum_{i=1}^\infty (-1)^{i+1} (e^x)^i/(i + 1)|
	   ;;	 = 2|e^x/2 + \sum_{i=2}^\infty (-1)^{i+1} (e^x)^i/(i + 1)|
	   ;;	<= 2|e^x/2|
	   ;;	<= eps.
	   ;;
	   (flexp x))
	  ((fl<=? x (fl/ fllog-error-bound -2.))
	   ;; Let d0 be the error of exp, and d1 the error of log1p;
	   ;; then by Lemma 4,
	   ;;
	   ;;	(1 + d1) log(1 + (1 + d0) e^x)
	   ;;	= (1 + d1) (1 + d') log(1 + e^x)
	   ;;
	   ;; for |d'| <= 8|d0|, so the relative error is bounded by
	   ;;
	   ;;	|d1 + d' + d1 d'|
	   ;;	<= |d1| + |d'| + |d1 d'|
	   ;;	<= |d1| + 8|d0| + 8|d0 d1|
	   ;;	<= 9 eps + 8 eps^2
	   ;;
	   ;; provided e^x does not overflow.
	   ;;
	   (fllog1+-guarded (flexp x)))

	  ;; log1pexp, continued: x >= log(1/sqrt(eps)) so far
	  ((fl<=? x (fl- fllog-error-bound))
	   ;; If x >= log(1/sqrt(eps)), then e^{-2x} <= eps.
	   ;; Write
	   ;;
	   ;;	log(1 + e^x)
	   ;;	 = log(e^x (1 + e^{-x}))
	   ;;	 = log(e^x) + log(1 + e^{-x})
	   ;;	 = x + log(1 + e^{-x}).
	   ;;
	   ;; From the alternating Taylor series of log(1 + y) at y =
	   ;; e^{-x}, this lies between
	   ;;
	   ;;	x + e^{-x}
	   ;;
	   ;; and
	   ;;
	   ;;	x + e^{-x} - e^{-2x}/2,
	   ;;
	   ;; which are both greater than 1, so the relative error is
	   ;; bounded by the larger of
	   ;;
	   ;;	|x + e^{-x} - (x + e^{-2x}/2)|/|x + e^{-x}|
	   ;;	 = |e^{-2x}/2|/|x + e^{-x}|
	   ;;	<= eps/2.
	   ;;
	   ;; and
	   ;;
	   ;;	|x + e^{-x} - (x + e^{-2x}/2)|/|x + e^{-x} + e^{-2x}/2|
	   ;;	 = |e^{-2x}/2|/|x + e^{-x} + e^{-2x}/2|
	   ;;	<= eps/2,
	   ;;
	   ;; since in both cases the denominator is >=1.  In this
	   ;; range, e^{-x} cannot underflow.
	   ;;
	   (fl+ x (flexp (fl- x))))
	  (else
	   ;; If x >= log(1/eps), then e^{-x} <= eps.  As above, write
	   ;;
	   ;;	log(1 + e^x) = x + log(1 + e^{-x}).
	   ;;
	   ;; Note that log(1 + e^{-x}) < e^{-x}, so we are computing
	   ;; a quantity between x and x + eps for x > 1, so the
	   ;; relative error is bounded by eps.
	   ;;
	   ;; If it's NaN, just propagate it; otherwise raise
	   ;; inexact-result.
	   ;;
	   x))))
