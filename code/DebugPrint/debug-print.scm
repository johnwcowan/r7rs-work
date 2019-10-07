(define (debug-print . objs)
  (if (output-port? (car objs))
    (debug-print* (cdr objs) #t #t (car objs))
    (debug-print* objs #t #t (current-error-port))))

(define ws (string-copy "ws"))
(define wrt (string-copy "wrt"))
(define disp (string-copy "disp"))
(define nonl (string-copy "nonl"))

(define (debug-print* objs display? newline? port)
  (cond
    ((null? objs)
     (if newline? (newline port)))
    ((eq? (car objs) ws)
     (display #\space port)
     (debug-print* (cdr objs) display? newline? port))
    ((eq? (car objs) wrt)
     (debug-print* (cdr objs) #f newline? port))
    ((eq? (car objs) disp)
     (debug-print* (cdr objs) #t newline? port))
    ((eq? (car objs) nonl)
     (debug-print* (cdr objs) display? #f port))
    (else ((if display? display write) (car objs) port)
     (debug-print* (cdr objs) display? newline? port))))
