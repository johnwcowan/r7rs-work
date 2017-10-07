= Efficiency Concerns and Analysis for Records = 

== Syntactic vs. Procedural Interfaces ==

While procedural interfaces provide a low level, assembly-like basis for records, they have some performance considerations. SRFI-99 attempts to dispel these concerns. However, the arguments for SRFI-99 rely on compilers and code patterns to achieve its efficiency. Absent the presence of a compiler or a recognized/malleable code pattern for optimization, these benefits are not as readily available.

Below is a bit of an analysis concerning the efficiency of procedural records versus syntatic ones.

Using the R6RS record form, let's consider the following transcript:

{{{
Chez Scheme Transcript [[Mon|Apr  5 20:12:47 2010]]
> (library (parent)
    (export p p-x rtd)
    (import (chezscheme))
    (define-record-type p (fields x))
    (define rtd (make-record-type-descriptor 'b #f #f #f #f '#((immutable y)))))
> (expand/optimize '(let ()
    (import (parent))
    (define-record-type a (parent p) (fields y))
    (define-record-type b (parent-rtd rtd (make-record-constructor-descriptor rtd #f #f)))
    (p-x (make-a 1 2))
    (a-y (make-a 2 3))
    ((record-accessor rtd 0) (make-b 3))))
(begin
  (#3%$import-library '(parent) '() 'parent.54)
  (#3%$invoke-library '(parent) '() 'parent.54)
  (let ([[rtd.55|(#3%$make-record-type-descriptor 'a '#<record type p> #f #f #f '#((immutable y))
                  'define-record-type)]])
    (let ([[rtd.56|(#3%$make-record-type-descriptor 'b (#3%$top-level-value 'rtd.57) #f #f #f '#()
                    'define-record-type)]])
      (let ([[rcd.58|(#3%$make-record-constructor-descriptor
                      rtd.56
                      (#3%make-record-constructor-descriptor
                        (#3%$top-level-value 'rtd.57)
                        #f
                        #f)
                      #f
                      'define-record-type)]])
        (let ([[make-b.59|(#3%r6rs:record-constructor rcd.58)]])
          ((#3%$top-level-value 'p-x.60) (#3%$record rtd.55 1 2))
          (let ([[g5.61|(#3%$record rtd.55 2 3)]])
            (#3%$object-ref 'scheme-object g5.61 17))
          ((#3%record-accessor (#3%$top-level-value 'rtd.57) 0)
            (make-b.59 3)))))))
> (let ()
    (import (chezscheme) (parent))
    (define-record-type a (parent p) (fields y))
    (define-record-type b (parent-rtd rtd (make-record-constructor-descriptor rtd #f #f)))
    (define (loop count thunk)
      (do ([[i|0 (fx1+ i)]])
          [[(>=|i count)]]
        (thunk)))
    (define count 90000000)
    (collect (collect-maximum-generation))
    (time
      (loop count
        (lambda ()
         (p-x (make-a 1 2)))))
    (collect (collect-maximum-generation))
    (time
      (loop count
        (lambda ()
          ((record-accessor rtd 0) (make-b 3))))))
(time (loop count ...))
    340 collections
    1249 ms elapsed cpu time, including 29 ms collecting
    1261 ms elapsed real time, including 39 ms collecting
    2880087040 bytes allocated, including 2872405840 bytes reclaimed
(time (loop count ...))
    512 collections
    6601 ms elapsed cpu time, including 52 ms collecting
    6610 ms elapsed real time, including 47 ms collecting
    4320131072 bytes allocated, including 4318493616 bytes reclaimed
> (expand '(let ()
    (import (chezscheme) (parent))
    (define-record-type a (parent p) (fields y))
    (define-record-type b (parent-rtd rtd (make-record-constructor-descriptor rtd #f #f)))
    (define (loop count thunk)
      (do ([[i|0 (fx1+ i)]])
          [[(>=|i count)]]
        (thunk)))
    (define count 90000000)
    (time
      (loop count
        (lambda ()
          (p-x (make-a 1 2)))))
    (time
      (loop count
        (lambda ()
          ((record-accessor rtd 0) (make-b 3)))))))
(begin
  (#3%$import-library '(chezscheme) '() 'scheme)
  (#3%$import-library '(parent) '() 'parent.54)
  (#3%$invoke-library '(parent) '() 'parent.54)
  (letrec* ([[rtd.62|(#3%$make-record-type-descriptor 'a '#<record type p> #f #f #f '#((immutable y))
                      'define-record-type)]]
            [[rcd.63|(#3%$make-record-constructor-descriptor
                      rtd.62
                      '#<record constructor descriptor>
                      #f
                      'define-record-type)]]
            [[make-a.64|(#3%r6rs:record-constructor rcd.63)]]
            [[a?.65|(#3%record-predicate rtd.62)]]
            [[a-y.66|(#3%record-accessor rtd.62 0)]]
            [[rtd.67|(#3%$make-record-type-descriptor 'b (#3%$top-level-value 'rtd.57) #f #f #f '#()
                      'define-record-type)]]
            [[rcd.68|(#3%$make-record-constructor-descriptor
                      rtd.67
                      (#3%make-record-constructor-descriptor
                        (#3%$top-level-value 'rtd.57)
                        #f
                        #f)
                      #f
                      'define-record-type)]]
            [[make-b.69|(#3%r6rs:record-constructor rcd.68)]]
            [[b?.70|(#3%record-predicate rtd.67)]]
            [[loop.71|(lambda (count.72 thunk.73)
                       ((letrec ([do.74 (lambda (i.75)
                                          (if (#3%not
                                                (#3%>= i.75 count.72))
                                              (begin
                                                (thunk.73)
                                                (do.74 (#3%fx1+ i.75)))
                                              (#2%void)))]])
                          do.74)
                         0))]
            [[count.76|90000000]])
    (#3%$as-time-goes-by
      '(loop count (lambda () (p-x (make-a 1 2))))
      (lambda ()
        (loop.71
          count.76
          (lambda ()
            ((#3%$top-level-value 'p-x.60) (make-a.64 1 2))))))
    (#3%$as-time-goes-by
      '(loop
         count
         (lambda () ((record-accessor rtd 0) (make-b 3))))
      (lambda ()
        (loop.71
          count.76
          (lambda ()
            ((#3%record-accessor (#3%$top-level-value 'rtd.57) 0)
              (make-b.69 3))))))))
> (transcript-off)
}}}

Observe that the procedural interface must default into cross-library references that can't be resolved at expand time. On the other hand, the syntactic interface can be resolved and optimized more readily.

On the other hand, if we take advantage of the new, experimental `library-group` form in Chez Scheme version 8.0, then wrapping a library group around the parent and the program code will result in the procedural and syntactic interface running at the same speed. Indeed, this reveals that you can optimize a procedural interface, but you have to have a number of factors in your favor to do so, and it requires more optimization than many Scheme interpreters will have. If you were to run that code on an interpreter or in a compiler that did not do as much optimization -- and Chez does go to some work to make these fast -- then you are struck with a possible 5 times performance hit.

I would appreciate results from other Scheme implementations to fill this out.