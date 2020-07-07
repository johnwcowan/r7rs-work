(import
  (scheme base)
  (srfi 64)
  (compounds))

(test-begin "Compounds")

(test-group "compound, make-compound, compound?, compound-subobjects"
            (define (test c)
              (test-assert (compound? c))
              (test-equal (compound-subobjects c) (list 1 2 3)))
            (test (make-compound (list 1 2 3)))
            (test (make-compound (list (compound 1) (compound 2) (compound 3))))
            (test (compound 1 2 3))
            (test (compound (compound 1) (compound 2) (compound 3))))

(test-group "compound-type?"
            (test-assert (compound-type? '(key)))
            (test-assert (compound-type? '(key (k1 . v1) (k2 . v2))))
            (test-assert (not (compound-type? '())))
            (test-assert (not (compound-type? '("key"))))
            (test-assert (not (compound-type? '(key ("k1" v1)))))
            (test-assert (not (compound-type? "test"))))

(test-group "compound-length"
            (test-equal 3 (compound-length (compound 1 2 3)))
            (test-equal 1 (compound-length 'test)))

(test-group "compound-ref"
            (test-equal 1 (compound-ref (compound 1) 0))
            (test-equal 1 (compound-ref 1 0)))

(test-group "compound-values"
            (define-values
              (a b c)
              (compound-values (compound 1 2 3)))
            (define-values
              (d)
              (compound-values 4))
            (test-equal
              (list a b c)
              (list 1 2 3))
            (test-equal d 4))

(test-group "compound-map"
            (define c (compound 1 2 3))
            (test-equal 
              (compound-subobjects 
                (compound-map 
                  (lambda (e) (+ 1 e)) 
                  c))
              (list 2 3 4))
            (test-equal 
              (compound-subobjects 
                (compound-map 
                  (lambda (e) (compound 0 (+ 1 e))) 
                  c))
              (list 0 2 0 3 0 4))
            (test-equal
              (compound-subobjects
                (compound-map 
                  (lambda (e) 
                    (+ 1 e))
                  1))
              (list 2)))

(test-group "compound-filter"
            (define c (compound 1 2 3))
            (test-equal
              (compound-subobjects
                (compound-filter
                  (lambda (e) (= e 2))
                  c))
              (list 2))
            (test-equal 
              (compound-subobjects
                (compound-filter
                  (lambda (e) (= e 2))
                  2))
              (list 2))
            (test-equal 
              (compound-subobjects
                (compound-filter
                  (lambda (e) (= e 2))
                  1))
              (list)))

(test-group "compound-predicate"
            (define c (compound 1 2 3))
            (test-equal 
              (compound-accessor (lambda (e) (not (= e 2)))
                                 number->string
                                 c
                                 "0")
              "1")
            (test-equal 
              (compound-accessor (lambda (e) #f)
                                 number->string
                                 c
                                 "0")
              "0")
            (test-equal 
              (compound-accessor (lambda (e) (= e 2))
                                 number->string
                                 2
                                 "0")
              "2")
            (test-equal 
              (compound-accessor (lambda (e) (= e 2))
                                 number->string
                                 1
                                 "0")
              "0"))

(test-group "compound-type-properties"
            (define c (compound '(key (k1 . v1))
                                '(key (k2 . v2))))
            (test-equal
              (compound-type-properties 'key c)
              '((k1 . v1)))
            (test-equal
              (compound-type-properties 'key2 c)
              #f)
            (test-equal
              (compound-type-properties 'key '(key (k1 . v1)))
              '((k1 . v1)))
            (test-equal
              (compound-type-properties 'key2 '(key (k1 . v1)))
              #f)
            (test-equal
              (compound-type-properties 'key2 "test")
              #f))

(test-end "Compounds")
