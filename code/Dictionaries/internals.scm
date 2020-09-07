;;; Internal procedure definitions (all take a vec argument first)

(define (idictionary? obj)
  (error "dictionary? method not defined"))

(define (idict-empty? dictionary)
  #f)

(define (idict-contains? dictionary key)
  #f)

(define idict-ref
  (case-lambda
    ((vec dictionary key)
     (idict-ref* vec dictionary key error values))
    ((vec dictionary key failure)
     (idict-ref* vec dictionary key failure values))
    ((vec dictionary key failure success)
     (idict-ref* vec dictionary key failure success))))

(define (idict-ref* dictionary key failure success)
  #f)

(define idict-ref
  (case-lambda
    ((vec dictionary key)
     (idict-ref* vec dictionary key error values))
    ((vec dictionary key failure)
     (idict-ref* vec dictionary key failure values))
    ((vec dictionary key failure success)
     (idict-ref* vec dictionary key failure success))))

(define (idict-ref* vec dictionary key failure success)
  #f)

(define (idict-ref/default dictionary key default)
  #f)

(define (idict-set! dictionary . objs)
  #f)

(define (idict-adjoin! dictionary . objs)
  #f)

(define (idict-delete! dictionary . keys)
  #f)

(define (idict-delete-all! dictionary keylist)
  #f)

(define (idict-replace! dictionary key value)
  #f)

(define (idict-intern! dictionary key failure)
  #f)

(define idict-update!
  (case-lambda
    ((vec dictionary key updater)
     (idict-update!* vec dictionary key updater error values))
    ((vec dictionary key updater failure)
     (idict-update!* vec dictionary key updater failure values))
    ((vec dictionary key updater failure success)
     (idict-update!* vec dictionary key updater failure success))))

(define (idict-update!* vec dictionary key updater failure success)
  #f)

(define (idict-update/default! dictionary key updater default)
  #f)

(define idict-pop!
  (case-lambda
    ((vec dictionary)
     (idict-pop!* vec dictionary error))
    ((vec dictionary failure)
     (idict-pop!* vec dictionary failure))))

(define (idict-pop!* vec dictionary failure)
  #f)

(define (idict-map! proc dictionary)
  (error "dict-map method not defined"))

(define (idict-filter! pred dictionary)  
  (error "dict-filter! method not defined"))

(define (idict-remove! dictionary pred)
  #f)

(define (idict-search! dictionary key failure success)
  (error "dict-search! method not defined"))

(define (idict-size dictionary)
  (error "dict-size method not defined"))

(define (idict-for-each proc dictionary)
  (error "dict-for-each method not defined"))

(define (idict-count pred dictionary)
  #f)

(define (idict-any pred dictionary)
  #f)

(define (idict-every pred dictionary)
  #f)

(define (idict-keys dictionary)
  #f)

(define (idict-values dictionary)
  #f)

(define (idict-entries dictionary)
  #f)

(define (idict-fold proc knil dictionary)
  #f)

(define (idict-map->list proc dictionary)
  #f)

(define (idict->alist dictionary)
  #f)

(define model-vec #(
  idictionary?  idict-empty?  idict-contains?  idict-ref
  idict-ref/default idict-set!  idict-adjoin!  idict-delete!
  idict-delete-all!  idict-replace!  idict-intern!
  idict-update!  idict-pop!  idict-map!  idict-filter!
  idict-remove!  idict-search!  idict-size idict-for-each
  idict-count idict-any idict-every idict-keys
  idict-values idict-entries idict-fold idict-map->list
  idict->alist))
