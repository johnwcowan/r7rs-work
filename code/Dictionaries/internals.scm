;;;; Internal procedure definitions (all take a vec argument first)

;;; Sample call of an internal procedure from another internal procedure:
;;; (dcall dref/default vec dictionary key default)

;;; Notes on definitions:
;;; Vec argument is not used except to pass to dcalls
;;; External procedures with a rest argument use a list argument here
;;; External procedures with optional arguments are not optional here
 
(define-syntax dcall
  (syntax-rules ()
    ((dcall dproc vec dictionary arg ...)
     ((vector-ref vec dindex) vec dictionary arg ...))))

(define (idictionary? vec obj)
  (error "dictionary? method not defined"))

(define (idict-empty? vec dictionary)
  (= 0 (dcall dsize vec dictionary)))

(define (idict-contains? vec dictionary key)
  (dcall dref vec dictionary key
         (lambda () #f) (lambda (x) #t)))

(define (idict-ref vec dictionary key failure success)
  #f)

(define (idict-ref/default vec dictionary key default)
  #f)

(define (idict-set! vec dictionary . objs)
  #f)

(define (idict-adjoin! vec dictionary . objs)
  #f)

(define (idict-delete! vec dictionary . keys)
  #f)

(define (idict-delete-all! vec dictionary keylist)
  #f)

(define (idict-replace! vec dictionary key value)
  #f)

(define (idict-intern! vec dictionary key failure)
  #f)

(define (idict-update! vec dictionary key updater failure success)
  #f)

(define (idict-update/default! vec dictionary key updater default)
  #f)

(define (idict-pop! vec dictionary failure)
  #f)

(define (idict-map! proc vec dictionary)
  (error "dict-map method not defined"))

(define (idict-filter! pred vec dictionary)  
  (error "dict-filter! method not defined"))

(define (idict-remove! vec pred dictionary)
  #f)

(define (idict-search! vec dictionary key failure success)
  (error "dict-search! method not defined"))

(define (idict-size vec dictionary)
  (error "dict-size method not defined"))

(define (idict-for-each proc vec dictionary)
  (error "dict-for-each method not defined"))

(define (idict-count pred vec dictionary)
  #f)

(define (idict-any pred vec dictionary)
  #f)

(define (idict-every pred vec dictionary)
  #f)

(define (idict-keys vec dictionary)
  #f)

(define (idict-values vec dictionary)
  #f)

(define (idict-entries vec dictionary)
  #f)

(define (idict-fold proc knil vec dictionary)
  #f)

(define (idict-map->list proc vec dictionary)
  #f)

(define (idict->alist vec dictionary)
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
