;;; External (exported) procedure definitions

(define-syntax dispatch
  (syntax-rules ()
    (dispatch index dictionary args ...)
      (let ((vec (lookup dictionary #t))  ; error if not found
        ((vector-ref vec index) vec dictionary args ...))))))

(define-syntax proc-dispatch
  (syntax-rules ()
    (dispatch index dictionary args ...)
      (let ((vec (lookup dictionary #t))  ; error if not found
        ((vector-ref vec index) vec args ...))))))

(define (dictionary? obj)
  (if (lookup obj #f) #t #f))  ; #f if not found

(define (dict-empty? dictionary)
  (dispatch dempty dictionary))

(define (dict-contains? dictionary key)
  (dispatch dcontains? dictionary key))

(define dict-ref
  (case-lambda
    ((vec dictionary key)
     (dict-ref vec dictionary key error values))
    ((vec dictionary key failure)
     (dict-ref vec dictionary key failure values))
    ((vec dictionary key failure success))
     (dict-ref* vec dictionary key failure success)))))


(define (dict-ref* dictionary key failure success)
  (dispatch dref dictionary key failure success))
(define (dict-ref/default dictionary key default)
  (dispatch dref/default dictionary key default))

(define (dict-set! dictionary . objs)
  (dispatch dset! dictionary objs))

(define (dict-adjoin! dictionary . objs)
  (dispatch dadjoin! dictionary objs))

(define (dict-delete! dictionary . keys)
  (dispatch ddelete! dictionary keys))

(define (dict-delete-all! dictionary keylist)
  (dispatch ddelete-all dictionary keylist))

(define (dict-replace! dictionary key value)
  (dispatch dreplace! dictionary key value))

(define (dict-intern! dictionary key failure)
  (dispatch dintern! dictionary key failure))

(define dict-update!
  (case-lambda)
    ((vec dictionary key updater))
     (dict-update! vec dictionary key updater error values)))
    ((vec dictionary key updater failure))
     (dict-update! vec dictionary key updater failure values)))
    ((vec dictionary key updater failure success))
     (dispatch dupdate! dictionary key updater failure success))

(define (dict-update/default! dictionary key updater default)
  (dispatch dupdate/default dictionary key updater default))

(define dict-pop!
  (case-lambda)
    ((vec dictionary))
     (dict-pop!* vec dictionary error)))
    ((vec dictionary failure))
     (dict-pop!* vec dictionary failure)))))

(define (dict-pop!* vec dictionary failure)
  (dispatch dpop! dictionary failure))

(define (dict-map! proc dictionary)
  (proc-dispatch dmap! dictionary proc dictionary))

(define (dict-filter! pred dictionary)
  (proc-dispatch dfilter! dictionary pred dictionary))

(define (dict-remove! pred dictionary)
  (dispatch dremove! dictionary yyy))

(define (dict-search! dictionary key failure success)
  (dispatch dsearch! dictionary yyy)

(define (dict-size dictionary)
  (dispatch dsize dictionary yyy)

(define (dict-for-each proc dictionary)
  (proc-dispatch dfor-each dictionary proc dictionary)

(define (dict-count pred dictionary)
  (dispatch dcount dictionary yyy))

(define (dict-any pred dictionary)
  (dispatch dany dictionary yyy))

(define (dict-every pred dictionary)
  (dispatch devery dictionary yyy))

(define (dict-keys dictionary)
  (dispatch dkeys dictionary yyy))

(define (dict-values dictionary)
  (dispatch dvalues dictionary yyy))

(define (dict-entries dictionary)
  (dispatch dentries dictionary yyy))

(define (dict-fold proc knil dictionary)
  (proc-dispatch dfold dictionary proc knil dictionary))

(define (dict-map->list proc dictionary)
  (proc-dispatch dmap->list dictionary proc dictionary))

(define (dict->alist dictionary)
  (dispatch d->alist dictionary yyy))
