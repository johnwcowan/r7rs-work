= Keyword Arguments = 

Some people have discussed Keyword arguments for WG1 and WG2. There are a few approaches that you can take when dealing with keyword arguments:

  1. Create a separate reader syntax for keyword arguments. I believe that these can be first class or not.
  1. Slice a namespace from the symbol space and use if for keyword objects.
  1. Just use symbols for keywords.
  1. Use separate keyword objects but do not have any explicit reader syntax for them.

I do not use keywords usually, but if I did need to use them, I would implement them using the fourth option. These are my reasons:

  * They don't eat up my symbol space for when I don't use keywords. 
  * They are a separate datatype, and so they can be recognized as being intended as keywords, rather than conflating symbols and keywords.

In other words, I get the benefits of having separate keywords without eating up the namespace when I don't use them. This also gives me the liberty to choose my own naming convention as I see fit. 

Here is a naive implementation of this idea:

{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyword Arguments and Lambda
;;; Version: 0.1
;;; 
;;; Copyright (c) 2010 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(library (arcfide keywords types)
  (export keyword? keyword=? make-keyword)
  (import (rnrs base) (rnrs records syntactic))

(define-record-type keyword
  (fields name)
  (opaque #t))

(define (keyword=? x y)
  (eq? (keyword-name x) (keyword-name y)))

)

(library (arcfide keywords lambda)
  (export lambda-opt define/optional)
  (import (rnrs base) 
          (arcfide keywords types)
          (only (chezscheme)
            assp))

(define (make-keyword-matcher name)
  (let ([[key|(make-keyword name)]])
    (lambda (x)
      (and (keyword? x)
           (keyword=? x key)))))

(define (find-keyword-arg arg-alist keyword default)
  (let ([[res|(assp (make-keyword-matcher keyword) arg-alist)]])
    (if res (cdr res) default)))

(define (arglist->alist args)
  (if (null? args)
      '()
      (cons (cons (car args) (cadr args))
            (arglist->alist (cddr args)))))

(define-syntax lambda-opt
  (syntax-rules ()
    [[(_|formals b1 b2 ...)
     (%lambda-opt formals () b1 b2 ...)]]))

(define-syntax %lambda-opt
  (syntax-rules ()
    [[(_|((key def) ...) (id ...) body ...)
     (lambda (id ... . rest)
       (bind-optionals rest ((key def) ...) body ...))]]
    [[(_|(nid rest ...) (id ...) body ...)
     (%lambda-opt (rest ...) (id ... nid) body ...)]]))

(define-syntax bind-optionals
  (syntax-rules ()
    [[(_|args ((key def) ...) body ...)
     (let ([arg-alist (arglist->alist args)]])
       (let ([[key|(find-keyword-arg arg-alist 'key def)]]
             ...)
         body ...))]))

(define-syntax define/optional
  (syntax-rules ()
    [[(_|(name args ...) b1 b2 ...)
     (begin
       (define-keywords args ...)
       (define name
         (lambda-opt (args ...) b1 b2 ...)))]]))

(define-syntax define-keywords
  (syntax-rules ()
    [[(_)|(begin)]]
    [[(_|(key def) rest ...)
     (begin
       (define key (make-keyword 'key))
       (define-keywords rest ...))]]
    [[(_|id rest ...)
     (define-keywords rest ...)]]))

)

(library (arcfide keywords)
  (export define/optional lambda-opt make-keyword)
  (import (arcfide keywords types) (arcfide keywords lambda)))
}}}

Currently there are a few problems with this version.

  * Keyword arguments must come at the end.
  * Keyword definition must come at the end.
  * Keywords are defined in the run-time space, when they really should be an identifier syntax.
  * It is possible to send useless optional arguments and they are ignored rather than throwing an error.

Ideally, I would want to be able to put optional arguments anywhere I wanted. Only the relative positions of non-optional arguments matters. This would require more code, however, and would detract from the overall clarity of the approach. I leave it up to the reader to develop this idea further.

I want to use this illustration to point out the utility of opaque records, as well as demonstrating that syntax and records can go a long way to deal with a lot of these issues, and so I wish to avoid needlessly complicating the language with additional features that are not really necessary, and may in fact be inferior solutions. 

Here is an example use:

{{{
> (define/optional (blah a b c (x: 0) (y: 7)) (list a b c x: y:))
> (blah 1 2 3 y: 5 x: 4) 
(1 2 3 4 5)
> 
}}}