
```

;; Optional inheritance 

(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype <datatype-name> <info> <designation> (<fieldname-spec> ...))
     (define-inherited-datatype <datatype-name> <info> () <designation> (<fieldname-spec> ...)))))

(define-syntax define-inherited-datatype
  (syntax-rules ()
    ;; Base data type
    ((define-inherited-datatype <datatype-name> <info> () <designation> (<fieldname-spec> ...))
     (begin
       ;; We need an informative macro for sub-types
       (define-syntax <info>
         (syntax-rules (create designate fields)
           ((<info> designate) <designation>)
           ((<info> fields) '(<fieldname-spec> ...))
           ((<info> create <self-designation> (<children> (... ...)) (<children-field-specs> (... ...)))
            (define-inherited-datatype-helper (<designation> <children> (... ...))
              <self-designation>
              (<fieldname-spec> ...)
              (<children-field-specs> (... ...))))))
       (define <datatype-name> (<info> create <designation> () ()))))
     
    ((define-inherited-datatype <datatype-name> <info> <parent-datatype-info> <designation> (<fieldname-spec> ...))
     (begin 
       (define-syntax <info>
         (syntax-rules (create designate fields)
           ((<info> designate) <designation>)
           ((<info> fields) '(<fieldname-spec> ...))
           ((<info> create <self-designation> (<children> (... ...)) (<children-field-specs> (... ...)))
            (<parent-datatype-info> create
                                    <self-designation>
                                    (<designation> <children> (... ...))
                                    (<fieldname-spec> ... <children-field-specs> (... ...))))))
     
       (define <datatype-name> (<info> create <designation> () ()))))))


(define-syntax define-inherited-datatype-helper
  (syntax-rules ()
    ((define-inherited-datatype-helper (<ancestor> ...)
                                     <designation>
                                     (<parent-fieldname-spec> ...)
                                     (<fieldname-spec> ...))
     (define-datatype-loop (<ancestor> ...) 
                         <designation>                      
                         (<parent-fieldname-spec> ... <fieldname-spec> ...)
                         () ()))))

```
