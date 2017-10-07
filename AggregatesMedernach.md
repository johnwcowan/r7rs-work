
= AggregatesMedernach =

== Rationale [[see|1]] ==

Declaring and generating fixed sized data type disjoint from all other types, called AGGREGATES.

General other record or object features may be build on top of these aggregates.

== Datatype and associated functions [[see|3]] ==

`(define-datatype `''<datatype-name>''` `''<datatype-info>''` `''<designation>''` (`''<fieldname-spec>''` ...))`

''<datatype-name>'' is bound to a new datatype and ''<datatype-info>'' to a description of this datatype. ''<designation>'' describes the datatype.  (<fieldname-spec> ...) is a list of field specification. A field specification is either:

  *  <field>
  *  (mutable <field>)   Fields require an explicit indication that it should be mutable. 

It is an error to have duplicated fieldnames. 

Field inheritance is optional and could be provided on top of this proposal with the following proposed syntax:

`(define-inherited-datatype `''<datatype-name>''` `''<datatype-info>''` `''<parent-datatype-info>''` `''<designation>''` (`''<fieldname-spec>''` ...))`

Corresponding testing implementations are available at AggregatesMedernachImplementation and AggregatesMedernachInheritanceImplementation.


== Datatype description ==

`(datatype-info->designation `''<datatype-info>''`)`

 Returns type designation of type described by <datatype-info>.

`(datatype-info->fields `''<datatype-info>''`)`

 Returns corresponding list of fields specification of type described by <datatype-info>.


== Aggregates ( constructor / accessor / mutators ) constructor ==

`(create-aggregate-functions `''<datatype>''`)`

If ''<datatype>'' is of datatype kind, then returns corresponding constructor, switch function (see below) and mutators. ''<datatype>'' ensures that the call to create-aggregate-functions is functional, the same aggregate functions are generated if applied to the same ''<datatype>''. 

''create-aggregate-functions'' returns 3 functions as values:

  ''{{{make-<aggregate>}}}'' is a function taking a fixed number of arguments as specified by the ''<datatype>'' and returning a new aggregate containing these arguments as aggregate components.

  ''{{{<aggregate>-switch}}}'' is explained below.

  ''{{{<aggregate>-mutators}}}'' is an association list from fields symbols to corresponding mutators for fields specified as mutators. A mutator function takes 2 arguments: 
  ({{{<mutator>}}} ''<obj>'' ''<val>'') sets corresponding field of ''<obj>'' with ''<val>'', if ''<obj>'' is not of ''<aggregate>'' corresponding datatype kind then an error is signaled.
  

  The data created by make-<aggregate> and selected by <aggregate>-switch are required to conform to ''<my-datatype>'' properties.
  
  With modules it is possible not to export the datatype value nor the generic aggregate functions but only functions defined on top of them. This way one could have finer control of aggregate usage. The idea is to allow flexibility by exposing only interface functions via modules exports and that it is impossible to rebuild aggregate functions if corresponding datatype (with unique designation) is not exported.


== Accessing aggregate components == 

Either
 1. Unsafe access procedures must be invoked after a predicate checking data type
 2. Or safe access procedure, then a check is performed before accessing and an error is signalled if the data type is not what is expected.

However data are aggregated in order to retrieve many part of it and not only one. Solution 2 requires to perform redundant check for each accessed field and moreover the error in general is fatal to the program execution. Solution 1 alone is unsatisfactory as if an unsafe access procedure is applied to not of the correct kind data then random and unwanted behaviors may appear.

Another solution is to group together data type checking with accessing in a case analysis function (per aggregate types) :

  `(<aggregate>-switch `''<aggregate-case>''` `''<else-case>''`) = (lambda (`''<obj>''`) ...)`

Two cases are possible: if the data ''<obj>'' is of ''<aggregate>'' corresponding datatype kind then ''<aggregate-case>'' function is called with the components of the ''<obj>'' data, else ''<else-case>'' is called with ''<obj>''.

The idea behind my-datatype-switch function is to open an environment with bindings for corresponding aggregate components.


== Variants ==

A data is a variant if it is one kind of a list of aggregates.

For instance, one could view the following classic types as variant:

- List as variant of NULL or CONS

- Tree as variant of LEAF or NODE

Convenience macro for variants may be provided as in [[see|2]]:
{{{
(define-syntax variant-case
  (syntax-rules (else)
    ((variant-case <obj>)
     (error "variant-case: all case exhausted " <obj>))
    
    ((variant-case <obj>
        (else <body> ...))
     (begin <body> ...))
    
    ((variant-case <obj>
        (<aggregate-switch> (<var> ...) <body> ...)
        rest ...)
     ((<aggregate-switch>
       (lambda (<var> ...) <body> ...)
       (lambda (<obj>) (variant-case <obj> rest ...)))
      <obj>))))
}}}
  
== SRFI-9 records interface ==
{{{

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type typename
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define-datatype type type-info 'typename ((mutable field-tag) ...))
       
       (define-values-with (type-constructor type-switch mutators)
         (create-aggregate-functions type))

       (define constructor
         (lambda (constructor-tag ...)
           (type-constructor field-tag ...)))

       (define predicate
         (type-switch (lambda (field-tag ...) #t)
                      (lambda (obj) #f)))

       ;; Macro call for accessors and optional modifiers
       (define-record-field type-switch mutators (field-tag ...) field-tag accessor . more)
       ...))))


(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type-switch mutators field-list field-tag accessor)
     (begin
       (define accessor
         (type-switch (lambda field-list field-tag)
                      (lambda (obj) (error "Invalid type: " obj))))))
     
    ((define-record-field type-switch mutators field-list field-tag accessor modifier)
     (begin
       (define accessor
         (type-switch (lambda field-list field-tag)
                      (lambda (obj) (error "Invalid type: " obj))))
        
       (define modifier (cadr (assoc 'field-tag mutators)))))))

}}}

== Various examples ==

Examples ready to be used with test implementation are available at AggregatesMedernachExamples.

== Issues ==

* Maybe it is better to have instead of create-aggregate-functions a syntactic construct to bind the created functions name ?

* An orthogonal mechanism is foreseen to allow securing data (like a lock mechanism with capabilities) but this is not the subject of this page.

== References ==

Disjointness issue raised and proposal:

[1]  Jonathan A. Rees. "User-defined data types". Lisp Pointers. 'The Scheme of Things' (column). 1993 

For variant-case to destructure records:

[2]  Daniel P. Friedman, Mitchell Wand, and Christopher T. Haynes. Essentials of Programming Languages. MIT Press and McGraw-Hill, 1991.

RTD (datatype) functions:

[3]  Jonathan A. Rees, Norman I. Adams IV and James R. Meehan. "The T manual". Yale University Computer Science Department. 1984.
 
