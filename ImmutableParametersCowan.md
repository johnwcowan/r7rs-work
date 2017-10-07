= Parameters =

''This is once more my current proposal.''

This proposal defines ''parameter objects'', the procedure `make-parameter` to create parameter objects and the `parameterize `special form to dynamically bind parameter objects. In the dynamic environment, each parameter object is bound to a value, which may be any Scheme object. When a procedure is invoked, the called procedure inherits the dynamic environment from the caller. The `parameterize` special form allows the binding of a parameter object to be changed for the dynamic extent of its body.  Parameter objects are disjoint from all Scheme types except procedures.

The proposal is an alternative to ParametersSnellPym, providing immutable parameter objects.  It is a subset of SRFI 39, with the difference being that passing a non-zero number of arguments to a parameter object has undefined effect rather than assigning a new value to the parameter object.  The effect of SRFI 39 mutable parameters can be emulated by binding a mutable object such as a pair to the parameter.

== Rationale ==

The ''dynamic environment'' is the structure which allows the system to find the value to be returned by the R5RS procedures `current-input-port` and `current-output-port`. The R5RS procedures `with-input-from-file` and `with-output-to-file` extend the dynamic environment to produce a new dynamic environment which is in effect for the dynamic extent of the call to the thunk passed as their last argument. These procedures are essentially special-purpose dynamic binding operations on hidden dynamic variables (one for `current-input-port` and one for `current-output-port`). The purpose of this proposal is to generalize this dynamic binding mechanism (which exists in all R5RS-compliant systems) to allow the user to introduce new dynamic variables and dynamically bind them.

General dynamic binding mechanisms exist in several implementations of Scheme under various names, including fluid variables and parameter objects. The parameter objects specified in this proposal are compatible with the semantics of most, but not all, implementations of Scheme that currently support [[http://srfi.schemers.org/srfi-39/srfi-39.html|SRFI-39]].

Parameters are certainly useful:

 1. They can be labor-savers, passing "configuration information" down through complex call stacks without needing explicit parameter passing through functions that do nothing with the parameter other than pass it to all child procedures called, until a lower layer actually uses it.

 2. They allow isolation of concerns; as per the previous point, the intermediate procedures do not need to know what configuration information is sent to which child procedures, which is useful when the child procedures are provided by external libraries, or are arbitrary closures which might invoke arbitrary external libraries.

 3. They allow dynamically scoped state, for tasks like exception handling and thread-local storage.

 4. They generalize a mechanism that must be present within the implementation to support `current-input-port` and `current-output-port`, making it portably available to library authors.

Parameters need to be specified as part of WG1 Scheme rather than implemented by portable libraries, because a portable library has no way of knowing if the implementation provides threads; and if it does, it has no portable way of implementing dynamically scoped state in a thread-safe manner.


== Specification ==

The dynamic environment is composed of two parts: the ''local dynamic environment'' and the ''global dynamic environment''. The global dynamic environment is used to look up parameter objects that can't be found in the local dynamic environment by mutating it. When parameter objects are created, their initial value is put into the global dynamic environment. The local dynamic environment is only extended by the `parameterize` form.

Parameter objects are created with the `make-parameter` procedure. The global dynamic environment is updated to associate the newly created parameter object to a value, which can be retrieved by invoking the parameter object as a procedure with no arguments.

The `parameterize` special form, when given parameter objects and corresponding values, binds for the dynamic extent of its body the each parameter object to a new value. The `parameterize` special form behaves analogously to `let` when binding more than one parameter object: that is, the order of evaluation is unspecified and the new bindings are only visible in the dynamic extent of the `parameterize` special form.  However, while `let` requires the user to specify identifiers to be bound, the parameter objects bound by `parameterize` can be specified using arbitrary Scheme expressions.

This proposal also specifies that `current-input-port` and `current-output-port` are implemented as parameter objects created with `make-parameter`.  Likewise `current-error-port` if it is provided.

== Procedures and syntax ==

`(make-parameter `''init''` `[''converter'']`)`                     ''procedure''

Returns a new parameter object which is bound in the global dynamic environment to the value returned by the call `(`''converter''` `''init''`)`. If ''converter'' is not specified the identity function is used instead.  The conversion procedure can be used to coerce ''init'' to a suitable type or to signal an error if its type is inappropriate.

Invoking the returned parameter object with no arguments returns its current value.  The effect of invoking a parameter object with arguments is undefined.

{{{
        (define radix
          (make-parameter 10))

        (define write-shared
          (make-parameter
            #f
            (lambda (x)
              (if (boolean? x)
                  x
                  (error "only booleans are accepted by write-shared")))))

        (radix)           ==>  10
        (write-shared 0)  ''gives an error''

        (define prompt
          (make-parameter
            123
            (lambda (x)
              (if (string? x)
                  x
                  (with-output-to-string (lambda () (write x)))))))

        (prompt)       ==>  "123"
}}}

`(parameterize ((`''expr1''` `''expr2''`) ...) ''body'')`             ''syntax''

The expressions ''expr1'' and ''expr2'' are evaluated in an unspecified order. The value of each ''expr1'' expression must be a parameter object. For each ''expr1'' expression, the local dynamic environment is extended in an unspecified order with a binding of the parameter object ''expr1'' to the result of the call `(`''converter''` `''val''`)`, where ''val'' is the value of ''expr2'' and ''converter''is the conversion procedure of the parameter object.  The resulting dynamic environment is then used for the evaluation of ''body'', which refers to the R5RS grammar nonterminal of that name. The result(s) of the `parameterize` form are the result(s) of ''body''.  At the end of the body, the former value of the parameter is restored without passing it through the converter procedure again.

{{{
        (radix)                                              ==>  10
        (parameterize ((radix 16)) (radix))                  ==>  16
        (radix)                                              ==>  10

        (define (f n) (number->string n (radix)))

        (f 10)                                               ==>  "10"
        (parameterize ((radix 8)) (f 10))                    ==>  "12"
}}}


== Interaction with threads ==

SRFI 39 does not specify how parameter objects interact with threads.  ParametersSnellPym provides mutable parameters shared between threads, which threads may atomically mutate to share information.  I take the view that using parameter objects in this way is an improper and unnecessary conflation of two distinct facilities, dynamic variables and shared data.  Instead, some mechanism for exchanging data between threads, such as mailboxes, should be provided as a part of standardizing threads, probably in WG2.

Therefore, this proposal specifies that when a parent thread creates a new child thread, the parameter objects in the child thread have ''initial'' values that are the same (in the sense of `eqv?`) as their ''current'' (not initial) values in the parent thread.  Because parameter objects are immutable, it does not matter whether the underlying representation uses copying or sharing.

Most implementations that provides SRFI 39 can trivially provide this facility.  The exceptions are implementations like Scheme48 which neither copy nor share parameter objects with child threads, but provide freshly initialized objects instead.

== Thanks ==

Thanks to Taylor Campbell, Alaric Snell-Pym, and Marc Feeley, who as usual are not responsible for what I did to their ideas and/or prose.



