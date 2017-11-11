*I've replaced this proposal with a slightly amended version of [ImmutableParametersCowan](ImmutableParametersCowan.md).*

# Parameters

This proposal defines *parameter objects*, the procedure `make-parameter` to create parameter objects, and the `parameterize `special form to dynamically bind parameter objects. In the dynamic environment, each parameter object is bound to a cell containing the value of the parameter, which may be any Scheme object. When a procedure is invoked, the called procedure inherits the dynamic environment from the caller. The `parameterize` special form allows the binding of a parameter object to be changed for the dynamic extent of its body.

The proposal is an alternative to [ParametersSnellPym](ParametersSnellPym.md), providing parameter objects that are *not* shared between threads.  It is a profile of SRFI 39, which does not specify how threads and parameters interact.

## Rationale

The *dynamic environment* is the structure which allows the system to find the value to be returned by the R5RS procedures `current-input-port` and `current-output-port`. The R5RS procedures `with-input-from-file` and `with-output-to-file` extend the dynamic environment to produce a new dynamic environment which is in effect for the dynamic extent of the call to the thunk passed as their last argument. These procedures are essentially special-purpose dynamic binding operations on hidden dynamic variables (one for `current-input-port` and one for `current-output-port`). The purpose of this proposal is to generalize this dynamic binding mechanism (which exists in all R5RS-compliant systems) to allow the user to introduce new dynamic variables and dynamically bind them.

General dynamic binding mechanisms exist in several implementations of Scheme under various names, including fluid variables and parameter objects. The parameter objects specified in this proposal are compatible with the semantics of some, but not all, implementations of Scheme that currently support [SRFI-39](http://srfi.schemers.org/srfi-39/srfi-39.html).

Parameters are certainly useful:

1. They can be labor-savers, passing "configuration information" down through complex call stacks without needing explicit parameter passing through functions that do nothing with the parameter other than pass it to all child procedures called, until a lower layer actually uses it.

2. They allow isolation of concerns; as per the previous point, the intermediate procedures do not need to know what configuration information is sent to which child procedures, which is useful when the child procedures are provided by external libraries, or are arbitrary closures which might invoke arbitrary external libraries.

3. They allow dynamically scoped state, for tasks like exception handling and thread-local storage.

4. They generalize a mechanism that must be present within the implementation to support `current-input-port` and `current-output-port`, making it portably available to library authors.

Parameters need to be specified as part of WG1 Scheme rather than implemented by portable libraries, because a portable library has no way of knowing if the implementation provides threads; and if it does, it has no portable way of implementing dynamically scoped state in a thread-safe manner.


## Specification

The dynamic environment is composed of two parts: the *local dynamic environment* and the *global dynamic environment*. The global dynamic environment is used to look up parameter objects that can't be found in the local dynamic environment. When parameter objects are created, their initial binding is put into the global dynamic environment by mutation. The local dynamic environment is only extended by the `parameterize` form.

Parameter objects are created with the `make-parameter` procedure. The global dynamic environment is updated to cause the newly created parameter object to refer to a value, which can be retrieved by invoking the parameter object as a procedure with no arguments.

A parameter object is a procedure which accesses the cell bound to a particular parameter object in the dynamic environment. When no argument is passed, the current contents of the cell is returned. When one argument is passed, the contents of the cell is updated with the result of applying the parameter object's conversion procedure to the argument.

The `parameterize` special form, when given parameter objects and corresponding values, binds, for the dynamic extent of its body only, each parameter object to a new value. The `parameterize` special form behaves analogously to `let` when binding more than one parameter object; that is, the order of evaluation is unspecified and the new bindings are only visible in the dynamic extent of the `parameterize` special form.  However, while `let` requires the user to specify identifiers to be bound, the parameter objects bound by `parameterize` can be specified using arbitrary Scheme expressions.

This proposal also specifies that `current-input-port` and `current-output-port` are implemented as parameter objects created with `make-parameter`.

## Procedures and syntax

`(make-parameter `*init*` `[*converter*]`)`                     *procedure*

Returns a new parameter object which is bound in the global dynamic environment to a cell containing the value returned by the call `(`*converter*` `*init*`)`. If *converter* is not specified the identity function is used instead.  The conversion procedure can be used to coerce *init* to a suitable type or to signal an error if its type is inappropriate.

Invoking the returned parameter object with no arguments returns the value it currently refers to.  Invoking the returned parameter object with a single argument *x* causes it to refer to *x*.

```
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
        (radix 2)
        (radix)           ==>  2
        (write-shared 0)  gives an error

        (define prompt
          (make-parameter
            123
            (lambda (x)
              (if (string? x)
                  x
                  (with-output-to-string (lambda () (write x)))))))

        (prompt)       ==>  "123"
        (prompt ">")
        (prompt)       ==>  ">"

```

`(parameterize ((`*expr1*` `*expr2*`) ...) *body*)`             *syntax*

The expressions expr1 and expr2 are evaluated in an unspecified order. The value of the expr1 expressions must be parameter objects. For each expr1 expression and in an unspecified order, the local dynamic environment is extended with a binding of the parameter object expr1 to a new cell whose content is the result of the call (converter val), where val is the value of expr2 and converter is the conversion procedure of the parameter object. The resulting dynamic environment is then used for the evaluation of <body> (which refers to the R5RS grammar nonterminal of that name). The result(s) of the parameterize form are the result(s) of the <body>.

The expressions *expr1* and *expr2* are evaluated in an unspecified order. The values of the *expr1* expressions must be parameter objects. For each *expr1* expression, the local dynamic environment is extended in an unspecified order with a binding of the parameter object expr1 to a new cell whose contents is the result of the call `(`*converter*` `*val*`)`, where *val* is the value of *expr2* and *converter* is the conversion procedure of the parameter object. The resulting dynamic environment is then used for the evaluation of *body*, which refers to the R5RS grammar nonterminal of that name. The result(s) of the `parameterize` form are the result(s) of *body*.

```
        (radix)                                              ==>  2
        (parameterize ((radix 16)) (radix))                  ==>  16
        (radix)                                              ==>  2

        (define (f n) (number->string n (radix)))

        (f 10)                                               ==>  "1010"
        (parameterize ((radix 8)) (f 10))                    ==>  "12"
        (parameterize ((radix 8) (prompt (f 10))) (prompt))  ==>  "1010"
```


## Interaction with threads

SRFI 39 does not specify how parameter objects interact with threads.  [ParametersSnellPym](ParametersSnellPym.md) provides mutable parameters shared between threads, which threads may atomically mutate to share information.  I take the view that using parameter objects in this way is an improper and unnecessary conflation of two distinct facilities, dynamic variables and shared data.  Instead, some mechanism for exchanging data between threads, such as mailboxes, should be provided as a part of standardizing threads, probably in WG2.

Therefore, this proposal specifies that when a parent thread creates a new child thread, the cells inside the parameter objects in the child thread are new objects whose *initial* contents are the same  (in the sense of `eqv?`) as their *current* (not initial) values in the parent thread.  In this way, parameters in the child thread are initialized sensibly but operate independently of the parent thread.


## Thanks

Thanks to Taylor Campbell, Alaric Snell-Pym, and Marc Feeley, who as usual are not responsible for what I did to their ideas and/or prose.


