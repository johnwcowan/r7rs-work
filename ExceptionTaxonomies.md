# Condition/Exception Taxonomies

This page lists the condition/exception system taxonomies for several Scheme implementations and SRFIs and for R6RS.

## Bigloo
```
  &exception
    &error
      &http-error
        &http-redirection-error
        &http-status-error
      &io-error
        &io-file-not-found-error
        &io-malformed-url-error
        &io-parse-error
        &io-port-error
          &io-closed-error
          &io-read-error
          &io-write-error
        &io-unknown-host-error
      &process-exception
      &type-error
    &http-redirection
    &warning
      &eval-warning
```

## Chez Scheme
```
  &condition
    &irritants
    &message
    &serious
      &violation
        &assertion
        &implementation-restriction
          &no-infinities
          &no-nans
        &lexical
        &non-continuable
        &syntax
        &undefined
      &error
        &i/o
          &i/o-port
            &i/o-decoding
            &i/o-encoding
          &i/o-read
          &i/o-write
          &i/o-invalid-position
          &i/o-filename
            &i/o-file-already-exists
            &i/o-file-does-not-exist
            &i/o-file-protection
              &i/o-file-is-read-only
    &warning
    &who
```

## Chicken

> Chicken supports SRFI-12.

```
  exn
    arity
    type
    arithmetic
    i/o
      file
      net
    bounds
    runtime
      limit
    match
    syntax
```

## Gambit

> Gambit doesn't have a hierarchy, but it does have a set of standard
> exceptions.
```
  heap-overflow-exception
  stack-overflow-exception

  no-such-file-or-directory-exception
  os-exception
  unbound-os-environment-variable-exception

  abandoned-mutex-exception
  deadlock-exception
  join-timeout-exception
  scheduler-exception
  started-thread-exception
  terminated-thread-exception
  uncaught-exception

  cfun-conversion-exception
  multiple-c-return-exception
  sfun-conversion-exception

  datum-parsing-exception

  expression-parsing-exception
  unbound-global-exception

  divide-by-zero-exception
  improper-length-list-exception
  range-exception
  type-exception

  keyword-expected-exception
  nonprocedure-operator-exception
  number-of-arguments-limit-exception
  unknown-keyword-argument-exception
  wrong-number-of-arguments-exception

  error-exception
```

## Gauche
```
  <condition>
    <compound-condition>
    <message-condition>
      <error>
        <io-error>
          <port-error>
            <io-closed-error>
            <io-read-error>
            <io-unit-error>
            <io-write-error>
        <read-error>
        <system-error>
        <unhandled-signal-error>
    <serious-condition>
      <serious-compound-condition>
```

## Guile

> Guile implements SRFI 18, SRFI 34, and R6RS exceptions.

## Ikarus

> Ikarus implements R6RS exceptions.

## Kawa

> Kawa gets its taxonomy of exceptions from Java.

## Larceny

> Larceny implements R6RS exceptions.

## MIT/GNU Scheme
```
  serious-condition
    error
      simple-error
      illegal-datum
        wrong-type-datum
          wrong-type-argument
          wrong-number-of-arguments
        datum-out-of-range
          bad-range-argument
        inapplicable-object
      file-error
        file-operation-error
        derived-file-error
      port-error
        derived-port-error
      variable-error
        unbound-variable
        unassigned-variable
      arithmetic-error
        divide-by-zero
        floating-point-overflow
        floating-point-underflow
      control-error
        no-such-restart
      not-loading
      primitive-procedure-error
        system-call-error
    warning
      simple-warning
    simple-condition
    breakpoint
```

## Racket
```
  exn
    exn:break
    exn:fail
      exn:fail:contract
        exn:fail:contract:arity
        exn:fail:contract:divide-by-zero
        exn:fail:contract:non-fixnum-result
        exn:fail:contract:continuation
        exn:fail:contract:variable
      exn:fail:filesystem
        exn:fail:filesystem:exists
        exn:fail:filesystem:version
      exn:fail:network
      exn:fail:out-of-memory
      exn:fail:read
        exn:fail:read:eof
        exn:fail:read:non-char
      exn:fail:syntax
      exn:fail:unsupported
      exn:fail:user
```

## R6RS
```
  &condition
    &irritants
    &message
    &serious
      &error
        &i/o
          &i/o-filename
            &i/o-file-already-exists
            &i/o-file-does-not-exist
            &i/o-file-protection
              &i/o-file-is-read-only
          &i/o-invalid-position
          &i/o-port
            &i/o-decoding
            &i/o-encoding
          &i/o-read
          &i/o-write
      &violation
         &assertion
         &non-continuable
         &implementation-restriction
         &lexical
         &syntax
         &undefined
    &warning
    &who
```

## Scheme48

> Scheme48 implements SRFI 34, SRFI 35, and SRFI 36.

## SCM

> SCM doesn't have an exception system, although it does have a way of handling the following exceptions reported by C code:
```
  alarm-interrupt
  arithmetic-error
  end-of-program
  out-of-storage
  profile-alarm-interrupt
  thrashing
  user-interrupt
  virtual-alarm-interrupt
```

## Scsh

> Scsh has no condition system.  It does include an exception system for handling errors reported by Unix system calls.

## SISC

> SISC gets its taxonomy of exceptions from Java.  It also implements SRFI 18, SRFI 34, and SRFI 35.

## SRFI 12: Exception Handling

> Any object can be used to represent a condition in this system.  There is no standard taxonomy of conditions, but there are two types of condition for which special support is included:
```
  composite condition
  property condition
```

## SRFI 18: Multithreading support
```
  abandoned-mutex-exception
  join-timeout-exception
  terminated-thread-exception
  uncaught-exception
```

## SRFI 34: Exception Handling for Programs

> No condition taxonomy is defined.  However, it does refer to SRFI 35 and SRFI 36.

## SRFI 35: Conditions
```
  &condition
    &message
    &serious
      &error
```

## SRFI 36: I/O Conditions
```
  &error
    &i/o-error
      &i/o-filename-error
        &i/o-file-already-exists-error
        &i/o-file-protection-error
          &i/o-file-is-read-only-error
        &i/o-malformed-filename-error
        &i/o-no-such-file-error
      &i/o-port-error
        &i/o-closed-error
        &i/o-read-error
        &i/o-write-error
  &read-error
```

## STklos

> STklos supports SRFI 34, SRFI 35, and SRFI 36.
