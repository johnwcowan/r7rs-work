This proposal provides mutable environments for R7RS-large.
It was originally based on
[this proposal by Pavel Curtis](http://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1988/msg00139.html),
but I have changed it to follow MIT Scheme, which provides a tree of environments.

## Introduction

The small language provides four procedures that return global environment specifiers for use by `eval`.  They are:

* `environment`, which returns an environment specifier when passed on zero or more import-specs which contains the bindings made available by those import specs

* `scheme-report-environment`, which when passed the argument `5` returns a specifier for a copy of the R5RS environment

* `null-environment`, which when passed the argument `5` returns a specifier for a copy of the R5RS environment containing syntax keywords only

* `interaction-environment`, which returns a mutable environment specifier containing implementation-defined bindings, including at least those exported by the base library

MIT Scheme does not have any of these: see the Implementation section for how they might be implemented on that system.

## Procedures

The following procedures allow an application to generate, examine, and mutate environment
specifiers which can be used like those obtained from the R7RS-small procedures above.
Every environment specifier except the ones from the above procedure has a parent, such that
if the environment does not have a binding, all its ancestors are searched for that binding.

(Incorporate here from the MIT [top-level environments](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Top_002dlevel-Environments.html#Top_002dlevel-Environments)
the `extend-top-level-environment` and `extend-root-top-level-environment` procedures with the names `make-environment` and `empty-environment` respectively.)

(Incorporate here all the MIT [environment operations](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Environment-Operations.html#Environment-Operations).)

(Incorporate here from the MIT [top-level environments](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Top_002dlevel-Environments.html#Top_002dlevel-Environments)
the `unbind-variable` procedure with the name `environment-unbind!`.)

`(environment-freeze! `*environment*`)`

Freezes an environment, making it an error to add, remove, or change its bindings,
and returns an unspecified result.

The freeze status of the top-level environments is as follows:

  *  All environments created with `make-environment` or `empty-environment` are initially unfrozen
  
  *  The result of calling `interaction-environment` is always unfrozen, and attempts to freeze it are ignored.

  *  The results of calling `environment`are always frozen.

  *  The results of calling `null-environment` and `scheme-report-environment` are implementation-specified.

## Implementation

The R7RS-small procedures are not defined in MIT Scheme, but may be defined as follows:

The `interaction-environment` procedure just returns the variable `user-initial-environment`.

The `null-environment` and `scheme-report-environment` procedures return distinct environments
constructed with `make-root-top-level-environment`.  The `link-variables` procedure
is then used to copy all R5RS bindings into the environment to be returned using
`scheme-report-environment`; only the R5RS syntax keyword bindings are copied
into the environment returned by `null-environment`.

The `environment-freeze!` procedure does nothing, as MIT Scheme does not implement frozen environments.

