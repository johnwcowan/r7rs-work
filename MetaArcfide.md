# Meta Definitions

By Aaron W. Hsu

## Introduction

It is useful to be able to explicitly indicate a phase or level for definitions. While this probably won't matter for WG1 and Thing One, for Thing Two, where you want to be able to import bindings into a specific phase, or any other  set of activities, it makes sense to enable such meta definitions generally.

## Proposal

Standardize two forms:

`(meta` `.` *definition*`)`[[BR]]
`(at-meta` *levels* `.` *definition*`)`

The first `meta` evaluates the *definition* one phase up from where the `meta` call occurs. `at-meta` allows the evaluation to be visible at a number of phases specified by *levels*. *Levels* is a list of exact non-negative integers or the keyword `..`. The keyword `..` should follow and come before integers. `n .. m` is the same as spelling out all the integers from *n* to *m*.

## Rationale

This is a more general mechanism for controlling the visibility of definitions, and allows you to introduce definitions inside of modules into different phases as well as control the visibility of imports, provided that [ModulesAndPackagesArcfide](ModulesAndPackagesArcfide.md) is standardized.

