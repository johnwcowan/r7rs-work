## Conditional Expansion of Modules

This is a proposal for a `cond-expand` facility within the R7RS module language.  The syntax is the same as [SRFI 0](http://srfis.schemers.org/srfi-0/srfi-0.html), but the bodies are declarations in the module language (ModulesShinn) rather than Scheme expressions.  This makes it possible, for example, to conditionally include a file of code based on the specific implementation or some feature of the implementation, or to conditionally import a module by name provided that module exists.  Conditional expansion is done when the module is parsed, and is based only on things already known to the implementation before it even begins compiling or interpreting.

In this version, an implementation or implementation feature is named by a symbol, and a module is named by a list whose first element is not `and`, `or`, or `not`.  The meaning of a module name is "the module with this name is available for import, whether it is currently imported or not".

## Syntax

We add a new type of module declaration, `<conditional expansion form>`, with the following syntax:

```
<conditional expansion form>
    --> (cond-expand <cond-expand clause>+)
      | (cond-expand <cond-expand clause>* (else <module declaration>*))
<cond-expand clause>
    --> (<feature requirement> <module declaration>*)
<feature requirement>
    --> <feature identifier>
      | <module name>
      | (and <feature requirement>*)
      | (or <feature requirement>*)
      | (not <feature requirement>)
<feature identifier>
    --> a symbol
<module name>
    --> a list of symbols or exact integers
```

## Semantics

Each implementation maintains a list of feature identifiers which are present, as well as a list of modules which may be imported.  The semantics of a `<feature requirement>` are that if each feature identifier and module name on the implementation's list is replaced by `#t` and all other feature identifiers and module names are replaced by `#f`, and the resulting expression is evaluated as Scheme, then the corresponding `<module declarations>` are part of the module iff the result of the evaluation is `#t`.

## Standard feature identifiers

An implementation MAY provide any or all of these feature identifiers, as well as any others that it chooses, but MUST NOT provide a feature identifier if it does not provide the corresponding feature.

|Feature identifier|Feature description|
|`r7rs` |All R7RS Scheme implementations have this feature.  Name may be changed.|
|`exact-closed`|All rational operations on exact values produce exact values, with the possible exception of /.  If an implementation restriction is reached, an error is signaled.  If this feature identifier is missing, some results may be inexact.|
|`ratios`|`/` with exact arguments produces an exact result.|
|`ieee-float`|Inexact numbers are IEEE 754 floating point values.  This implies support for `+inf.0`, `-inf.0`, `+nan.0`, and `-0.0`.|
|`full-unicode`|All Unicode characters are supported.|
|`windows`|This Scheme implementation is running on Windows.|
|`posix`|This Scheme implementation is running on a Posix system.|
|`unix, darwin, linux, bsd, netbsd, openbsd, freebsd, solaris, ...`|Operating system flags (more than one may apply).|
|`i386, x86_64, ppc, sparc, sparc64, jvm, clr, llvm,` ...|CPU architecture flags.|
|`ilp32, lp64, ilp64,` ...|C memory model flags|
|`big-endian, little-endian`|Byte order flags.|
|*name*|The name of this implementation.|
|*name*`-`*version*|The name and version of this implementation.|
