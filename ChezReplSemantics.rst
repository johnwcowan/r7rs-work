= REPL Semantics of Chez Scheme =

== Introduction ==

Chez Scheme represents an example of a high-speed incremental compiler with an interactive environemnt that follows a traditional lisp REPL behavior. It is interesting to note that Chez Scheme achieves high performance while still providing a dynamic environment, though the optimizations that Chez Scheme can make are limited by the top level semantics.

== Official Documentation ==

The Chez Scheme User's Guide Version 8 provides a chapter on the [[http://www.scheme.com/csug8/use.html#./use:h3|Interaction Environment]] of Chez Scheme. The reader is referred to this reading for the full description.

== Important Key REPL Semantics ==

  * Expressions are entered at the REPL one at a time
  * Each expression received is expanded and evaluated in the current interaction environment before the next expression that is entered is processed
  * Files that are `load`ed by reading each form in the file and processing each in turn, as if they had been entered by hand into the REPL
  * There is a single interaction environment containing bindings to macro transformers and procedures
  * The initial environment is the Chez Scheme default environment
  * All initial bindings are immutable
  * Definitions are mutable
  * Initial bindings may be redefined
  * A parameter exists that allows you to alter the interaction environment using `environment` controls
  * All identifiers are considered implicitly bound if not already defined
    * This allows recursive definitions to be entered one at a time
    * As a result, top-level identifiers will mess with `free-identifier=?`, requiring auxilary syntaxes to be explicitly bound instead of implicitly used
  * Top level identifiers are not optimized away, since they may be reassigned