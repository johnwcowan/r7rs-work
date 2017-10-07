== MetaModules ==

Using a separate language is a powerful yet clean way to allow for
extensible module syntax.  The separation of the module language and
the actual Scheme code in the body of the module helps avoid phasing
issues and naming conflicts, and allows for static analysis of the
module dependencies and imports and exports.

As an example of issues with naming conflicts, ChezWeb is an extension
to a syntactic module system which basically allows for mixing
documentation in with the module syntax.  One problem with this is
that it must be "installed" globally (into the top-level REPL, or as a
pre-load step when batch compiling), and there's no means of avoiding
naming conflicts between multiple module-system extensions or whatever
the user may have defined at the REPL.  An implementation with a
single meta-module like Scheme48 alleviates this somewhat by
separating the module language from the REPL.  An implementation with
unlimited meta-modules like PLT basically solves this problem entirely
- every module first specifies what language it is with the `#lang
<module-name>` syntax, and only extensions explicitly provided by that
language are available.

Static analysis means you can find out information about the module
statically.  This is easiest of course in a completely static module
system, but it is still easy in a meta-module system because you can
expand the module form (evaluating code only inside the meta-module)
without expanding the actual body of the module.  Thus you can cheaply
determine the dependency graph of a group of modules, or all visible
imports of a module (is essential for an IDE which wants to provide
syntax-highlighting or auto-completion), or all exports of a module
(needed to determine if the module satisfies the export signature
needed for a given interface, etc.).  With a syntactic module system
you can't determine any of this information without completely
expanding the module.  This may not even be possible if not all of the
pre-requisite modules are installed, or if the FFI accesses a C
library not installed, or if there's a bug in the code, etc.

