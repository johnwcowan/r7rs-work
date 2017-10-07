== Introduction ==

As the charter states, "the purpose of this work is to facilitate
sharing of Scheme code."  To this effect, a module system is
explicitly given as a requirement in the charter.  We need to consider
all the different types of module systems supported by R5RS and R6RS
implementations and come up with a proposal that can gain widespread
use.

Module systems serve the purpose of encapsulating code and managing
namespaces.  Although `lambda` lets us manage local identifiers,
it doesn't allow us to encapsulate macros, and it wants for a
friendlier interface for module-like uses.

There are a number of fundamentally different ways in which modules
can be provided.  The major types of module systems include:

  * FirstClassEnvironments

    First-class environments that can be passed as a second argument
    to `eval` (or the equivalent) can be used as a form of
    encapsulation, where a module is the set of bindings defined in
    the environment.  If macros are not first-class, the environments
    would need some interface for passing back and forth macro
    bindings between environments.  The disadvantages of this approach
    are that many implementations do not support such first-class
    environments, separate compilation and phasing becomes more
    complicated, and static analysis is almost impossible.

  * SyntacticModules

    Another natural approach to creating modules is to build them on
    top of, and allow them to be composed with, macros, and allowing
    import forms to be expanded from macros.  It is relatively easy to
    implement this for non-syntactic bindings only, and one sample
    portable implementation can be found in
    [[http://mumble.net/~campbell/scheme/lexmod.tar.gz|lexmod]].
    Allowing importing and exporting syntax requires non-portable
    extensions.  Chez Scheme uses this approach.

  * MetaModules

    An alternative approach to extending a module system is to provide
    one or more extensible languages in which to compose the modules.
    Generally, these languages are described within the module itself.
    PLT takes this approach with multiple languages, which can also
    modify the read syntax and other features such as the semantics of
    function application.  Scheme48 takes this approach with a single
    meta module called the `config` module, which is globally extended
    when you want to add new module syntax.  In either of these cases,
    you obtain the flexibility to extend the module syntax with new
    features, but at the same time provide a separation between the
    module meta-info and the actual body of the module itself,
    allowing a certain amount of static analysis without actually
    expanding the module.

  * StaticModules

    R6RS provides a simple static module syntax which provides just
    the basics needed for importing and exporting (optionally renamed)
    identifiers from other modules.  Neither the module itself nor the
    import or export forms may be expanded from macros, so the only
    way to extend the syntax is with future standardization efforts.
    The primary reason a static module system is desirable is because
    by specifying only the syntax it can be implemented by any of the
    above module systems.  Implementations can then extend the syntax
    as needed when not concerned with portability, but a common ground
    exists when no advanced features are needed.

== Additional Issues ==

  * ModuleSystemSurvey
  * ModuleSyntax
  * ModuleAdvancedFeatures

== Existing Proposals ==

  * [[http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_chap_7|R6RS]] (the default choice)
  * ModulesAndPackagesArcfide
  * ModulesShinn
  * ModulesGanz
