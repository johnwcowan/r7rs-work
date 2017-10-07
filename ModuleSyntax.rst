== ModuleSyntax ==

Whatever implementation strategy is used, a syntax for the standard
module form must be chosen.  Modulo keyword names, options include:

  * Unwrapped:

    (module <declarations ...)
    <body> ...

    Seen in most other programming languages, the `module` form is
    just a single declaration, and the rest of the file contains the
    actual code.  This has the distinct disadvantage that it can't be
    implemented easily in many strategies, or on top of existing
    module systems.  Guile and Gauche use this syntax, although Gauche
    allows including the body inside the module form.

  * Simple wrapper:

    (module <declarations> ... <body> ...)

    Used in the R6RS proposal and many Scheme implementations, this is
    simple but opens questions of whether declarations may be expanded
    from macros, and if so makes any static analysis of the module
    impossible without expanding the body.  Sometimes disliked because
    it requires indenting the body of the module.

  * Delimited wrapper:

    (module (<declarations> ...) <body> ...)

    This avoids the issues above, simply by delimiting the
    declarations with a pair of parentheses so they are all known in
    advance.  If they are static (can't be expanded from macros), then
    a simple rule of allowing and ignoring any unknown declaration
    keyword allows for easy forward-compatibility and
    implementation-specific extensions.

  * Declaration-only:

    (module <declarations> ...)

    The module form only allows declarations - any code needs to be
    specified with declarations such as (include <file>) or (body
    <code> ...).  This is the syntax used in Scheme48 and
    Chibi-Scheme.  It is equivalent in expressiveness to the delimited
    wrapper approach, trading an additional level of indentation for
    no extra parens around the declarations.

  * Declaration-only with embedded body shortcut:

    (module <declarations> ... ---- <body> ...)

    where `----` is some arbitrary symbol chosen to act as a delimiter
    between the declarations and the body of the library.  Otherwise
    the same as above, the shortcut syntax allows the body to have
    only one level of indentation instead of two.

In addition, with all of these syntaxes, some declarations
that are frequently used may get fixed positions.  The name
of the module is almost universally the first argument after
the `module` keyword.  In some systems, such as Chez and
Chicken, the exports list is given as the second positional
argument.

One frequent debate with respect to syntax is whether to
keep the module declaration and source in one file or to
split them across separate files.  However, whichever
module system is used this largely boils down to user
preference.  The syntaxes which include an implicit body
suggest a single file, but so long as an (include <file>)
or similar form is provided, any such system can move the
body to a separate file.  From the other side, syntaxes
with no <body> suggest an `include` is required, but all
such systems provide a way to inline the body in the
module declaration (`begin` in Scheme48 and `body` in Chibi).
So this preference shouldn't affect your choice of syntax.
