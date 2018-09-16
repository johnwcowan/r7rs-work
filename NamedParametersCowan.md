## Named Parameters

In this design, keywords are disjoint from symbols, but there is no special syntax for them: for each keyword, a variable is bound to the value of that keyword.  Keywords must be compared with `keyword=?` rather than with `eq?`.

The problem with this design is that keywords in a library must be exported explicitly, and if the same keyword is used in more than one library then it must be imported from exactly one (or renamed) due to R6RS/R7RS import rules, even though `keyword=?` does not care about the name of the identifier, only the name of
the symbol inside the keyword object.  This is a great nuisance.

## Syntax

`(keywords-lambda `*arg* ... `(`*key*` `*value*`) `...`) *body* ...`)`  [syntax]

Returns a procedure whose body is *body* that accepts non-keyword arguments *args* and keyword arguments whose names are *keys*.  *Values* specify the default value for the corresponding *keys*.

Note that *keys* need not have any particular syntax such as beginning with or ending with a colon, but they may do so by convention.  As identifiers, keywords must be explicitly exported from modules.

`(keywords-define (`*name*` `*arg* ... `(`*key*` `*value*`) `...`) *body* ...`)`  [syntax]

Defines a procedure named *name* with body `(lambda/keywords `*arg* ... `(`*key*` `*value*`) `...`) `*body* ...`)`

## Procedures

`(make-keyword `*symbol*`)`

Constructs and returns a keyword object whose name is *symbol*.

`(keyword? `*obj*`)`

Returns `#t` if *obj* is a keyword object, and `#f` otherwise.

`(keyword=? `*kw1*` `*kw2*`)`

Returns `#t` if *kw1* and *kw2* are keywords with the same name, and `#f` if they have different names.  It is an error if *kw1* and *kw2* are not keyword objects.

## Rationale and implementation

See [KeywordArgumentsArcfide](KeywordArgumentsArcfide.md).  The syntax keywords `keywords-define` and `keywords-lambda` here correspond to `define/optional` and `lambda/opt` there.
