= Implict Let-nil to replace implicit Begin = 

== Proposal ==

Certain forms, such as `case`, `cond`, `when`, and `unless` allow for multiple expressions to all follow one after another in certain parts of their syntax. These expressions are currently wrapped implicitly, in a `begin` form. This proposal suggests that we change this to an implicit LET-NIL wrapping.
 
== Rationale == 

While preserving backwards compatibility, this change allows for internal definitions to occur conveniently within certain expressions, where they would otherwise not be allowed. This suits some programmer's styles, and makes some things convenient without breaking anything else.