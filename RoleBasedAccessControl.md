**This page is a pre-SRFI, but has nothing to do with R7RS.**

## Abstract

The idea of role-based access control (RBAC) is that
there are *actions* that can or cannot be performed on *resources* by *principals*, 
depending on whether or not a principal belongs to a *role*.
This library allows clients to check what can be done,
but it is up to its clients to actually permit or forbid actions based on this information.
Such checks are done relative to a *rulebase*;
all rulebases are entirely independent from each other.

Actions, principals, and roles are all named by symbols.
Actions are typically verbs like `read`, `write`, `modify`, and `delete`;
principals are typically nouns, especially proper names;
roles are typically plural nouns.
However, any of these can be anything at all.

When setting up a rulebase for use by RBAC,
we specify the actions, principals, and roles to be used in that rulebase.
We can also specify *rules* of various types.
A rule can say that a principal belongs to a role.  
Another kind of rule specifies that a role is a sub-role of another role;
this means that all principals belonging to the first role
automatically also belong to the second role.

A resource is named by a list of symbols representing a path in an abstract hierarchy.
A rule specifying that a role is *allowed* to perform an action on a resource
implicitly means that the role is also allowed to perform
the same action on any sub-resources.
For example, if role `updaters` is allowed to perform action `write` on the resource
`(localhost pub)`, then it is also allowed to perform that action on the sub-resource
`(localhost pub canada)`.

However, if a rule specifies that a role is *blocked*
from taking a given action on a given resource,
then principals belonging to that role cannot perform that action
on the specified resource or its sub-resources.
In particular, if `updaters` is blocked from performing action
`write` on `(localhost pub private`), then that rule prevails
over a rule that allows `updaters` to perform the action `write`
on `(localhost pub)`.

By default, all roles are blocked from performing any action
on the root resource named `()`.
Resources are not declared in a rulebase,
but are deduced from their presence in allow and block rules.

Finally, principals can be members of *groups*,
and groups can belong to roles in the same way that principals can.
We determine if a principal belongs to a group based on procedures
that specify which principals are in the group.
A group also has a *lead member* declared for it;
at query time, if a group's procedures say
that the lead member is not part of the group,
an error is signaled if an attempt is made
to allow or block a group member from taking an action.
Note that principals and groups belong to the same namespace:
a given symbol cannot represent both a principal and a group.

## Rulebase constructor

`(make-rbac)`

Return a rulebase with no actions, principals, roles, groups, or rules.

## Adding and removing rulebase objects

`(rbac-add-action `*rulebase action*`)`

Add the symbol *action* to *rulebase*.

`(rbac-remove-action `*rulebase action*`)`

Remove the symbol *action* from *rulebase*.

`(rbac-add-principal `*rulebase principal*`)`

Add the symbol *principal* to *rulebase*.

`(rbac-remove-principal `*rulebase principal*`)`

Remove the symbol *principal* from *rulebase*.

`(rbac-add-role `*rulebase role*`)`

Add the symbol *role* to *rulebase*.

`(rbac-remove-role `*rulebase role*`)`

`(rbac-add-group `*rulebase group all-members member? lead-member*`)`

Add the symbol *group* to *rulebase*,
specifying *proc* as the procedure for retrieving and checking group membership
and *lead-member* for the lead member of the group.
If *all-members* is called with no arguments, it returns a list of all
the principals in the group.  If *member?* is called with one argument,
a principal, it returns `#t` if the principal is a member of the
group and `#f` otherwise.

`(rbac-remove-group `*rulebase group*`)`

Remove the group identified by the symbol *group* from *rulebase*.

## Adding and removing rulebase relationships

Note that attempts to remove rules
that are not in the rulebase are ignored.

`(rbac-add-to-role `*rulebase principals role*`)`

Adds a rule specifying that the principals and/or groups in
the list *principals* belong to *role*.

`(rbac-remove-from-role `*rulebase principals role*`)`

Removes a rule specifying that the principals and/or groups in
the list *principals* belong to *role*.

`(rbac-add-subrole `*rulebase subrole role*`)`

Adds a rule specifying that the principals belonging to *subrole*
automatically also belong to *role*.

`(rbac-remove-subrole `*rulebase subrole role*`)`

Removes a rule specifying that the principals belonging to *subrole*
belong to *role*.

`(rbac-add-allow `*rulebase role actions resource*`)`

Adds rules allowing *role* to perform any of the actions
in the list *actions* on *resource*.

`(rbac-remove-allow `*rulebase role actions resource*`)`

Removes any rules allowing *role* to perform any of the actions
in the list *actions* on *resource* or any of its sub-resources..

`(rbac-add-block`*rulebase role actions resource*`)`

Adds rules blocking *role* from performing any of the actions
in the list *actions* on *resource* or any of its sub-resources.

`(rbac-remove-block `*rulebase role actions resource*`)`

Removes any rules blocking *role* from performing any of the actions
in the list *actions* on *resource* or any of its sub-resources.

## Compiled rulebases

`(rbac-compile `*rulebase*`)`

Checks the *rulebase* for consistency:
that is, actions, principals, roles, and groups
referred to in rules have to exist in the rulebase.
Returns a compiled form of the rulebase which can be efficiently checked;
in particular, all groups and roles are expanded
to the corresponding list of principals at this time.
The objects and rules defined in compiled rulebases cannot be changed.

`(rbac-allow? `*compiled-rulebase principal action resource*`)`

Returns `#t` if *principal* (or some group it is a member of)
belongs to a role which is allowed to perform *action* on *resource*
or any of its sub-resources, as specified by *compiled-rulebase*, and `#f` otherwise.
Checks that groups include lead members are done at this time.

