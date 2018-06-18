Trees, like lists, are an application of Scheme pairs.
An *atom* is any Scheme object that is not a pair.
A *tree* is a non-empty list whose elements are either trees or atoms.
Trees cannot be circular.
The atoms and subtrees of a tree are called its *nodes*.
The words *parent, child, ancestor, descendant, sibling* are used with the usual meanings.

Although the empty list is an atom, it's a bad idea to make use of it in trees,
as it may confuse list-oriented procedures that treat it as a list.

## Predicates

`(tree? `*obj*`)`

Returns `#t` if *obj* is a tree, and `#f` otherwise.

`(atom? `*obj*`)`

Returns `#t` if *obj* is an atom, and `#f` otherwise.

`(tree-contains? `*tree node*`)`

Returns `#t` if *node* is the same (in the sense of `eqv?`) as a node of *tree*, 
and `#f` otherwise.  This can also be used to determine if *tree* is an ancestor of *node*.

`(tree-c-commands? `*tree commanding commanded*`)`

If the subtree *commanding* c-commands the subtree *commanded* in *tree*, returns `#t`; 
otherwise returns `#f`.  
It is an error if either *ancestor* or *descendant* is not a subtree of *tree*.

A node in a tree c-commands its sibling node(s) and all of its siblings' descendants; 
however, a node without siblings c-commands everything that its parent node c-commands.

`(tree=? `*same? tree1 tree2*`)`

Returns `#t` if *tree1* and *tree2* are isomorphic 
and their atoms are the same in the sense of *same?*, and `#f` otherwise.

## Tree operations

`(tree-depth-of `*tree subtree*`)`

Returns the depth of *subtree* within *tree* as an exact integer. 
If *tree* and *subtree* are the same in the sense of `eqv?`, returns 0.

`(tree-depth `*tree*`)`

Returns the maximum depth of *tree* as an exact integer.

`(tree-parent `*tree node*`)`

Returns the parent node of *node* within *tree*.  This involves a search from *tree*.  
Returns `#f` if *node* is not a descendant of *tree*.

`(tree-path `*tree subtree*`)`

Returns a list of nodes containing *subtree* and 
all the ancestors of *subtree* ending with *tree*.  
Returns `#f` if *subtree* is not a descendant of *tree*.

`(tree-copy `*tree*`)`

Return a copy of *tree*.  Atoms are shared, but tree structure is not.

`(tree-map `*proc tree*`)`

Returns a copy of *tree*, except that each descendant atom has been passed through *proc*.  Tree structure is not shared.'

`(tree-flatten ` *tree*`)`

Returns a list of the atoms in *tree* in depth-first preorder.

## Node examination

The following functions examine all nodes in the tree in an unspecified order.
*Pred* is a predicate which has no side effects and always returns the same result 
on the same argument.

`(tree-size `*tree*`)`

Returns the number of atoms in *tree* as an exact integer.

`(tree-count `*pred tree*`)`

Returns the number of nodes of *tree* that satisfy *pred* as an exact integer.

`(tree-any? `*pred tree*`)`

Examines the nodes of *tree* to determine if any of them satisfy *pred*.

`(tree-every? `*pred tree*`)`

Returns the number of nodes of *tree* to determine if all of them satisfy *pred*.

## Tree walkers

The following procedures return finite 
[SRFI 121](http://srfi.schemers.org/srfi-121/srfi-121.html)
generators that return some of the nodes of a tree in any of a variety of orders. 
When all the nodes have been returned, the generator returns an EOF object.

`(tree-make-preorder-generator [[`*tree*`)`

Returns a generator that when invoked returns the nodes of *tree* in depth-first preorder: 
parents are generated before children, and children are generated in left-to-right order.

`(tree-make-postorder-generator `*tree*`)`

Returns a generator that when invoked returns the nodes of *tree* in depth-first postorder:
children are generated in left-to-right order and then their parent is generated.

`(tree-make-breadthfirst-generator `*tree*`)`

Returns a generator that when invoked returns the nodes of *tree* in breadth-first order: 
*tree* is generated, then all children of *tree* in left-to-right order, then all 
grandchildren of *tree* in left-to-right order, and so on.

`(tree-make-atom-generator `*tree*`)`

Returns a generator that when invoked returns pairs, 
where the car of each pair is an atomic descendant of *tree*, 
and the cdr of each pair is the depth of that atom.  
The order of generation is depth-first postorder.

`(atom-generator->tree `*generator*`)`

Returns a reconstituted tree from the values of *generator*, 
which generates pairs of the type created by a *tree-make-atom-generator* generator.  
The result is `tree=?` to *tree* with an atomic comparison of `eqv?`.

## Tree rewriting

These procedures do not mutate the tree they work on, 
but return a new tree isomorphic to the old tree and with the same elements, 
except as specified below.  The new tree may share storage with the old.

`(tree-add `*tree subtree newnode*`)`

Returns a tree where *node* has an additional child, *newnode*,  
which is placed to the right of all existing children.  
It is an error if *subtree* is not a descendant of *tree* 
or if *newnode* is a non-atomic descendant of *tree*.

`(tree-insert `*tree subtree index newnode*`)`

Returns a tree where *subtree* has an additional child, *newnode*,  
which is placed immediately to the left of the child 
whose position in *subtree* is *index* (an exact integer).  
It is an error if *subtree* is not a descendant of *tree*, 
if *newnode* is a non-atomic descendant of *tree*, 
or if *index* is greater than or equal to the number of child nodes of *subtree*.

`(tree-prune `*tree subtree*`)`

Returns a tree where *subtree* and all its descendants are not part of the new tree.  
It is an error if *subtree* is not a descendant of *tree*.

`(tree-replace `*tree subtree newnode*`)`

Returns a tree where *node* has been replaced by *newnode* in the new tree.  
It is an error if *subtree* is not a descendant of *tree* 
or if *newnode* is already a non-atomic descendant of *tree*.


## Node numbering

`(tree-number-preorder `*tree*`)`

Return a tree that is isomorphic to *tree*, 
except that every subtree has a new first child prepended to its existing children.  
This child is an exact integer, starting at 0 for the root 
and *n* - 1 for the last child in depth-first preorder, 
where *n* is the number of subtrees of *tree*.

`(tree-number-postorder `*tree*`)`

Return a tree that is isomorphic to *tree*, 
except that every subtree has a new first child prepended to its existing children.  
This child is an exact integer, starting at 0 for the root 
and *n* - 1 for the last child in postorder, 
where *n* is the number of subtrees of *tree*.

`(tree-number-depth-firstr `*tree*`)`

Return a tree that is isomorphic to *tree*, 
except that every subtree has a new first child 
prepended to its existing children.  
This child is an exact integer, starting at 0 for the root 
and *n* - 1 for the last child in depth-first order, 
where *n* is the number of subtrees of *tree*.

## Output

`(tree-display-atoms `*tree* []]( *port*)`)`

Walks through the atoms of *tree* in breadth-first order 
and displays them (as if using `display`) on *port*, 
which defaults to the value of `(current-output-port)`.
