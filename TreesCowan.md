# Trees
Trees, like lists, are an application of Scheme pairs.
An *atom* is any Scheme object that is not a pair.
A *tree* is a non-empty list whose elements are either trees or atoms.
Trees cannot be circular; parts of them cannot share
storage with other parts (though separate trees can share
storage with each other).
The atoms and subtrees of a tree are called its *nodes*.
The words *root, parent, child, ancestor, descendant, sibling, depth*
are used with the usual meanings.
The *subtrees* of a tree are the tree itself and all its descendant trees.
The term *local position* is used to mean the index of a child of a tree;
that is, the first child is at index 0, and so on.

Although the empty list is an atom, it's a bad idea to make use of it in trees,
as it may confuse list-oriented procedures that treat it as a list.

## Predicates

`(tree? `*obj*`)`

Returns `#t` if *obj* is a tree, and `#f` otherwise.

`(atom? `*obj*`)`

Returns `#t` if *obj* is an atom, and `#f` otherwise.

## Tree walkers

The following procedures walk the nodes of a tree in any of a variety of orders,
applying a procedure to each node.

`(tree-walk-preorder `*proc tree*`)`

Accesses the nodes of *tree* in depth-first preorder and applies
*proc* to each in turn.  That is, each node is walked and then all of its children are
recursively walked.  Returns an unspecified value.

`(tree-walk-postorder `*proc tree*`)`

Accesses the nodes of *tree* in depth-first preorder and applies
*proc* to each in turn.  That is, all the children of each node are walked and then the
node itself is walked.  Returns an unspecified value.

`(tree-walk-breadthfirst `*proc tree*`)`

Accesses the nodes of *tree* in depth-first preorder and applies
*proc* to each in turn. That is, *proc* is invoked on *tree*,
then on all children of *tree*
in left-to-right order, then on all grandchildren of *tree*
in left-to-right order, and so on.
Returns an unspecified value.

## Tree inversion

The *inversion* of a tree is an opaque object that maps each
subtree of a tree to three values:

  *  its parent, except for the root which is mapped to `#f`
  *  its depth as an exact integer (the root has depth 0)
  *  its local position as an exact integer
  
Lookups in the inversion have an amortized cost of O(1),
so hash tables with `eqv?` as the equality function are suitable.

`(invert-tree `*tree*`)`

Returns an inversion of *tree*, not necessarily freshly allocated.

`(tree-parent `*subtree inversion*`)`

Uses *inversion* to return the parent subtree of *subtree*, or
`#f` if subtree is the root.

`(tree-depth `*subtree inversion*`)`

Uses *inversion* to return the depth of *subtree*, or
`#f` if subtree is the root.

`(tree-local-position `*subtree inversion*`)`

Uses *inversion* to return the local position of *subtree*, or
`#f` if subtree is the root.

`(tree-contains? `*inversion node*`)`

Returns `#t` if *subtree* is a subtree of
he tree represented by *inversion*,
and `#f` otherwise.

`(tree-c-commands? `*inversion commanding commanded*`)`

If the subtree *commanding* c-commands the subtree *commanded* in
he tree represented by *inversion* , returns `#t`; 
otherwise returns `#f`.  
It is an error if either *ancestor* or *descendant* is not a subtree of
he tree represented by *inversion*.

A node in a tree c-commands its sibling node(s) and all of its siblings' descendants; 
however, a node without siblings c-commands everything that its parent node c-commands.

`(tree=? `*same? tree1 tree2*`)`

Returns `#t` if *tree1* and *tree2* are isomorphic 
and their atoms are the same in the sense of *same?*, and `#f` otherwise.

`(tree-path `*inversion subtree*`)`

Returns a list of nodes containing *subtree* and 
all the ancestors of *subtree* ending with *tree*.  
Returns `#f` if *subtree* is not a descendant of *tree*.

## Tree operations

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
If so, returns `#t`, otherwise returns `#f`.

`(tree-every? `*pred tree*`)`

Examines the nodes of *tree* to determine if all of them satisfy *pred*.
If so, returns `#t`, otherwise returns `#f`.

## Tree rewriting

These procedures do not mutate the tree they work on, 
but return a new tree isomorphic to the old tree and with the same elements, 
except as specified below.  The new tree may share storage with the old.

`(tree-add `*tree inversion subtree newnode*`)`

Returns a tree where *subtree* has an additional child, *newnode*,  
which is placed to the right of all existing children.  
It is an error if *subtree* is not a descendant of *tree* 
or if *newnode* is a non-atomic descendant of *tree*.

`(tree-insert `*tree inversion subtree index newnode*`)`

Returns a tree where *subtree* has an additional child, *newnode*,  
which is placed immediately to the left of the child 
whose local position in *subtree* is *index* (an exact integer).  
It is an error if *subtree* is not a descendant of *tree*, 
if *newnode* is a non-atomic descendant of *tree*, 
or if *index* is greater than or equal to the number of child nodes of *subtree*.

`(tree-prune `*tree inversion subtree*`)`

Returns a tree where *subtree* and all its descendants are not part of the new tree.  
It is an error if *subtree* is not a descendant of *tree*.

`(tree-replace `*tree inversion subtree newnode*`)`

Returns a tree where *subtree* has been replaced by *newnode* in the new tree.  
It is an error if *subtree* is not a descendant of *tree* 
or if *newnode* is already a non-atomic descendant of *tree*.

## Output

`(tree-display-atoms `*tree* []]( *port*)`)`

Walks through the atoms of *tree* in breadth-first order 
and displays them (as if using `display`) on *port*, 
which defaults to the value of `(current-output-port)`.
