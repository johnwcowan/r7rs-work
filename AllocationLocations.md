Where can we allocate objects in SBCL?

  * In managed space (the stack or in the garbage-collected heap). No restrictions.
    
  * In uncollected space (can't contain any Scheme objects).  Object must be freed manually.
  
  * In homogeneous-vector space (can't contain any Scheme objects).  Object will be collected when inaccessible.
  
  * In weak-vector space.  Any Scheme objects are only weakly stored and may be garbage collected without notice to the object.  Object will be collected when inaccssible.
