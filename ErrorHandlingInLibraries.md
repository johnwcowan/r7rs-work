These are ideas for how to specify errors in WG2 libraries.  More than one can be used in a given package.

  1. Say "it is an error".  That means users should avoid it and implementations can do what they want, including signalling an error, usefully extending the behavior, or making demons fly out of your nose.
  2. Say "an error is signalled".  That means that the implementation behaves as if `raise` is called with an implementation-defined object.
  3. Say that `raise` is called on a newly allocated condition object of a specified type.
    a. Specify a predicate for testing the type of the condition.
    b. Specify a predicate and accessors for getting specified information out of the condition.
    c. Specify a predicate, accessors, and a constructor so that the user can create their own conditions of the type.