Finalizers are functions associated with an object that are intended to be invoked
before the object is collected by the garbage collector.  There are remarkably few
guarantees:  the finalizer may or may not be called before the process ends, and
if the implementation provides multiple threads, the finalizer may be called from
any thread.

Finalizers should be used as a way of disposing of resources tied to a Scheme object
(such as a file descriptor, database connection, or object maintained by another
language runtime) when the Scheme object is no longer accessible.  This is particularly
important when the Scheme object escapes the scope in which it is created.

To register a finalizer on an object, the procedure `set-finalizer!` is invoked on
the object, and an undefined value is returned.  It is an error to invoke `set-finalizer!`
on the same object more than once: two finalizers may be registered, or the later
finalizer may replace the previous one, or the later finalizer may be ignored.

