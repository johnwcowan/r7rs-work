**This page has nothing to do with Scheme.**

# CL-R

CL-R is a reduced form of Common Lisp intended to be quickly and efficiently compiled
to efficient programs by restricting the language.  The name can be interpreted as
"Common Lisp Runtime", "Common Lisp Restricted" or "Common Lisp without Reflection".

Everything in CL is in CL-R except as noted below.

The CL-R compiler is (intended to be written) in CL, and everything that happens
at compile time can exploit the full resources of CL.  In particular, a function
that is called only from macros (as opposed to a function that is referred to
in the output of a macro function) can be in full CL.  Furthermore, random
top-level forms can be in full CL.

A general CL-R limitation is that a function designator cannot be a symbol.

If a chapter is not mentioned here, all its functions are included in CL-R.

## Principles for removing functions and variables

  *  No loading, evaluation, compilation, or macroexpansion.
  *  No reflection on symbol values.
  *  No computed type specifiers, class names, or slot names.
  *  Packages are immutable except for interning new symbols.
  *  Classes are immutable.
  *  The class of an instance is immutable.
 
## Variables

All CL variables are part of CL-R except:

\*MACROEXPAND-HOOK\*, as no macro expansion happens at runtime.

The REPL variables -, +, ++, +++, \*, \*\*, \*\*\*, /, //, ///, as there is no REPL
at run time.

\*READ-EVAL\* is provided, but is always NIL, as there is no evaluation at run time.

## 3 Evaluation and Compilation

None of the functions of Chapter 3, Evaluation and Compilation, are in CL-R.

However, calls to COERCE and TYPEP are interpreted by the CL-R compiler as macro invocations;
the type specifier argument must be quoted.

## 5 Data and Control

All of the functions of Chapter 5, Data and Control, are in CL-R, except:

FDEFINITION, FBOUNDP, FMAKUNBOUND, which involve reflection on symbol values.

GET-SETF-EXPANSION, which performs reflection on the name of a SETF-expander.

## 7 Objects

None of the functions of Chapter 7, Objects, are in CL-R, except:

MAKE-INSTANCE.

CLASS-OF and CLASS-NAME, which allow classes to be referred to by name.

The local functions CALL-NEXT-METHOD and NEXT-METHOD-P.

Calls to SLOT-BOUNDP, MAKUNBOUND, and SLOT-VALUE are interpreted by the CL-R compiler
as macro invocations; the slot name argument must be quoted.

The compiler accepts methods for the generic functions INITIALIZE-INSTANCE,
SHARED-INITIALIZE, SLOT-MISSING, SLOT-UNBOUND, NO-APPLICABLE-METHOD,
and NO-NEXT-METHOD but does not allow them to be explicitly called.

## 10 Symbols

All of the functions of Chapter 10, Symbols, are in CL-R, except:

SET, which reflects on a symbol value.

SYMBOL-FUNCTION, SYMBOL-VALUE, BOUNDP, MAKUNBOUND, which involve
reflection on symbol values.

## 11 Packages

None of the functions of Chapter 11, Packages, are in CL-R, except:

FIND-PACKAGE and PACKAGE-NAME, which allow packages to be referred to by name.

INTERN, which allows symbols to be added to packages and is used by READ.

MAKE-PACKAGE is TBD.

## 12 Numbers

All of the functions of Chapter 12, Numbers, are in CL-R, except:

UPGRADED-COMPLEX-PART-TYPE, which is only useful at compile time given that
type names are constants at run time.

## 14 Arrays

All of the functions of Chapter 14, Arrays, are in CL-R, except:

UPGRADED-ARRAY-ELEMENT-TYPE, which is only useful at compile time given that
type names are constants at run time.

## 24 System Construction

None of the functions of Chapter 24, System Construction, are in CL-R.

## 25 Environment

None of the functions of Chapter 25, Environment, are in CL-R, except:

ENCODE-UNIVERSAL-TIME and DECODE-UNIVERSAL-TIME.

DESCRIBE-OBJECT.

GET-INTERNAL-RUN-TIME and GET-INTERNAL-REAL-TIME.


