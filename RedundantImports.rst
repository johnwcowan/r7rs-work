Importing two or more different libraries that export the same identifier with the same binding is okay in both R7RS and R6RS libraries and programs.  Importing the same identifier with different bindings is an error in R7RS and signals an error in R6RS systems, but should be permitted by an R7RS REPL (R7RS section 5.2).  This is tested by the following program:
{{{
(define-library (local xy)
  (export list car cdr x y)
  (import (scheme base))
  (begin
   (define x 101)
   (define y 202)))
(define-library (local x)
  (export car x)
  (import (scheme base) (local xy)))
(define-library (local y)
  (export cdr y)
  (import (scheme base) (local xy)))
(import (scheme base) (scheme write) (local x) (local y))
(begin
 (write (list x (car (cdr (list x y)))))
 (newline))
}}}
For R6RS, change `define-library` to `library`, `(scheme base)` to `(rnrs base)`, and `(scheme write)` to `(rnrs io simple)`.  To test different bindings, add new definitions of `x` and `y` to `(local x)` and `(local y)` respectively.

||'''system'''||'''version'''||'''mode'''||'''same binding'''||'''different binding'''||'''different binding in REPL'''||
||Chibi|| 0.7 || r7rs || okay || bug? || bug? ||
||Chicken|| 4.9.0.1 || r7rs || okay || warned || okay ||
||Foment|| 0.4 (debug) || r7rs || bug? || bug? || bug? ||
||Gauche|| 0.9.4 || r7rs || okay || accepted || okay ||
||Kawa|| 2.0 || r7rs || okay || rejected || okay ||
||Larceny|| 0.98 || r7rs || okay || rejected || okay ||
||Petit Larceny|| 0.98 || r7rs || okay || rejected || okay ||
||Sagittarius|| 0.6.4 || r7rs || okay || rejected || rejected ||
||Larceny|| 0.98 || r6rs || okay || rejected || n/a ||
||Petit Larceny|| 0.98 || r6rs || okay || rejected || n/a ||
||Petite Chez|| 8.4 || r6rs || okay || rejected || n/a ||
||Racket|| 6.1.1 || r6rs || okay || rejected ? || n/a ||
||Sagittarius|| 0.6.4 || r6rs || okay || rejected || n/a ||
||Vicare|| 0.3d7 || r6rs || okay || rejected || n/a ||

The R6RS effectively forbids REPLs.

In some cases, indicated by "bug?", the observed behavior appears to indicate a bug that's unrelated to the issue tested here.

See also the list of real and potential [[SrfiInteroperability|conflicts between SRFIs]].