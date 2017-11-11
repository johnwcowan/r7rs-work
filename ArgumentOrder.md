This is a test of the order in which Scheme REPLs evaluate the arguments of a procedure call.  It doesn't necessarily apply when arguments have different levels of complexity, nor does it apply to compiled code.  The test is `((lambda x x) (display "l") (display "t") (display "r"))`.  In some cases, side-effecting procedures other than `display` were used.

Displays `ltr`: Racket, Gauche, Gambit, Chicken, Bigloo, Scheme48/scsh, Kawa, SCM, Vicare, Ypsilon, Mosh, IronScheme, NexJ, STklos, KSi, SigScheme, Scheme 9, RScheme, S7, Rep, Schemik, Elk, UMB, Llava, Sizzle, FemtoLisp, Dfsch, Inlab, Foment, Sagittarius

Displays `rtl`: MIT, BDC, XLisp, SXM, Chibi

Unpredictable order: Chez, Larceny

Undeterminable order (no side effects): Owl Lisp

The following C code was also tested:

```
# include <stdio.h>

int yotz(int a, int b, int c) {
  return a + b + c;
}

int main() {
  yotz(printf("l"), printf("t"), printf("r"));
}
```

Displays `ltr`: Clang

Displays `rtl`: GCC
