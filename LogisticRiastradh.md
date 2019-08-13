Interface: definitely logsumexp, logistic, logit;  
probably log1mexp, log1pexp, logit-exp, log-logistic;  
maybe logistic-1/2, logit1/2+.

Notes:

So, focus on logsumexp, logistic, and logit;  
consider log1mexp, log1pexp, logit-exp, and log-logistic;  
and it wouldn't hurt to have logistic-1/2 and logit1/2+  
but it's a very very small difference from tanh/artanh.

You should also require bounds on the forward relative error so that it's actually reliably useful  
to have them, and in the documentation you should have a plot of the function  
and of its condition number, and for the complex functions, a plot of the branch cut.

The MIT Scheme reference manual (in git master) has documentation for these too,  
including the aforementioned plots.

You should use the name error-bound for the largest relative error,
and ulp-of-one for the distance from 1 to the next larger floating-point number in magnitude.

[MIT code](http://git.savannah.gnu.org/cgit/mit-scheme.git/tree/src/runtime/arith.scm#n1974)
[MIT test code](http://git.savannah.gnu.org/cgit/mit-scheme.git/tree/tests/runtime/test-arith.scm#n193)

[Wikipedia](https://en.wikipedia.org/wiki/Logistic_function)