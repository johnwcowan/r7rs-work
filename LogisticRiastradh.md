Interface: logsumexp, logistic, logit,
log1mexp, log1pexp, logit-exp, log-logistic. 

Notes:

`logistic-1/2` and `logit1/2+` are omitted
because they are only slightly different
from `tanh` and `atanh`.

You should also require bounds on the forward relative error so that it's actually reliably useful  
to have them, and in the documentation you should have a plot of the function  
and of its condition number, and for the complex functions, a plot of the branch cut.

The MIT Scheme reference manual (in git master) has documentation for these too,  
including the aforementioned plots.

You should use the name error-bound for the largest relative error,
and ulp-of-one for the distance from 1 to the next larger floating-point number in magnitude.

[MIT code](http://git.savannah.gnu.org/cgit/mit-scheme.git/tree/src/runtime/arith.scm#n1974)  
[MIT test code](http://git.savannah.gnu.org/cgit/mit-scheme.git/tree/tests/runtime/test-arith.scm#n193)  
[MIT texi docs](https://git.savannah.gnu.org/cgit/mit-scheme.git/plain/doc/ref-manual/numbers.texi)

[Wikipedia](https://en.wikipedia.org/wiki/Logistic_function)

TexInfo documentation:
```
@deffn procedure logistic x
@deffnx procedure logit x
Logistic and logit functions.
Equivalent to:
@iftex
@tex
\eqimage{fig/logistic}
  {$$\mathop{\rm logistic} x = {e^x \over 1 + e^x} = {1 \over 1 + e^{-x}},$$}
\eqimage{fig/logit}
  {$$\mathop{\rm logit} p = \log {p \over 1 - p} = \log {1 \over {1\over p} - 1}.$$}
@end tex
@end iftex
@ifnottex

@example
@group
logistic x = exp(x)/[1 + exp(x)] = 1/[1 + exp(-x)],
logit p = log p/(1 - p).
@end group
@end example

@end ifnottex

These functions are inverses of one another.
The logit function maps a probablity @math{p} in @math{[0, 1]} into
log-odds @math{x} in the extended real line, and the logistic function
maps back from log-odds to probabilities.

@itemize @bullet
@item
The logistic function is defined on the entire real line, but is
ill-conditioned for large negative @var{x}, with condition number
@iftex
@tex
\eqimage{fig/cn-logistic}
  {$$x f'(x)/f(x) = {x e^{-x} \over 1 + e^{-x}}.$$}
@end tex
@end iftex
@ifnottex

@example
x f'(x)/f(x) = x exp(-x)/[1 + exp(-x)].
@end example

@image{fig/cn-logistic}

@end ifnottex
The identity
@iftex
@tex
$$\mathop{\rm logistic}(-x) = 1 - \mathop{\rm logistic}(x)$$
@end tex
@end iftex
@ifnottex

@example
logistic(-x) = 1 - logistic(x)
@end example

@end ifnottex
may help to rearrange a computation, along with the logistic-1/2
function which ranges from @math{-1/2} to @math{+1/2} and centered at
zero rather than from @math{0} to @math{1} and centered at @math{1/2}.

This implementation gives forward relative error bounded by at most
seven times the forward relative error bound of the system math
library's exp, which is usually below 1ulp.

@item
The logit function is defined on the closed unit interval @math{[0,
1]}, but is ill-conditioned near @math{1/2} and @math{1}, with
condition number
@iftex
@tex
\eqimage{fig/cn-logit}
  {$$x f'(x)/f(x) = {1/(1 - p) \over \log (p/(1 - p))}.$$}
@end tex
@end iftex
@ifnottex

@example
x f'(x)/f(x) = 1/[(1 - p) log(p/(1 - p))].
@end example

@image{fig/cn-logit}

@end ifnottex
The identity
@iftex
@tex
$$\mathop{\rm logit}(1 - p) = -\mathop{\rm logit}(p)$$
@end tex
@end iftex
@ifnottex

@example
logit(1 - p) = -logit(p)
@end example

@end ifnottex
may help to rearrange a computation, along with the logit1/2+ function
which is defined on @math{-1/2} to @math{+1/2} and centered at zero
rather than on @math{0} to @math{1} and centered at @math{1/2}.

This implementation gives forward relative error bounded by at most
ten times the forward relative error bound of the system math
library's log, which is usually below 1ulp.
@end itemize
@end deffn


@deffn procedure logsumexp list
@var{List} must be a list of real numbers
@iftex
@tex
$x_1, x_2, \ldots, x_n$.
@end tex
@end iftex
@ifnottex
@var{x1}, @var{x2}, @dots{}, @var{xn}.
@end ifnottex
Returns an approximation to:
@iftex
@tex
$$\log (e^{x_1} + e^{x_2} + \cdots + e^{x_n}).$$
@end tex
@end iftex
@ifnottex

@example
log(exp(@var{x1}) + exp(@var{x2}) + @dots{} + exp(@var{xn})).
@end example

@end ifnottex
The approximation avoids intermediate overflow and underflow.
To minimize error, the caller should arrange for the numbers to be
sorted from least to greatest.

Edge cases:

@itemize @bullet
@item
If @var{list} is empty, the result is @code{-inf}, as if the
intermediate sum were zero.

@item
If @var{list} contains only finite numbers and @code{-inf}, the
@code{-inf} elements are ignored, since the exponential of @code{-inf}
is zero.

@item
If @var{list} contains only finite numbers and @code{+inf}, the result
is @code{+inf} as if the sum had overflowed.
(Otherwise, overflow is not possible.)

@item
If @var{list} contains both @code{-inf} and @code{+inf}, or if
@var{list} contains any NaNs, the result is a NaN.
@end itemize

@code{Logsumexp} never raises any of the standard @acronym{IEEE 754-2008}
floating-point exceptions other than invalid-operation.
@end deffn


@deffn procedure log1mexp x
@deffnx procedure log1pexp x
Equivalent to:
@iftex
@tex
\eqimage{fig/log1mexp}{$$\mathop{\rm log1mexp} x = \log (1 - e^x),$$}
\eqimage{fig/log1pexp}{$$\mathop{\rm log1pexp} x = \log (1 + e^x).$$}
@end tex
@end iftex
@ifnottex

@example
@group
log1mexp x = log (1 - e^x),
log1pexp x = log (1 + e^x).
@end group
@end example

@image{fig/log1mexp}
@image{fig/log1pexp}

@end ifnottex
Like log1p and expm1, these avoid numerical pathologies with the
intermediate quantities @math{1 - e^x} and @math{1 + e^x} and inputs
to log near @math{1}.

@itemize @bullet
@item
log1mexp computes the complement of a probability @math{p} in
log-space @math{\log p}, and as such is a self-inverse.
It is finite when @math{x < 0}; negative infinity when @math{x =
0}; and invalid otherwise.

@item
log1pexp is related to the logistic function @math{1/(1 + e^{-x})} ---
specifically, it differs from the logarithm of the logistic function
only by the sign of the input and the output.
@end itemize

This implementation gives forward relative error bounded by ten times
the forward relative error bound of the system math library's log and
exp, which is usually below 1ulp.

Beware that although the forward relative error of the MIT/GNU Scheme
@emph{implementations} of these functions is bounded, these
@emph{functions} are ill-conditioned for large negative inputs:
@iftex
@tex
\nobreak\par\nobreak%
$$x f'(x)/f(x) = {\pm x e^x \over (1 \pm e^x) \log(1 \pm e^x)}
  \approx x, \quad\hbox{for $x \ll 0$.}$$
\nobreak\par\nobreak\leavevmode%
\begingroup%
  % Load up the two figures into \box0 and \box1.
  \setbox0=\hbox{\image{fig/cn-log1pexp}}%
  \setbox1=\hbox{\image{fig/cn-log1mexp}}%
  % Compute the width we have to work with here in \dimen0.
  \dimen0=\hsize%
    \advance\dimen0 by -\leftskip%
    \advance\dimen0 by -\hangindent%
    \advance\dimen0 by -\rightskip%
  % Create an hbox of that width for the two figures with horizontal
  % space before, between, and after.
  \hbox to \dimen0{%
    \hfill%
    \vbox{\dimen0=\wd0 \box0    % Save width and dispense with box.
      \hbox to\dimen0{\hfill Condition number of log1pexp\hfill}}%
    \hfill%
    \vbox{\dimen1=\wd1 \box1    % Save width and dispense with box.
      \hbox to\dimen1{\hfill Condition number of log1mexp\hfill}}%
    \hfill%
  }%
\endgroup
@end tex
@end iftex
@ifnottex

@example
x f'(x)/f(x) = (+/- x exp(x))/((1 +/- e^x) log(1 +/- e^x)),
  --> x,  for x << 0.
@end example

@image{fig/cn-log1mexp}
@image{fig/cn-log1pexp}

@end ifnottex
@end deffn


deffn procedure log-logistic x
@deffnx procedure logit-exp x
Equivalent to:
@iftex
@tex
\eqimage{fig/loglogistic}
  {$$\eqalign{\mathop{\hbox{\rm log-logistic}} x &= \log \mathop{\rm logistic}(x) \cr&= \log [1/(1 + e^{-x})],}$$}
\eqimage{fig/logitexp}
  {$$\eqalign{\mathop{\hbox{\rm logit-exp}} x &= \mathop{\rm logit}(e^x) \cr&= \log [e^x/(1 - e^x)].}$$}
@end tex
@end iftex
@ifnottex

@example
@group
log-logistic x = log(logistic(x)) = log [1/(1 + exp(-x))]
logit-exp x = logit(exp(x)) = log [exp(x)/(1 - exp(x))]
@end group
@end example

@end ifnottex

Like @code{logistic} and @code{logit}, these functions are inverses of
one another.

@itemize @bullet
@item
@cindex log-probability
@cindex log-odds
The log@hy{}logistic function maps log-odds on the extended real line
to log@hy{}probability on the nonpositive half of the extended real
line, and is ill@hy{}conditioned for large positive @var{x}:
@iftex
@tex
\eqimage{fig/cn-loglogistic}
  {$$x f'(x)/f(x) = {-x e^{-x}/(1 + e^{-x}) \over \log (1 + e^{-x})}.$$}
@end tex
@end iftex
@ifnottex

@example
x f'(x)/f(x) = (-x exp(-x))/[(1 + exp(-x)) log(1 + exp(-x))]
@end example

@image{fig/cn-loglogistic}

@end ifnottex
```
