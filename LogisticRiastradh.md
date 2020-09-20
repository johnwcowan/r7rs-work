Notes:

`logistic-1/2` and `logit1/2+` are omitted
because they are only slightly different
from `tanh` and `atanh`.

You should also require bounds on the forward relative error so that it's actually reliably useful  
to have them, and in the documentation you should have a plot of the function  
and of its condition number, and for the complex functions, a plot of the branch cut.

[MIT code](http://git.savannah.gnu.org/cgit/mit-scheme.git/tree/src/runtime/arith.scm#n1974)  
[MIT test code](http://git.savannah.gnu.org/cgit/mit-scheme.git/tree/tests/runtime/test-arith.scm#n193)  

[Wikipedia](https://en.wikipedia.org/wiki/Logistic_function)

`(logistic `*x*`)`  
`(logit `*x*`)`

Logistic and logit functions.
Equivalent to:

```
\eqimage{fig/logistic`
  {$$\mathop{\rm logistic` x = {e^x \over 1 + e^x` = {1 \over 1 + e^{-x``,$$`
\eqimage{fig/logit`
  {$$\mathop{\rm logit` p = \log {p \over 1 - p` = \log {1 \over {1\over p` - 1`.$$`
```
Examples:
logistic x = exp(x)/[1 + exp(x)] = 1/[1 + exp(-x)],
logit p = log p/(1 - p).


These functions are inverses of one another.
The logit function maps a probablity @math{p` in @math{[0, 1]` into
log-odds @math{x` in the extended real line, and the logistic function
maps back from log-odds to probabilities.

The logistic function is defined on the entire real line, but is
ill-conditioned for large negative @var{x`, with condition number
```
\eqimage{fig/cn-logistic`
  {$$x f'(x)/f(x) = {x e^{-x` \over 1 + e^{-x``.$$`
```

Examples:
x f'(x)/f(x) = x exp(-x)/[1 + exp(-x)].

[image](fig/cn-logistic)

The identity
$$\mathop{\rm logistic`(-x) = 1 - \mathop{\rm logistic`(x)$$


Examples:
logistic(-x) = 1 - logistic(x)

may help to rearrange a computation, along with the logistic-1/2
function which ranges from @math{-1/2` to @math{+1/2` and centered at
zero rather than from @math{0` to @math{1` and centered at @math{1/2`.

The sample implementation gives forward relative error bounded by at most
seven times the forward relative error bound of the system math
library's exp, which is usually below 1ulp.

The logit function is defined on the closed unit interval @math{[0,
1]`, but is ill-conditioned near @math{1/2` and @math{1`, with
condition number
```
\eqimage{fig/cn-logit`
  {$$x f'(x)/f(x) = {1/(1 - p) \over \log (p/(1 - p))`.$$`
```


Examples:
x f'(x)/f(x) = 1/[(1 - p) log(p/(1 - p))].

[image](fig/cn-logit)

The identity
$$\mathop{\rm logit`(1 - p) = -\mathop{\rm logit`(p)$$


Examples:
logit(1 - p) = -logit(p)

may help to rearrange a computation, along with the logit1/2+ function
which is defined on @math{-1/2` to @math{+1/2` and centered at zero
rather than on @math{0` to @math{1` and centered at @math{1/2`.

The sample implementation gives forward relative error bounded by at most
ten times the forward relative error bound of the system math
library's log, which is usually below 1ulp.


`(logsumexp `*list*`)`

*List* must be a list of real numbers
$x_1, x_2, \ldots, x_n$.
*x1*, *x2*, @dots{*, *xn*.
Returns an approximation to:
$$\log (e^{x_1` + e^{x_2` + \cdots + e^{x_n`).$$


Examples:
log(exp(*x1*) + exp(*x2*) + ... + exp(*xn*)).

The approximation avoids intermediate overflow and underflow.
To minimize error, the caller should arrange for the numbers to be
sorted from least to greatest.

Edge cases:

If *list* is empty, the result is `-inf`, as if the
intermediate sum were zero.

If *list* contains only finite numbers and `-inf`, the
`-inf` elements are ignored, since the exponential of `-inf`
is zero.

If *list* contains only finite numbers and `+inf`, the result
is `+inf` as if the sum had overflowed.
(Otherwise, overflow is not possible.)

If *list* contains both `-inf` and `+inf`, or if
*list* contains any NaNs, the result is a NaN.

`Logsumexp` never raises any of the standard IEEE 754-2008
floating-point exceptions other than invalid-operation.


`(log1mexp `*x*`)`  
`(log1pexp `*x*`)`

Equivalent to:
```
\eqimage{fig/log1mexp`{$$\mathop{\rm log1mexp` x = \log (1 - e^x),$$`
\eqimage{fig/log1pexp`{$$\mathop{\rm log1pexp` x = \log (1 + e^x).$$`
```

Examples:
log1mexp x = log (1 - e^x),
log1pexp x = log (1 + e^x).

[image](fig/log1mexp)
[image](fig/log1pexp)

Like log1p and expm1, these avoid numerical pathologies with the
intermediate quantities @math{1 - e^x` and @math{1 + e^x` and inputs
to log near @math{1`.

log1mexp computes the complement of a probability @math{p` in
log-space @math{\log p`, and as such is a self-inverse.
It is finite when @math{x < 0`; negative infinity when @math{x =
0`; and invalid otherwise.

log1pexp is related to the logistic function @math{1/(1 + e^{-x`)` ---
specifically, it differs from the logarithm of the logistic function
only by the sign of the input and the output.

The sample implementation gives forward relative error bounded by ten times
the forward relative error bound of the system math library's log and
exp, which is usually below 1ulp.

Beware that although the forward relative error of the MIT/GNU Scheme
*implementations* of these functions is bounded, these
*functions* are ill-conditioned for large negative inputs:
\nobreak\par\nobreak%
$$x f'(x)/f(x) = {\pm x e^x \over (1 \pm e^x) \log(1 \pm e^x)`
  \approx x, \quad\hbox{for $x \ll 0$.`$$
\nobreak\par\nobreak\leavevmode%
\begingroup%
  % Load up the two figures into \box0 and \box1.
  \setbox0=\hbox{\image{fig/cn-log1pexp``%
  \setbox1=\hbox{\image{fig/cn-log1mexp``%
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
      \hbox to\dimen0{\hfill Condition number of log1pexp\hfill``%
    \hfill%
    \vbox{\dimen1=\wd1 \box1    % Save width and dispense with box.
      \hbox to\dimen1{\hfill Condition number of log1mexp\hfill``%
    \hfill%
  `%
\endgroup


Examples:
x f'(x)/f(x) = (+/- x exp(x))/((1 +/- e^x) log(1 +/- e^x)),
  --> x,  for x << 0.

[image](fig/cn-log1mexp)
[image](fig/cn-log1pexp)



`(log-logistic `*x*`)`  
`(logit-exp` *x*`)`

Equivalent to:
```
\eqimage{fig/loglogistic`
  {$$\eqalign{\mathop{\hbox{\rm log-logistic`` x &= \log \mathop{\rm logistic`(x) \cr&= \log [1/(1 + e^{-x`)],`$$`
\eqimage{fig/logitexp`
  {$$\eqalign{\mathop{\hbox{\rm logit-exp`` x &= \mathop{\rm logit`(e^x) \cr&= \log [e^x/(1 - e^x)].`$$`
```

Examples:
log-logistic x = log(logistic(x)) = log [1/(1 + exp(-x))]
logit-exp x = logit(exp(x)) = log [exp(x)/(1 - exp(x))]


Like `logistic` and `logit`, these functions are inverses of
one another.


```
