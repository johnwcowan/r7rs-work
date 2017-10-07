== Introduction ==

A ''tally'' is a member of a disjoint type whose purpose is to keep running descriptive statistics on ''observations'', consisting of one or two Scheme numbers, that are injected into it.  It can compute a variety of simple statistics such as the arithmetic mean, the variance, and the standard deviation.  It does not record the observations themselves, and therefore cannot compute the median or the mode.  It is also possible to inject a secondary observation at the same time as the primary one; this does not affect the basic statistics, but is used for covariance and correlation computations.

A ''histogram'' is a type of tally which also maintains a number of ''bins'' decided in advance, and counts how many observations fall into each bin.  It can return bin counts and indicate the current median and modal bins.  The bins are of equal width.

== Constructors ==

`(make-tally `[[|''key'' ]]`)`

Returns a newly allocated tally with no observations.  The ''key'' is an arbitrary Scheme object useful for distinguishing this tally from other tallies; the default key is `#f`.

`(make-histogram ` ''low high width'' [[|''key'' ]]`)`

Returns a newly allocated histogram with no observations.  It will have sufficient bins to count observations from ''low'' (inclusive) to ''high'' (exclusive) with the specified ''width''.  Observations that fall on a bin boundary are always placed in the higher bin.  There are two additional bins for observations lower than ''low'' and higher than ''high''.  The ''key'' is as for `make-tally`.

== Injectors ==

`(tally! `''tally'' ''value''`)`

Injects ''value'' into ''tally'' and updates all descriptive statistics.

`(tally-weighted! `''tally'' ''value'' ''weight''`)`

Injects ''value'' into ''tally'', assigning it a weight of ''weight'' for the purpose of calculating weighted sums and weighted averages, and updates all descriptive statistics.

`(tally-correlated! `''tally'' ''value,,1,,'' ''value,,2,,''`)`

Injects ''value,,1,,'' into ''tally'', tracking ''value,,2,,'' separately for the purpose of calculating covariance and correlation.

`(tally-weighted-correlated! `''tally'' ''value,,1,,'' ''weight'' ''value,,2,,''`)`

Injects ''value,,1,,'' into ''tally'', assigning it a weight of ''weight'' and tracking ''value,,2,,'' separately for the purpose of calculating covariance and correlation.

== Generators ==


`(generator->tally! `''tally'' ''generator''`)`

Injects the values returned by ''generator'' into ''tally'' and updates all descriptive statistics.

`(weighted-generators->tally! `''tally'' ''generator,,v,,'' ''generator,,w,,''`)`

Injects the values returned by ''generator,,v,,'' into ''tally'', assigning each a weight returned from ''generator,,w,,'' for the purpose of calculating weighted sums and weighted averages, and updates all descriptive statistics.  Tallying stops when either generator is exhausted, at which point the other generator is in an undefined state.

`(correlated-generators->tally! `''tally'' ''generator,,v1,,'' ''generator,,v2,,''`)`

Injects the values returned by ''generator,,v1,,'' into ''tally'', tracking the alues returned by ''generator,,v2,,'' separately for the purpose of calculating covariance and correlation. Tallying stops when either generator is exhausted, at which point the other generator is in an undefined state.

`(weighted-correlated-generators->tally! `''tally'' ''generator,,v1,,'' ''generator,,w,,'' ''generator,,v2,,''`)`

Injects the values returned by ''generator,,v1,,'' into ''tally'', weighted by the values returned by ''generator,,w,,'' and tracking the alues returned by ''generator,,v2,,'' separately for the purpose of calculating covariance and correlation. Tallying stops when any generator is exhausted, at which point the other generators are in an undefined state.

== Basic statistics ==

`(tally-key `''tally''`)`

Returns the key of ''tally''.

`(tally-count `''tally''`)`

Returns the number of observations injected so far as an exact integer.

`(tally-sum `''tally''`)`

Returns the sum of all observations injected so far.

`(tally-sum-squares `''tally''`)`

Returns the sum of the squares of all observations injected so far.

`(tally-product `''tally''`)`

Returns the product of all observations injected so far.

`(tally-harmonic-sum `''tally''`)`

Returns the harmonic sum of all observations injected so far.  This is the sum of the reciprocals of the observation values.  If any observation value is zero, returns `+nan.0`.

`(tally-mean `''tally''`)`

Returns the arithmetic mean of all observations injected so far.  This is the quotient of the sum and the count, or `+nan.0` if the count is zero.

`(tally-geometric-mean `''tally''`)`

Returns the geometric mean of all observations injected so far.  This is the ''n''th root of the product, where ''n'' is the count of observations.

`(tally-harmonic-mean `''tally''`)`

Returns the harmonic mean of all observations injected so far.  This is the count divided by the harmonic sum, or `+nan.0` if the count is zero.

`(tally-max `''tally''`)`

Returns the maximum observation injected so far.  If no observations have been injected, returns `-inf.0`.

`(tally-min `''tally''`)`

Returns the minimum observation injected so far.  If no observations have been injected, returns `+inf.0`.

`(tally-weighted-sum `''tally''`)`

Returns the weighted sum (that is, the sum of observations multiplied by their weights, treating a missing weight as 1) of the observations and weights injected so far.

`(tally-weighted-mean `''tally''`)`

Returns the weighted arithmetic mean  (that is, the arithmetic mean) of observations multiplied by their weights, treating a missing weight as 1) of the observations and weights injected so far.  If no observations have been injected, returns `+nan.0`.

== Range and variance ==

`(tally-range `''tally''`)`

Returns the difference between the maximum and the minimum.  If no observations have been injected, returns `+nan.0`.

`(tally-sample-variance `''tally''`)`

Returns the sample variance of the observations injected so far.  If no observations have been injected, returns `+nan.0`.

`(tally-population-variance `''tally''`)`

Returns the population variance of the observations injected so far.  If no observations have been injected, returns `+nan.0`.

`(tally-sample-standard-deviation `''tally''`)`

Returns the sample standard deviation of the observations injected so far.  If no observations have been injected, returns `+nan.0`.

`(tally-population-standard-deviation `''tally''`)`

Returns the population standard deviation of the observations injected so far.  If no observations have been injected, returns `+nan.0`.

== Covariance and correlation ==

`(tally-sample-covariance `''tally''`)`

Returns the sample covariance of the pairs of observations injected so far.  If no observations have been injected, returns `+nan.0`.

`(tally-population-covariance `''tally''`)`

Returns the population covariance of the pairs of observations injected so far.  If no observations have been injected, returns `+nan.0`.

`(tally-correlation `''tally''`)`

Returns the correlation based on the pairs of observations injected so far.

== Histogram values ==

Note:  A tally that is not a histogram looks to these functions like a histogram whose low, high, and width values are all zero.

`(histogram-low `''histogram''`)`

`(histogram-high `''histogram''`)`

`(histogram-width `''histogram''`)`

Return the basic parameters of ''histogram''.

`(histogram-bin-count `''histogram''`)`

Return the number of bins in ''histogram''.

`(histogram-bins `''histogram''`)`

Return a vector whose elements are the counts in the bins in ''histogram''.  It is an error to modify this vector.

`(histogram-bin-ref `''histogram index''`)`

Return the number of observations in the histogram bin numbered ''index''.  It is an error if ''index'' is less than zero, or greater than or equal to the number of bins.

`(histogram-underflow `''histogram''`)`

Return the number of observations less than the ''low'' parameter of ''histogram''.

`(histogram-overflow `''histogram''`)`

Return the number of observations greater than or equal to the ''high'' parameter of ''histogram''.

`(histogram-modal-bins `''histogram''`)`

Return a list of the indexes to the bins that contain the maximum number of observations.  The underflow and overflow bins are excluded. Note that the modal ''bin'' does not necessarily contain the modal ''value''; if the width of a bin is 1.0 and the values are {1, 1.1, 1.2, 1.3, 1.4, 1.5, 2.0, 2.0, 2.0}, then the modal bin is 1.0-2.0 whereas the modal value, 2.0, is in the non-modal bin 2.0-3.0.

`(histogram-median-interval `''histogram''`)`

Return two values, a lower (inclusive) and upper (exclusive) bound for the median observation.  Normally the upper and lower bounds are one bin width apart, but if the number of items in bins lower than ''k'' for some ''k'' is equal to the number of items in higher bins, the values will be two bin widths apart.  The underflow and overflow bins are excluded.
 