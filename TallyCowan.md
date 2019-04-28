## Introduction

A *tally* is a [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html) accumulator
whose purpose is to keep running descriptive statistics on *observations*, each represented by a real number,
that are injected into it.
It can compute a variety of simple statistics such as the arithmetic mean, the variance, and the standard deviation.
It does not record the observations themselves, and therefore cannot compute the media, the mode,
or even the standard deviation.

A *histogram* is a different kind of accumulator which maintains a number of *bins* decided in advance,
and counts how many observations fall into each bin.  It can return bin counts and indicate the current median and modal bins.
The bins are of equal width.

## Constructors

`(make-tally)`

Returns a newly allocated tally with no observations.
When the tally is invoked with a real number as an argument, the number is injected as an observation.
When the tally is invoked with an end of file object, an opaque state object is returned.
It is an error to modify this object.
It is explicitly permitted to add more observations after injecting an end of file object.
The state objects returned by the injection of multiple end of file objects
may or may not be the same (in the sense of `eqv?`).

The various procedures below are provided for extracting statistics from the state.

`(make-histogram ` *low high width*`)`

Returns a newly allocated histogram with no observations.
It will have sufficient bins to count observations from *low* (inclusive) to *high* (exclusive)
with the specified *width*.  Observations that fall on a bin boundary are always placed in the higher bin.
There are two additional bins for observations less than *low* and greater than
or equal to *high*.  The calling conventions are the same as for tallies.

## Generators

`(generator->tally! `*tally* *generator*`)`

Injects the values returned by *generator* into *tally*.

`(generator->histogram! `*histogram* *generator*`)`

Injects the values returned by *generator* into *histogram*.

## Basic statistics

`(tally-count `*state*`)`

Returns the number of observations injected so far as an exact integer.

`(tally-sum `*state*`)`

Returns the sum of all observations injected so far.

`(tally-sum-squares `*state*`)`

Returns the sum of the squares of all observations injected so far.

`(tally-product `*tally*`)`

Returns the product of all observations injected so far.

`(tally-harmonic-sum `*tally*`)`

Returns the harmonic sum of all observations injected so far.
This is the sum of the reciprocals of the observation values.
If any observation value is zero, returns `+nan.0`.

`(tally-mean `*state*`)`

Returns the arithmetic mean of all observations injected so far.
This is the quotient of the sum and the count, or `+nan.0` if the count is zero.

`(tally-geometric-mean `*state*`)`

Returns the geometric mean of all observations injected so far.
This is the *n*th root of the product, where *n* is the count of observations,
or `+nan.0` if the count is zero.

`(tally-harmonic-mean `*state*`)`

Returns the harmonic mean of all observations injected so far.
This is the count divided by the harmonic sum, or `+nan.0` if the count is zero.

`(tally-max `*state*`)`

Returns the maximum observation injected so far.
If no observations have been injected, returns `-inf.0`.

`(tally-min `*state*`)`

Returns the minimum observation injected so far.
If no observations have been injected, returns `+inf.0`.

`(tally-range `*state*`)`

Returns the difference between the maximum and the minimum.
If no observations have been injected, returns `+nan.0`.

## Histogram values

`(histogram-low `*histogram-state*`)`

`(histogram-high `*histogram-state*`)`

`(histogram-width `*histogram-state*`)`

Return the basic parameters of *histogram*.

`(histogram-bin-count `*histogram-state*`)`

Return the number of bins in *histogram*.

`(histogram-bins `*histogram-state*`)`

Return a vector whose elements are the counts in the bins in *histogram-state*.
It is an error to modify this vector.

`(histogram-bin-ref `*histogram-state index*`)`

Return the number of observations in the histogram bin numbered *index*.
It is an error if *index* is less than zero, or greater than or equal to the number of bins.

`(histogram-underflow `*histogram-state*`)`

Return the number of observations less than the *low* parameter of *histogram*.

`(histogram-overflow `*histogram-state*`)`

Return the number of observations greater than or equal to the *high* parameter of *histogram*.

`(histogram-modal-bins `*histogram-state*`)`

Return a list of the indexes to the bins that contain the largest number of observations.
The underflow and overflow bins are excluded. 

Note that the modal *bin* does not necessarily contain the modal *value*;
if the width of a bin is 1.0 and the values are {1, 1.1, 1.2, 1.3, 1.4, 1.5, 2.0, 2.0, 2.0},
then the modal bin is 1.0-2.0 whereas the modal value, 2.0, is in the non-modal bin 2.0-3.0.

`(histogram-median-interval `*histogram-state*`)`

Return two values, a lower (inclusive) and upper (exclusive) bound for the median observation.
Normally the upper and lower bounds are one bin width apart,
but if the number of items in bins lower than *k* for some *k*
is equal to the number of items in higher bins, the values will be two bin widths apart.
he underflow and overflow bins are excluded.

