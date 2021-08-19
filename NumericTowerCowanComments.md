## John Cowan's comments

See [NumericTower](https://doc.scheme.org/surveys/NumericTower/)
for the meaning of the codes below.

In my opinion, the reasonable general-purpose towers are:

 * `--+-` (fixnums and flonums)
   This tower's operations have the advantage of running in constant time on modern hardware
 * `+-+-` (fixnums, bignums, and flonums)
 * `+++-` (fixnums, bignums, ratnums, and flonums)
 * `++++` (full).
 
 The `++--` tower (ratnums but not flonums) may be reasonable for some purposes,
 but numerical code will often run very slowly without inexact support
 due to fractions which grow larger and larger.
 
 The `----` (fixnum-only) tower might possibly be appropriate
 where only a tiny amount of space is available
 and essentially no numerical work will be done.

See [NumericTowerManisComments](NumericTowerManisComments.md) for Vincent Manis's
comments on an earlier version of this page.
