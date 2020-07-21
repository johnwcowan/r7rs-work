## NaN functions

`(nan-negative? `*nan*`)`

Returns `#t` if the sign bit of *nan* is 1 and `#f` otherwise.

`(nan-quiet? `*nan*`)`

Returns `#t` if *nan* is a quiet NaN, and `#f` otherwise.

`(nan-payload `*nan*`)`

Returns the payload of *nan* as an exact integer.

`(nan= `*nan1 nan2*`)`

Returns `#t` if *nan1* and *nan2* have the same sign, quiet bit,
and payload; and `#f` otherwise.
