## Time durations

A *duration* is an immutable object of a disjoint type representing a period of time,
the sum of some number of years, months, weeks, days, hours, minutes, seconds, and nanoseconds.
Note that a duration cannot always be translated directly to a number of seconds, because
months, days, hours, and minutes are not all the same length.
However, all weeks are assumed to be 7 days,
and all years are assumed to be 12 months.

## Duration procedures

`(duration `*alist*`)`

Returns a duration from an alist similar to the one required by `date`,
except that the fields are
`years`, `months`, `days`, `hours`, `minutes` `seconds`, and `nanoseconds`.

`(duration? `*obj*`)`

Returns `#t` if *obj* is a duration, and `#f` otherwise.

`(duration-years `*duration*`)`  
`(duration-months `*duration*`)`  
`(duration-days `*duration*`)`  
`(duration-hours `*duration*`)`  
`(duration-minutes `*duration*`)`  
`(duration-seconds `*duration*`)`  
`(duration-nanoseconds `*duration*`)`

Extract the individual value of each field of *duration*.
The returned values don't
necessarily correspond with the original values
from which the duration was constructed;
in particular, a duration of 28 months is treated
as if it had been specified as 2 years 4 months.

`(duration->alist `*duration*`)`

Returns a newly allocated alist mapping the symbols
`years`, `months`, etc. into their values.
However, `weeks` is not included.

`(duration->iso `*duration*`)`

Returns an ISO 8601 string representing *duration*.

`(iso->duration `*string*`)`

Returns a duration corresponding to an ISO 8601 string that represents a duration.
As long as all strings that can be produced by *iso->duration*
are accepted, this procedure does not have to be a general ISO 8601
duration parser.

`(duration+ `*duration1 duration2*`)`  
`(duration- `*duration1 duration2*`)`

Adds/subtracts *duration1* and *duration2* componentwise, returning a duration.

`(duration* `*duration exact-number*`)`  
`(duration- `*duration exact-number*`)`

Multiplies/divides *duration* componentwise by *exact-number* componentwise, returning a duration.

## Mixed procedures

`(date-add `*date duration*`)`

Adds *duration* to *date* to produce another date.  Components of the duration are added
in the order specified above: first years, then months, etc.

`(date-subtract `*date1 date2*`)`

Subtracts *date1* from *date2* to produce a duration, which will always have
0 years and 0 months.

