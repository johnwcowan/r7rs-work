## Time durations

A *duration* is an immutable object of a disjoint type representing a period of time,
the sum of some number of years, months, weeks, days, hours, minutes, seconds, and nanoseconds.
Note that a duration cannot always be translated directly to a number of seconds, because months are
not all the same length.

## Duration procedures

`(make-duration `*years months days hours minutes seconds nanoseconds*`)`

`(make-week-duration `*weeks days hours minutes seconds nanoseconds*`)`

Returns a duration.  The values of each field must be exact integers.

`(duration? `*obj*`)`

Returns `#t` if *obj* is a duration, and `#f` otherwise.

`(duration-years `*duration*`)`  
`(duration-months `*duration*`)`  
`(duration-weeks `*duration*`)`  
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
Note that durations created with `make-duration` report 0 weeks,
whereas durations created with `make-week-duration` report 0 months and 0 years.

`(duration-total-months `*duration*`)`

Reports the total number of months in *duration*, counting each year as 12 months.

`(duration-total-seconds `*duration*`)`

Reports the number of seconds, exclusive of any months or years, in *duration*,
counting each week as 7 days, each day as 24 hours, each hour as 60 minutes,
and each minute as 60 seconds.

`(duration->alist `*duration*`)`

Returns a newly allocated alist mapping the symbols
`years`, `months`, etc. into their values.

`(duration->iso `*duration*`)`

Returns an ISO 8601 string representing *duration*.

`(iso->duration `*string*`)`

Returns a duration corresponding to an ISO string that represents a duration.
As long as all strings that can be produced by *iso->duration*
are accepted, this procedure does not have to be a general ISO
duration parser.

`(date-add `*date duration*`)`

Adds *duration* to *date* to produce another date.  Components of the duration are added
in the order specified above: first years, then months, etc.

`(date-subtract `*date1 date2*`)`

Subtracts *date1* from *date2* to produce a duration, which will always have
0 years and 0 months.