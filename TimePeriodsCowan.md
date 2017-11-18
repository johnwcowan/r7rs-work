## Time durations and intervals

A *duration* is an immutable object of a disjoint type and representing a period of time according a particular chronology.  In the ISO, Gregorian, and Julian chronologies, it is the sum of
some number of years, months, weeks, days, hours, minutes, and seconds.

A *time interval* is an immutable object belonging to a distinct type and representing the elapsed time between two instants of time.  See [TimeAdvancedCowan](TimeAdvancedCowan.md) for the definition of instants and date objects.

## Duration procedures

`(make-duration `[[|*chronology* ]]` `*alist*`)`

Returns a duration based on *chronology*, defaulting to the value of `(current-chronology)`.  *Alist* is an alist mapping duration field names understood by that chronology to their values.  In the ISO, Gregorian, and Julian calendars, the field names are
`years`, `months`, `weeks`, `days`, `hours`, `minutes`, and `seconds`.
The values of each field must be exact integers.

`(duration? `*obj*`)`

Returns `#t` if *obj* is a duration, and `#f` otherwise.

`(duration->alist `*duration*`)`

Returns a newly allocated alist mapping field names existing in *duration* into their values.  Fields mapped to `#f` don't appear in the alist.

`(duration-field `*fieldname*` `*duration*`)`

Returns the value associated with *fieldname*.  For example, `(duration-field 'weeks d)` returns the number of weeks in duration *d*.  If the duration was not constructed with a
particular unit, `#f` is returned instead.  No conversion is done: a 7-day interval is not considered equivalent to a 1-week interval, for example.


## Time interval procedures

An interval represents the time between two or more instants, possibly repeated more than once.  Intervals belong to an immutable disjoint type.

`(make-interval `*date-or-duration-1*` `*date-or-duration-2*` ` [[|*repetition* ]] `)`

Returns an interval which:

* if both arguments are dates, extends from the first argument to the second;

* if *date-or-duration-1* is a date and *date-or-duration-2* is a duration, extends from the first argument for the duration specified by the second argument;

* if *date-or-duration-1* is a duration and *date-or-duration-2* is a date, extends for the duration of the first argument until the date specified by the second argument.

An error is signaled if both arguments are durations, or if the arguments do not share the same chronology, or the dates do not have enough fields to specify an instant.

The *repetition* argument specifies how many times the interval is repeated, 1 by default.  If the argument is `#t`, the interval is repeated indefinitely.

`(interval? `*obj*`)`

Returns `#t` if *obj* is an interval and `#f` otherwise.

`(interval-start `*interval*`)`

`(interval-stop `*interval*`)`

`(interval-duration `*interval*`)`

`(interval-repetition `*interval*`)`

Returns the start date, stop date, duration, or repetition count of *interval*.  Depending on how the interval was constructed, at least one of these will have to be calculated.
