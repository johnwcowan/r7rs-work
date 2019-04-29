## Date and time arithmetic

This is a WG2 proposal for date and time arithmetic, loosely based on Java's
[Joda Time](http://joda-time.sourceforge.net) functions.
It's possible to implement parts of SRFI 19 on top of it, but it provides much more flexibility.
See also [TimePeriodsCowan](TimePeriodsCowan.md) and [IsoDatesCowan](IsoDatesCowan.md).

## Issues

None at present.

## Instants

For the purposes of this proposal, an *instant* is an exact
or inexact rational number representing
a particular second or fraction of a second
of the TAI scale, such that 0 represents midnight on January 1, 1970 TAI
(equivalent to approximately 8 seconds before midnight Universal Time)
and the value 1 represents one TAI second later.
The current instant can be obtained more or less accurately
by invoking the R7RS procedure `current-second`.

## Instant procedures

These procedures convert between a TAI instant and a *timespec*,
which is a pair whose car is the number of whole seconds and the cdr is
the number of nanoseconds on the Posix time scale, which counts the number
of seconds *excluding leap seconds* since midnight on January 1, 1970
Universal Time.

In this SRFI, the Posix time of a leap second is always the same as
the Posix time of the following second, but the results of calling
the Posix procedure `clock_gettime()` do not necessary agree.

`(tai->posix `*instant*`)`

Converts an instant to the corresponding Posix timespec.

`(posix->tai `*timespec*`)`

Converts a Posix timespec to the corresponding instant.

## Chronologies

A *chronology* is an immutable member of a disjoint type that describes a particular
combination of time zone, calendar, and time scale.
The time scale shows how seconds are mapped to minutes;
the time zone indicates how minutes
and hours in the zone are mapped to the UTC time zone;
the calendar shows how days are mapped to months and
years.  An hour is always 60 minutes and a day is always 24 hours.

This SRFI requires support for numeric time zones expressed as an exact number of hours
between -24 and 24 inclusive that is a multiple of 1/60 representing the number
of hours after UTC, and recommends that the
IANA historical time zone names (which are strings) be supported as well.  It requires
support for the Gregorian (ISO) and Julian calendars.  The only required time scale id
Posix, which has exactly 60 seconds per minute but ignores leap seconds.  The only
required calendar is the Gregorian calendar.

## Chronology procedures

`(default-chronology)`

A chronology used by default.
Its value should normally represent local civil time: the user's current time zone,
the Gregorian calendar, and the UTC time scale.

`(make-chronology `*timezone calendar scale*`)`

Returns a chronology with the specified *timezone* (exact number or string),
*calendar* (a symbol, either `gregorian`, `julian`, or an implementation-specific
symbol), and *scale* (a symbol, either `tai`, `utc`, `posix`, or an
implementation-specific symbol).

`(chronology? `*obj*`)`

Returns `#t` if *obj* is a chronology and `#f` otherwise.

`(chronology-timezone `*chronology*`)`  
`(chronology-calendar `*chronology*`)`  
`(chronology-scale `*chronology*`)`

Return the appropriate values from the *chronology* object.

## Date objects

A *date object* is an immutable member of a disjoint type
that provides information about a specific instant
of time with respect to a certain chronology.
Date objects have multiple numeric-valued fields such as `year` or `minute`,
whose meanings and possible values are determined by the chronology.

## Date object procedures

`(make-ymd-date `[ *chronology* ] *year month day hour minute second*`)`  
`(make-ywd-date `[ *chronology* ] *year week day-of-week hour minute second*`)`  
`(make-yd-date `[ *chronology* ] *year day-of-year hour minute second*`)`

Returns a date object based on a year, month, and day; a year, an ISO
week number, and a day of the week; or a year and a day within the year.
In all cases the hours, minutes, and seconds are provided.  See the
"Date fields" section for an explanation of the numeric values used.
If the *chronology* argument is omitted, the `default-chronology` is used.

`(instant->date `[ *chronology* ] *instant*`)`

Returns a date object referring to *instant*.
If the *chronology* argument is omitted, the `default-chronology` is used.

`(date? `*obj*`)`

Returns `#t` if *obj* is a date object, and `#f` otherwise.

`(date->alist `*date*`)`

Returns a newly allocated alist containing the fields of *date* (see below).

`(date-update `*date*` `*fieldname*` `*value*`)`

Returns a date object based on *date*, but with the field named *fieldname* updated to *value*.
An error that satisfies `date-error?` is signaled if the field is unknown
or the value is out of range.

`(date-adjust `*date*` `*fieldname*` `*increment*`)`

Returns a date object which is later than *date* by *increment*
measured in the units specified by *fieldname*,
or earlier if *increment* is negative.
An error that satisfies`date-error?` is signaled if *fieldname* is unknown.

For example, `(date-increment `*date*` 'day-of-month 7)` adds seven days to *date*.

`(date-chronology `*date*`)`

Returns the chronology associated with *date*.

`(date-field-maximum `*date*` `*fieldname*`)`

`(date-field-minimum `*date*` `*fieldname*`)`

Returns the maximum or minimum legal value of the field named *fieldname*
in the chronology associated with *date*. 
This value is not necessarily the same for all date objects in a particular chronology;
for example, 28 is the maximum value of `day-of-month` if `month` has the value 2 (February)
and `year` is not a leap year.  Returns `#f` if the value cannot be determined
(there is no maximum or minimum year in the Gregorian or Julian calendars, for example).

`(date-round `*date*` `*fieldname*`)`

`(date-ceiling `*date*` `*fieldname*`)`

`(date-floor `*date*` `*fieldname*`)`


Returns a date object which is the same as *date*, but adjusted to the nearest integral value of *fieldname*
using the `round`, `ceiling`, or `floor`
This may cause other fields to change their values as well.


## Date fields

These fields are specified for the Gregorian chronology, but
may be used for other chronologies as well.
Implementation-specific chronologies may support other fields as well.
Unless otherwise noted, all ranges are inclusive at both ends, and all
field values are exact integers that have been rounded down if necessary.

`instant`: The instant.

`timespec`: The Posix timespec.

`year`: The year.  Note that 1 BCE is represented as 0 and 2 BCE as -1.

`month`: The month, where 1 is January and 12 is December.

`day`: The day of the month between 0 and 31 (or less in some months)

`hour`: The hour of the day, where 0 represents the time between
midnight and 1 AM.

`minute`:  The minute of the hour between 0 and 59.

`second`: The second of the minute between 0 and 60 exclusive.  This number
may be exact or inexact.

`week`: The ISO week number.  Week 1 of a year is the week
from Monday to Sunday containing January 4.  The last week
of a year may be 52 or 53, and may include days from the
following year.

`day-of-week`: The day of the date's week, where Monday is 1 and Sunday is 7.

`week-year`: The year, except that the days before week 1 begins belong to the
previous year.

`day-of-year`: The day of the year, in the range 1-365 in non-leap years and 1-366 in leap years.

`daylight-saving-time`: `#t` if daylight saving time (summer time) was in effect,
or `#f` if not.  This field discriminates between 2:00 A.M. daylight time and 2:00 A.M. standard time
on the day when daylight saving time ends in the U.S.
(and the corresponding periods for other daylight saving time regimes).

`julian-day`: The wjhole number of days between this date and noon Universal Time, January 1, 4173 B.C.E. Julian
(which is November 24, 4714 B.C.E. Gregorian).

`modified-julian-day`: The whole number of days between this date and midnight Universal Time, November 17, 1858
Gregorian.  This number may be exact or inexact.

`local-time-offset`: The local time zone offset (standard or daylight saving, as the case may be)
in hours ahead of UTC.  This is an exact number which is a multiple of 1/60.


`second-of-day`: The second of the day, in the range 0 to 86400 exclusive.

## Comparators

`date-comparator`

A comparator suitable for ordering date objects by their underlying instants.

## Exceptions

`(date-error `*obj*`)`

Returns `#t` if *obj* is an object signaled by an error as specified above, and `#f` otherwise.

## Conversion to ISO 8601 date strings

TBD

## Implementation notes

The sample implementation provides only an approximation of the mapping between TAI
and UTC.  The two time scales are assumed to be synchronized at 00:00:00
on January 1, 1958 and at all times before that.  From 1958 through 1971,
the relationship is complex, but since 1971 the two scales have been kept
within 0.9 seconds of each other by inserting leap seconds as needed.

For the messy area, the implementation pretends that there were leap seconds
at the end of December 31 (that is, at 23:59:60 proleptic UTC time) in the following
years:  1959, 1961, 1963, 1964, 1965, 1966, 1967, 1968, 1970, and 1971.
This has the following desirable effects: the TAI-UTC offset is 0 in 1958
(true by definition),
is 8 (which is within a few milliseconds of the true value) when the Unix
epoch begins, and is 10 at the start of 1972 when the leap second regime
begins.  Not having a leap second in 1969 ensures that there is none
just before the Unix epoch.  The implementation also pretends,
*faute de mieux*, that there will be no more leap seconds in the future.

To update the leap second tables, download the file
[http://maia.usno.navy.mil/ser7/tai-utc.dat](http://maia.usno.navy.mil/ser7/tai-utc.dat}
and run the script `update-leapsec`, which is written in portable Scheme.


