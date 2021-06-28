## Date and duration operations

This is a SRFI for date and duration operations.
All the objects discussed here are immutable.
This SRFI supports only the
[proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar).

## Issues

None at this time.

## Instants and time objects

Scheme uses two internal formats for absolute timestamps, which
in this SRFI are called *instants* and *time-objects*.

An *instant* is an exact or inexact number representing a count of
seconds since eight seconds before midnight January 1, 1970 in the UTC timezone.
The current instant can be obtained by calling the R7RS procedure `current-second`.

A *time-object* is an object defined by
FIXME: [SRFI 174 bis](https://github.com/pre-srfi/time-objects/blob/master/TimeObjects.md)
that represents a count of whole seconds and nanoseconds
plus a time type such as UTC, TAI, or duration.
The current UTC time object can be obtained by calling the SRFI 170
procedure `posix-time`.
SRFI 174 bis has procedures for converting between UTC and TAI time objects
and between either of them and instants.  Consequently, the procedures
of this SRFI only accept UTC time objects.

In this SRFI, a UTC time object during a leap second is always assumed to be the same
(in the sense of `=`) for both the seconds and the nanoseconds) as
the time object for the following second, but the results of actually calling
`posix-time` in the vicinity of a leap second do not necessarily agree.


## Date objects

A *date object* is an immutable member of a disjoint type
that provides information about a specific time object
with respect to a certain time zone.
Date objects have multiple numeric-valued fields
that can be extracted.
They are listed in the "Date Fields" section below.

## Duration objects

A *duration object* represents the amount of elapsed time
between an earlier instance or time object and a later one
that has been broken into smaller intervals of time.
There are two types of duration objects: a standard duration
object is specified as a number of years, months, days,
hours, minutes, seconds, and nanosections; a week duration
object is specified as a number of ISO years and weeks,
and cannot be more finely specified.

Note that certain familiar identities do not hold: while the
number of minutes in a day and the number of months in a year
are both fixed, the number of seconds in a minute is not, because
leap seconds, and the number of days in a month is not fixed either.
Therefore the most compact format for a duration object is in
months, minutes, and nanoseconds, all of which can be stored as 64-bit fixnums.


## Time zones

Local civil time is everywhere specified using
an offset from Universal Time.  (For a few years this was not
true in Saudi Arabia, but this SRFI ignores the Saudi government's
attempt to change local civil time so that hour 0 began at astronomical sunset.)

In each political jurisdiction the rules for changing the offset vary,
both the annual cycle of standard vs. daylight saving time (if in effect)
and any unpredictable changes in the offset applied by political entities.
This SRFI therefore requires a *time zone* to be supplied in order
to convert a UTC time object to local time.  It requires support for numeric timezones,
which are Universal Time minus local time in seconds, and strongly recommends
support for named time zones as defined by the
[IANA time zone database](https://www.iana.org/time-zones); these are strings.

When local time jumps backwards (typically some time in the autumn in the temperate zones,
or else for political reasons at any time),
the same local time can represent two different Universal Time values.
Such a situation is called a *time fold* and is represented as 0 for the earlier time
and 1 for the later time.  For example, the fold is 0 at 2:00 A.M. daylight time
and 1 at 2:00 A.M. standard time one hour later
on the day when daylight saving time ends in the U.S.
The fold for any unaffected time is always 0.  The idea behind the name is that
the local time scale is folded up, as it were, replicating the same local times.

The current timezone can generally be obtained from the `TZ` environment variable.
Note that there is no concept in this SRFI of
a date or time without a timezone; some timezone must be supplied whenever a
date object is created.

## Localization

This SRFI does not deal with localization beyond the matter of time zones.
It does not know the names of the months or of the days of the week in any language,
or the proper ordering of day, month, and year,
or the names and starting dates of the Japanese eras or any others,
or whether local clocks are 12-hour or 24-hour,
or how to spell "AM" and "PM",
or anything about non-Gregorian calendars,
including those proposed for other celestial bodies than the Earth.
Sufficient to the day is the evil thereof.

Note that names like "EST" are actually not timezone names but names of offsets,
and are not globally unique even in a single language:
this SRFI does not deal with them either.

## Procedures

### Date object procedures

`(date `*alist*`)`  

Returns a date object based on the values fields in the alist,
which maps symbols (called fields) to specific values.
The fields `year`, `month`, `day`, `hours`, `minute`, `second`,
`nanosecond`, and `timezone` are required.
The fields `nanoseconds` and `fold` are optional, and default to 0.
An error satisfying `date-error?` is signaled if any other fields are present.

`(time-object->date `*timezone time-object*`)`

Returns a date object referring to *time-object* modified by *timezone*.

`(date? `*obj*`)`

Returns `#t` if *obj* is a date object, and `#f` otherwise.

`(date->alist `*date*`)`

Returns a newly allocated alist containing all the fields of *date* (see below).

`(date-ref `*date fieldname*`)`

Retrieves the value of the field named by the symbol *fieldname* from *date*.
This may be more efficient than generating an alist, but may also be
less efficient if several different fields are required.

`(date-update `*date fieldname value*`)`

Returns a date object based on *date*,
but with the field named *fieldname* updated to *value*.
An error that satisfies `date-error?` is signaled if the field is unknown
or the value is out of range.

`(date-adjust `*date fieldname increment*`)`

Returns a date object which is later than *date* by *increment*
measured in the units specified by *fieldname*,
or earlier if *increment* is negative.
For example, `(date-adjust `*date*` 'day-of-month 7)`
adds seven days to *date*.

`(date-round `*date fieldname*`)`

`(date-ceiling `*date fieldname*`)`

`(date-floor `*date fieldname*`)`

`(date-truncate `*date fieldname*`)`

Returns a date object which is the same as *date*,
but adjusted to the nearest integral value of *fieldname*
using the conventions of `round`, `ceiling`, `floor`, or `truncate`.
This may cause other fields to change their values as well.

### Date fields

Unless otherwise noted, all
field values are exact integers that have been rounded down if necessary.
All ranges are inclusive at both ends.

`time-object`: The time object of this date.

`timezone`: The timezone with which this date was created.

`local-time-offset`: The local time zone offset in effect at this
date at the specific timezone in seconds ahead of UTC.

`year`: The year.  Note that 1 BCE is represented as 0 and 2 BCE as -1.

`month`: The month, where 1 is January and 12 is December.

`day`: The day of the month between 1 and 31 (or less in some months)

`hour`: The hour of the day, where 0 represents the time between
midnight and 1 AM and 23 represents the time between 11 PM
and midnight.  Midnight itself is both minute 0 of hour 0
of the following day and minute 0 of hour 24 of the preceding day.

`minute`:  The minute of the hour between 0 and 59.

`second`: The second of the minute between 0 and 60.

`day-of-week`: The day of the week, where Monday is 1 and Sunday is 7.

`days-in-month`: The number of days in the date's month, between 1 and 31.

`day-of-year`: The day of the year, between 1 and 365 in non-leap years
and 1 and 366 in leap years.

`days-in-year`: The number of days in this date's year.

`julian-day`: The whole number of days between this date and noon Universal Time, January 1, 4173 B.C.E. Julian
(which is November 24, 4714 B.C.E. Gregorian).  Leap seconds are ignored.

`modified-julian-day`: The whole number of days between this date and midnight Universal Time, November 17, 1858
Gregorian.  Leap seconds are ignored.

`iso-week-number`: The number of the week in this date's year, between 1 and either 52 or 53.
Week 1 is the first full week where Thursday ia in January.

`iso-week-year`: The number of the period which begins in week 1 and ends in week 52 or 53.
It is the same as `year` except for up to six days in January and December.

`seconds-in-minute`: the number of seconds in the date's minute.

`second-of-day`: The second of the date's day.

`seconds-in-day`: The total number of seconds in the date's day.

`time-fold`:  The time fold associated with this date (see above).

### Duration procedures

`(duration `*alist*`)`  

Returns a duration object based on the values fields in the alist,
which maps symbols (called fields) to specific values.
A standard duration object has the
fields `years`, `months`, `days`, `hours`, `minutes`, `seconds`,
and `nanoseconds`; 
a week duration object has the fields
`iso-week-years` and `iso-week-numbers`
Missing fields are interpreted as 0.
An error satisfying `date-error?` is signaled if any other fields are present.

`(duration `*earlier later*`)`

Returns a duration object representing the elapsed time between
time objects *earlier* (inclusive) and *later* (exclusive).

`(time-object->duration `*timezone time-object*`)`

Returns a duration object representing the elapsed time
represented by *time-object*.
It is an error if *time-object* is not of type `time-duration`.

`(duration? `*obj*`)`

Returns `#t` if *obj* is a duration object, and `#f` otherwise.

`(standard-duration? `*obj*`)

Returns `#t` if *obj* is a standard duration object, and `#f` otherwise.

`(week-duration? `*obj*`)

Returns `#t` if *obj* is a week duration object, and `#f` otherwise.

`(duration->alist `*duration*`)`

Returns a newly allocated alist containing all the fields of *duration*.

`(duration-ref `*duration fieldname*`)`

Retrieves the value of the field named by the symbol *fieldname* from *duration*.
This may be more efficient than generating an alist, but may also be
less efficient if several different fields are required.

`(duration-adjust `*duration fieldname increment*`)`

Returns a duration object which is later than *duration* by *increment*
measured in the units specified by *fieldname*,
or earlier if *increment* is negative.
For example, `(duration-adjust `*duration*` ' day-of-month 7)`
adds seven days to *duration*.

`(duration-round `*duration fieldname*`)`

`(duration-ceiling `*duration fieldname*`)`

`(duration-floor `*duration fieldname*`)`

`(duration-truncate `*duration fieldname*`)`

Returns a duration object which is the same as *duration*,
but adjusted to the nearest integral value of *fieldname*
using the conventions of `round`, `ceiling`, `floor`, or `truncate`.
This may cause other fields to change their values as well.

### ISO formatting

TBD

### Comparators

`date-comparator`

A comparator suitable for ordering date objects by their underlying time objects.

`duration-comparator`

A comparator suitable for ordering duration objects by their durations.

## Exceptions

`(date-error? `*obj*`)`

Returns `#t` if *obj* was signaled by one of the above procedures, or
`#f` otherwise.
