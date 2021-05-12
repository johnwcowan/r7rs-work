## Date and time operations

This is a SRFI for date and time operations.
It's possible to implement parts of SRFI 19 on top of it,
but it is both simpler to use and more flexible.
All the objects discussed here (with the technical
exception of timespecs) are immutable.
This SRFI supports only the
[proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar).

## Issues

None at this time.

## Instants and timespecs

Scheme uses two internal formats for absolute timestamps, which
in this SRFI are called *instants* and *timespecs*.

An *instant* is an exact
or inexact rational number representing
a particular second or fraction of a second
of the TAI scale, such that 0 represents midnight on January 1, 1970 TAI
(equivalent to approximately 8 seconds before midnight Universal Time)
and the value 1 represents one TAI second later.
The current instant can be obtained more or less accurately
by invoking the R7RS-small procedure `current-second`.

A *timespec* is an object defined by [SRFI 174](http://srfi.schemers.org/srfi-174/srfi-174.html)
representing a count of whole seconds and nanoseconds, but *excluding leap seconds*.
The current timespec can be obtained by calling the SRFI 170
procedure `posix-time`.
In this SRFI, the timespec during a leap second is always assumed to be the same
(in the sense of `=` for both the seconds and the nanoseconds) as
the timespec for the following second, but the results of actually calling
`posix-time` in the vicinity of a leap second do not necessarily agree.

## Date objects

A *date object* is an immutable member of a disjoint type
that provides information about a specific instant
of time with respect to a certain time zone.
Date objects have multiple numeric-valued fields
that can be extracted.
They are listed in the "Date Fields" section below.

## Duration objects

A *duration object* represents the amount of elapsed time
between an earlier instance or timespec and a later one.
There are two types of duration objects: a standard duration
object is specified as a number of years, months, days,
hours, minutes, seconds, and nanosections; a week duration
objects is specified as a number of ISO years and weeks,
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
to convert a timespec or instant to local time.  It requires support for numeric timezones,
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
or the names and starting dates of the Japanese eras,
or whether local clocks are 12-hour or 24-hour,
or how to spell "AM" and "PM",
or anything about non-Gregorian calendars,
including those proposed for other celestial bodies.
Sufficient to the day is the evil thereof.

Note that names like "EST" are actually not timezone names but names of offsets,
and are not globally unique even in a single language:
this SRFI does not deal with them either.

## Procedures

### Instance and timespec procedures

`(tai->posix `*instant*`)`

Converts an instant to the corresponding timespec.
Because instants are inexact numbers, the correspondence is inexact.

`(posix->tai `*timespec leapsec*`)`

Converts a timespec to the corresponding instant.
Because instants are inexact numbers, the correspondence is inexact.

The *timespec* can refer ambiguously
to a leap second (23:59:60) or to the second just before it
(23:59:59).  If that is the case and *leapsec* is true,
the returned value will refer to the leap second.  Otherwise,
it will refer to the second before it.

`(instant->iso `*instant*`)`  
`(timespec->iso `*timespec*`)`

Converts a timespec/instant to an ISO 8601 string representing the year,
month, day of the month, hour, minute, second, and fraction of a second.
All such ISO 8601 strings are in the UTC timezone.

`(iso->instant `*string*`)`  
`(iso->timespec `*string*`)`

Converts an ISO 8601 string of the form output by `instant->iso`
or `timespec->iso` to an instant/timespec.
As long as this procedure can accept any string
generated by `instant->iso` or `timespec->iso`, it does not need to be a general
ISO 8601 parser.

### Date object procedures

`(date `*alist*`)`  

Returns a date object based on the values fields in the alist,
which maps symbols (called fields) to specific values.
The fields `year`, `month`, `day`, `hours`, `minute`, `second`,
`nanosecond`, and `timezone` are required.
The fields `nanoseconds` and `fold` are optional, and default to 0.
An error satisfying `date-error?` is signaled if any other fields are present.

`(instant->date `*timezone instant*`)`

Returns a date object in *timezone* that is equivalent to
*instant*. 

`(timespec->date `*timezone timespec*`)`

Returns a date object referring to *timespec* modified by *timezone*.

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
All ranges are inclusive at both ends, 

`instant`: The instant of this date.

`timespec`: The timespec of this date.

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
a standard week object has the fields
`iso-week-years` and `iso-week-numbers`
Missing fields are interpreted as 0.
An error satisfying `date-error?` is signaled if any other fields are present.

`(instants->duration `*earlier later*`)`

Returns a duration object representing the elapsed time between
instants *earlier* (inclusive) and *later* (exclusive).

`(timespecs->duration `*timezone timespec*`)`

Returns a duration object representing the elapsed time between
durations *earlier* (inclusive) and *later* (exclusive).

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

### Comparators

`date-comparator`

A comparator suitable for ordering date objects by their underlying instants.

`duration-comparator`

A comparator suitable for ordering duration object by their durations.

## Exceptions

`(date-error? `*obj*`)`

Returns `#t` if *obj* was signaled by one of the above procedures, or
`#f` otherwise.

## Implementation notes

The sample implementation provides
only an approximation of the mapping between TAI
and UTC.  The two time scales are assumed to be synchronized at 00:00:00
on January 1, 1958 and at all times before that.  From 1958 through 1971,
the relationship is complex, but since 1971 the two scales have been kept
within 0.9 seconds of each other by inserting leap seconds as needed.

For the messy period, the implementation pretends that there were leap seconds
at the end of the following days (that is, at 23:59:60 proleptic UTC time):
30 Jun 1959; 30 Jun 1961; 31 Dec 1963; 31 Dec 1964; 30 Jun 1966;
30 Jun 1967; 30 Jun 1968; 30 Jun 1969; 30 Jun 1970.
(Thanks to Daphne Preston-Kendal for determining an optimal leap second set.)
This has the following desirable effects: the TAI-UTC offset is 0 in 1958
(true by definition), at the Posix epoch it is 8
(which is within a few milliseconds of the true value),
and it is 10 at the start of 1972 when UTC and its leap second regime
begin.  Not having a leap second at the end of 1969 ensures that there is none
just before the Posix epoch.  The implementation also pretends,
*faute de mieux*, that there will be no more leap seconds in the future.

To update the leap second tables, download
[`leap-seconds.list`](https://www.ietf.org/timezones/data/leap-seconds.list)
for IANA's version of such a table, which is maintained.
For exact leap second data before 1972, see the old USNO file
[`tai-utc.dat`](http://web.archive.org/web/20191022082231/http://maia.usno.navy.mil/ser7/tai-utc.dat).
This file is *not* being updated, and should be used only if the
implementation wants to make exact conversions for the 1961-72 period.

The following table describes the arbitrary 1958-71 times and offsets
described above, using the same format as `leap-seconds.list`.

```
-378648000 0 01 Jan 1958
-283953600 1 01 Jan 1961
-252417600 2 01 Jan 1962
-205286400 3 01 Jul 1963
-157723200 4 01 Jan 1965
-110592000 5 01 Jul 1966
 -79056000 6 01 Jul 1967
 -47433600 7 01 Jul 1968
 -15897600 8 01 Jul 1969
  15638400 9 01 Jul 1970
```
