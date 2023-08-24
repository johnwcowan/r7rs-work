## Date and duration operations

This is a SRFI for date and duration operations.
All the objects discussed here are immutable.
This SRFI supports only the
[proleptic Gregorian calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar).

## Instants and time objects

Scheme uses two internal formats for absolute timestamps, which
in this SRFI are called *instants* and *time objects*.

An *instant* is an inexact number representing the
approximate number of 
seconds since December 31, 1969 in the 
proleptic Gregorian calendar and 23:59:52 (eight seconds before midnight)
in UTC time.

A time object belongs to a disjoint type (see
FIXME: [TimeObjects](https://github.com/pre-srfi/time-objects/blob/master/TimeObjects.md))
that represents a count of whole seconds and nanoseconds
plus a time type such as UTC, TAI, or duration.
The current UTC time object can be obtained by calling the SRFI 170
procedure `posix-time`.

FIXME: SRFI TimeObjects has procedures for converting between UTC and TAI time objects
and between either of them and instants.  Consequently, the procedures
of this SRFI only accept UTC and duration time objects.

In this SRFI, a UTC time object during a leap second is always assumed to be the same
(in the sense of `=`) for both the seconds and the nanoseconds) as
the time object for the following second, but the results of actually calling
`posix-time` in the vicinity of a leap second do not necessarily agree.


## Date objects

A *date object* is an immutable member of a disjoint type
that provides information about a date, a time, or both at once;
they are based on a local time scale.

Date objects have multiple mostly numeric-valued fields
that can be extracted from them.
They are listed below.

The date objects of this SRFI are similar to
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html)
date objects, except that a SRFI 19 date object supports
numeric timezones only and does not support time folds correctly.

## Duration objects

A *duration object* represents the amount of elapsed time
between an earlier instance or time object and a later one
that has been broken into smaller intervals of time.

Duration objects have multiple mostly numeric-valued fields
that can be extracted from them.
They are listed below.

## Time zones

Local civil time is everywhere specified using
an offset from Universal Time.  (For a few years this was not
true in Saudi Arabia, but this SRFI ignores the Saudi government's
attempt to rearrange local civil time so that hour 0 began at astronomical sunset.)

In each political jurisdiction the rules for changing the offset vary,
both the annual cycle of standard vs. daylight saving time (if in effect)
and any unpredictable changes in the offset applied by political entities.
This SRFI therefore requires a *time zone* to be supplied in order
to convert a UTC time object to local time.  It requires support for numeric timezones,
which are Universal Time minus local time in seconds, and strongly recommends
support for named time zones as defined by the
[IANA time zone database](https://www.iana.org/time-zones); these are strings.

When local time jumps backwards (typically some time in the autumn in the temperate zones
or else for political reasons at any time),
the same local time can represent two different UTC values.
Such a situation is called a *time fold* and is represented as 0 for the earlier time
and 1 for the later time.  For example, on the day when daylight saving time ends in the U.S.,
the fold is 0 at 2:00 A.M. daylight time
and 1 at 2:00 A.M. standard time one hour later.
The fold for any unaffected time is always 0.  The idea behind the name is that
the UTC scale is folded up, as it were, with some parts of it representing the same local times.

The current timezone can generally be obtained from the `TZ` environment variable.
Note that there is no concept in this SRFI of
a date or time without a timezone; some timezone must be supplied whenever a
date object is created.

## Localization

This SRFI does not deal with localization beyond the matter of time zones.
It does not know:
 * the names of the months or of the days of the week in any language,
 * the proper ordering of day, month, and year,
 * the names and starting dates of the Japanese eras or any others,
 * whether local clocks are 12-hour or 24-hour,
 * how to spell "AM" and "PM",

It also does not know anything about non-Gregorian calendars,
including those proposed for other celestial bodies than the Earth.
Sufficient to the day is the evil thereof.  However, it does provide
a method for specifying them in date objects.

Note that names like "EST" are actually not timezone names but names of offsets,
and are not globally unique even in a single language:
this SRFI does not deal with them either.

## Procedures

### Date object constructor

`(date `*objs*`)`  

Returns a date object based on the *objs*,
which alternate between symbols (called field names) and specific values.
The complete list of field names is `calendar`, `era`, `year`, `month`,
`day-of-month`, `iso-week-year`, `iso-week`, `day-of-week`,
`hour`, `minute`, `second, `nanosecond`, `timezone`, and
`time-fold'.  These are explained below in the date accessors section.

 * `calendar` is a symbol representing the calendar in use.  The only
   required calendar is `iso`, corresponding to the Gregorian calendar
   except that the year before the year 1 C.E. is numbered 0.
 * `era` is an exact integer representing the era.  In the ISO calendar,
   only era 0 exists.  In the Gregorian calendar, the B.C.E. era is numbered
   -1 and the C.E. era is numbered 0; a negative era number indicates that
   the era's year numbers decrease with increasing time.
 * `iso-week` represents a numbering of complete 7-day weeks from 1 to 52 or 53, where
   each week begins on Monday and the first week of the `iso-week-year`
   is the week in which January 4 falls.
 * `week` is 1 for Monday and 7 for Sunday.
 * `iso-date-string` is a string in the format specified by ISO 8601.

If one of the following groups of fields are present,
the date object is a *complete date object*,
and corresponds to a specific UTC time object.

 * The fields `year`, `month`, `day-of-month`, `hours`, `minutes`, and `seconds`.
 * The fields `iso-week-year`, `iso-week`, `day-of-week`, `hour`,
   `minute`, and `second`.
 * The field `iso-date-string`.

These combinations specify a date object that corresponds to a particular UTC
time object.  If the specified fields do not constitute any of these three possibilities,
an *incomplete date object* is returned, which does not correspond to a particular
UTC time object, but may be useful nonetheless.  For example, an incomplete date
object representing Gregorian Easter in 2022 has a year of 2022, a month of 4, and a
day-of-month of 17, but all other fields are `#f`.

The fields `calendar`, `era`, `nanosecond`, `timezone`, and `time-fold`
are always optional, and default to `iso`, 0, 0, `utc`, and 0.

### Date object predicate

`(date? `*obj*`)`

Returns `#t` if *obj* is a date object, and `#f` otherwise.

### Date object accessors

The following procedures accept one argument, which is a date object.
Unless otherwise noted, all
returned values are exact integers that have been rounded down if necessary
or else `#f .  All ranges are inclusive at both ends.

`date-calendar`: A symbol representing the calendar in which this date is
specified.  The only required calendar is `iso`, but calendars such as
`hebrew`, `thai-buddhist`, `indian` may also be supported.

`date-era` is an exact integer representing the era.  In the `iso` calendar,
only era 0 exists.  In the Gregorian calendar, the B.C.E. era is numbered
-1 and the C.E. era is numbered 0; a negative era number indicates that
the era's year numbers decrease with increasing time.

`date-local-time-offset`: The local time zone offset in effect at this
date at the specific timezone in seconds ahead of UTC.

`date-year`: The year.  Note that 1 BCE is represented as 0 and 2 BCE as -1.

`date-month`: The month, where 1 is January and 12 is December.

`date-day`: The day of the month.

`date-hour`: The hour, where 0 represents the time between
midnight and 1 AM and 23 represents the time between 11 PM
and midnight.  Midnight itself is both minute 0 of hour 0
of the following day and minute 0 of hour 24 of the preceding day.

`date-minute`:  The minute.

`date-second`: The second.

`date-nanosecond`: The nanosecond.

`date-timezone`: The timezone with which this date was created,
either a string or a number.

`date-day-of-week`: The day of the week, where Monday is 1 and Sunday is 7.

`date-days-in-month`: The number of days in the date's month: between 28 and 31.
Date folds can change the range.

`date-day-of-year`: The day of the year, typically between 1 and 365 in non-leap years
and 1 and 366 in leap years.  Date folds can change the range.

`date-days-in-year`: The number of days in this date's year, either 365 or 366.
Date folds can change the range.

`date-julian-day`: The whole number of days between this date and noon Universal Time, January 1, 4173 B.C.E. Julian
(which is November 24, 4714 B.C.E. Gregorian).  Leap seconds are ignored.

`date-modified-julian-day`: The whole number of days between this date and midnight Universal Time, November 17, 1858 Gregorian.

`date-iso-utc-string`: A string that conforms to the
format for UTC time in the [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339)
profile of ISO 8601.  Roughly speaking, this is of the form
`yyyy-mm-ddThh:mm:ss.dddZ`.
There may be any number of subsecond digits; if they are omitted,
so is the preceding decimal point.

`date-iso-week-year`: The number of the period which begins in week 1 and ends in week 52 or 53.
It is the same as `year` except for up to six days in January and December.

`date-iso-week-number`: The number of the week in this date's year, between 1 and either 52 or 53.
Week 1 is the first full week where Thursday ia in January.

`date-seconds-in-minute`: the number of seconds in the date's minute.

`date-seconds-in-day`: The total number of seconds in the date's day.

`date-time-fold`:  The time fold associated with this date (see "Time zones" below).`

### Other date object procedures

`(date->utc-date `*date*`)`

`(date-in-calendar `*date calendar*`)`

Returns a date object representing the same time object as *date*, but in calendar *calendar*.

Returns a date object in the UTC time zone representing the same time as *date*.

`(date-update `*date fieldname value*`)`

Returns a date object based on *date*,
but with the value of the field named *fieldname* replaced by to *value*.
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

### Duration constructor

`(duration `*alist*`)`  

Returns a duration object based on the *objs*,
which alternate between symbols (called fields) and specific values.
The following possibilities for combinations of fields may be used:

 * The fields `years`, `months`, `weeks`, `days`, `hours`, `minutes`, `seconds`, and `nanoseconds`
   are all optional, but at least one must be provided.
   Missing fields are interpreted as 0.
 * The field `iso-duration-string` is required and no other fields are allowed.
 * The field `time-object` is required.

An error satisfying `date-error?` is signaled if any other fields are present.

### Duration accessors

The following procedures
accept one argument, a duration object.
Unless otherwise noted, the result values are exact integers.

`duration-years`: The number of years in the duration.

`duration-months`: The number of months in the duration.

`duration-weeks`: The number of weeks in the duration.

`duration-days`: The number of days in the duration.

`duration-hours`: The number of hours in the duration.

`duration-minutes`: The number of minutes in the duration.

`duration-seconds`: The number of seconds in the duration.

`duration-nanoseconds`: The number of nanoseconds in the duration.

`duration-iso-string`: A string beginning with `P` followed by a letter
Y, M, W, D, H, M, S, or W and a number,
repeated for all avaiable fields with which the duration object was created.

`duration-time-object`: A time object of type `duration` equal in length
to the duration object.  It is an error if the `years` and `months`
fields are not both 0.

### Other duration procedures

`(duration-difference `*earlier later*`)`

Returns a duration object representing the elapsed time between
date objects *earlier* (inclusive) and *later* (exclusive).

`(duration? `*obj*`)`

Returns `#t` if *obj* is a duration object, and `#f` otherwise.

`(duration-adjust `*duration fieldname increment*`)`

Returns a duration object which is later than *duration* by *increment*
measured in the units specified by *fieldname*,
or earlier if *increment* is negative.
For example, `(duration-adjust `*duration*` 'days 7)`
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

A comparator suitable for ordering date objects by their underlying time objects.

`duration-comparator`

A comparator suitable for ordering duration objects by their durations.
Duration objects are equal iff their fields are equal.

## Exceptions

`(date-error? `*obj*`)`

Returns `#t` if *obj* was signaled by one of the above procedures, or
`#f` otherwise.

## Lexical syntax (optional)

A lexical syntax for date and duration objects may be provided by prefixing
`#@` to either an ISO date string or a
duration string (the syntaxes are disjoint).
