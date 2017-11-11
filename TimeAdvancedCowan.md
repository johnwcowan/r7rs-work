## Date and time arithmetic

This is a WG2 proposal for date and time arithmetic, loosely based on Java's [Joda Time](http://joda-time.sourceforge.net) functions.  It's possible to implement parts of SRFI 19 on top of it, but it provides much more flexibility.  See also [TimePeriodsCowan](TimePeriodsCowan.md).

## Issues

1. Need to do something about ISO 8601 dates like "194902", which don't correspond to a single instant.  Extend dates to their first instant (also last instant?).

2. What about subsecond timing?  Specify milli, micro, nano, picoseconds?

3. Simple conversion between ISO 8601 date strings and date objects?


## Instants

For the purposes of this proposal, an *instant* is an exact integer representing a particular second of the TAI scale, such that 0 represents midnight on January 1, 1970 TAI (equivalent to ten seconds before midnight Universal Time) and the value 1 represents one TAI second later.  The current instant can be obtained more or less accurately by evaluating `(round (current-second))`.


## Chronologies

A *chronology* is an immutable member of a disjoint type that describes a particular calendar, such as the ISO, Gregorian, Julian, TAI, Jewish, Islamic, Persian, French Revolutionary, Maya, Chinese, Buddhist, Thai, Coptic, Ethiopic, or Martian calendar.  Implementations MUST support the ISO chronology, and MAY support any of the other calendars mentioned here, or indeed any calendar not mentioned here.  (The ISO chronology is similar to the Gregorian chronology: see the "ISO date fields" section for detailed differences.)

Chronologies also incorporate the concept of time zone.  For the ISO chronology (and the Gregorian and Julian chronologies if they are supported), an implementation MUST support the UTC (Universal Coordinated Time) time zone and any time zones expressible as a fixed offset in minutes from UTC.  It SHOULD support the historical time zones of the [IETF time zone database](http://en.wikipedia.org/wiki/Tz_database).

## Chronology procedures

`(current-chronology)`

A parameter representing a chronology used by default.  Its initial value should normally represent the user's preferred calendar and current time zone.  It can be rebound with `parameterize`.

`(chronology `*name*`)`

Returns the chronology named by the symbol *name*.  The ISO chronology is named `iso`.  If provided, the Gregorian chronology is named `gregorian`, the Julian chronology is named `julian`, and the TAI chronology is named `tai`.  These chronologies are in the UTC time zone.  All these chronologies are proleptic (extend into the past before the date they were adopted).  The names and behaviors of other chronologies are implementation-dependent.  If the name is not known to the implementation, an error that satisfies `date-error?` is signaled.

`(chronology-names)`

Returns a list of the symbols naming the chronologies known to the implementation.  It is an error to modify this list.

`(chronology-name `*chronology*`)`

Returns the symbol that names *chronology*.

`(chronology-fields `*chronology*`)`

Returns a list of the symbols naming the possible fields in date objects backed by this chronology (see below).  It is an error to modify this list.

`(chronology? `*obj*`)`

Returns `#t` if *obj* is a chronology and `#f` otherwise.

`(derived-chronology `*name* [#|*chronology* ]] *timezone*`)`

Returns a chronology named *name* (a symbol).  The returned value is based on *chronology*, by default the value of `(current-chronology)`, but using the time zone specified by *timezone*.    In the ISO and Gregorian chronologies, if *timezone* is an integer, it represents the number of minutes ahead of UTC, but if the implementation supports the `tz` database, and *time-zone* is a string containing a time zone name defined by that database, it represents the time zone with that name.  Otherwise, the interpretation of *timezone* is chronology- and implementation-dependent.

`(chronology-time-zone `*chronology*`)`

Returns the time-zone value associated with this chronology.

`(compound-chronology `*name initial-chronology* (*date chronology* )`)`

Returns a chronology named *name* (a symbol) that uses *initial-chronology1* at all times before the first *date* (a date object, see below), and each *chronology* for all times including and following the corresponding *date* and up to but not including the next *date*, if any.  This is useful for constructing chronologies that transition from Julian to Gregorian or from one timezone to another at a specified date.  Compound chronologies may also be constructed by the application.  The fields available in a compound chronology are the union of those available in the underlying chronologies, but not all fields are necessarily available for all instants.

## Date object procedures

A *date object* is an immutable member of a disjoint type that provides information about a specific interval of time with respect to a certain chronology.  Chronologies are independent of spatial coordinates: no account is taken of relativistic time dilation.  For example, at least with respect to the ISO, Gregorian, or Julian chronologies, a date object may represent a specific year, a specific week of a specific year, or an instant in time precise to a second.  Date objects have multiple numeric-valued fields such as `year` or `minute-of-day`, whose meanings and possible values are determined by the chronology.

## Date procedures

`(make-date `[#|*chronology* ]]` `*alist*`)`

Returns a date object using *chronology*, defaulting to the value of `(current-chronology)`.  *Alist* is an association list that maps symbols which are field names meaningful to *chronology* to associated numeric values.  An error that satisfies `date-error?` is signaled if the field values are unknown to the chronology, insufficient to specify a particular date object (for example, a month without a year) or mutually inconsistent.

`(date? `*obj*`)`

Returns `#t` if *obj* is a date object, and `#f` otherwise.

`(date->alist `*date*`)`

Returns a newly allocated alist containing the fields of *date*.  Implementations SHOULD provide computed fields as well as explicitly set ones.

`(date-field `*date*` `*fieldname*`)`

Returns the numeric value of the field named *fieldname* (a symbol) within *date*, or `#f` if there is no such field.  If the specified field was not provided when *date* was constructed, the value is computed and returned.

`(convert-date `[#|*chronology* ]]` `*date*`)`

Equivalent to `(make-date `[#|*chronology* ]]` (date->alist `*date*`))`, but potentially much more efficient.

`(date-update `*date*` `*fieldname*` `*value*`)`

Returns a date object based on *date*, but with the field named *fieldname* updated to *value*.  An error that satisfies `date-error?` is signaled if the field is unknown or the value is out of range.

`(date-increment `*date*` `*fieldname*` `*increment*`)`

Returns a date object which is later than *date* by *increment* measured in the units specified by *fieldname*, or earlier if *increment* is negative.   An error that satisfies`date-error?` is signaled if *fieldname* is unknown.

For example, `(date-increment `*date*` 'day-of-month 7)` adds seven days to *date*
(which is typically the same as one week, but may be different in some chronologies).

`(date-chronology `*date*`)`

Returns the chronology associated with *date*.

`(date-field-maximum `*date*` `*fieldname*`)`

`(date-field-minimum `*date*` `*fieldname*`)`

Returns the maximum or minimum legal value of the field named *fieldname* in the chronology associated with *date*.  This value is not necessarily the same for all date objects in a particular chronology; for example, 28 is the maximum value of `day-of-month` if `month` has the value 2 (February) and `year` is not a leap year.  Returns `#f` if the value cannot be determined (there is no maximum or minimum year in the ISO chronology, for example).

`(date-round `*date*` `*fieldname*`)`

`(date-ceiling `*date*` `*fieldname*`)`

`(date-floor `*date*` `*fieldname*`)`

`(date-truncate `*date*` `*fieldname*`)`

Returns a date object which is the same as *date*, but adjusted to the nearest integral value of *fieldname* using the `round`, `ceiling`, `floor`, or `truncate` functions.  This may cause other fields to change their values as well.


## ISO date fields

The values of these fields MUST be exact integers.  These fields are specified for the ISO, Gregorian, and Julian chronologies, but they may be relevant to other chronologies as well; it is easiest to use a chronology if it supports the standard fields.  For interoperability, every chronology must support the `instant` field.  The field names come from Joda Time, and ultimately from ISO 8601.

`century`::
> The absolute century of the date.  In the ISO chronology, the century of 1965 C.E. is 19, the century of 70 C.E. is 0, and the century of 43 B.C.E. is -1.  In the Gregorian and Julian chronologies, they are 20, 1, and -1 respectively, and there is no century 0.

`century-of-era`::
> The century of the date's era.

`clock-hour-of-day`::
> The hour of the date's day in the range 0-23.

`clock-hour-of-half-day`::
> The hour of the date's half-day, in the range 1-12.

`day-of-month`::
> The day of the date's month, in the range 1-31 (or less in some months).

`day-of-week`::
> The day of the date's week.  In the ISO chronology, Monday is day 1 and Sunday is day 7.  In the Julian and Gregorian chronologies, Sunday is day 1 and Saturday is day 7.

`day-of-year`::
> The day of the date's year, in the range 1-365 in non-leap years and 1-366 in leap years.

`daylight-saving-time`::
1 if daylight saving time (summer time) was in effect, or 0 if not.  This field discriminates between 2:00 A.M. daylight time and 2:00 A.M. standard time on the day when daylight saving time ends in the U.S. (and the corresponding periods for other daylight saving time regimes).

`era`::
> The date's era.  The ISO chronology has just one era.  In the Gregorian and Julian chronologies, C.E. (or A.D.) = 1, and B.C.E. (or B.C.) = 0.  By convention, the era containing instance 0 is era 1.

`half-day-of-day`::
> The half-day (A.M. or P.M.) of this day.  A.M. = 0, P.M. = 1.

`hour-of-day`::
> The hour of the date's day, in the range 0-23.

`hour-of-half-day`::
> The hour of the date's half-day, in the range 0-11.

`instant`::
> The instant corresponding to this date.

`julian-day`::
> The integral difference in days between this date and noon Universal Time, January 1, 4173 B.C.E. Julian (November 24, 4714 B.C.E. Gregorian).

`local-time-offset`::
> The local time zone offset (standard or daylight saving, as the case may be) in minutes ahead of UTC.

`minute-of-day`::
> The minute of the date's day, in the range 0-1439.

`minute-of-hour`::
> The minute of the date's hour, in the range 0-59.

`modified-julian-day`::
> The Julian day minus 2400000.5.

`month-of-year`::
> The month of the date's year, in the range 1-12.

`second-of-day`::
> The second of the date's day, in the range 0-86399.

`second-of-minute`::
> The second of the date's minute, in the range 0-60 (for leap seconds).

`standard-time-offset`::
> The standard time zone offset in minutes ahead of UTC.

`week-of-week-year`::
> The week of the date's week-year, in the range 1-53.

`week-year`::
> The date's week-year, which in the ISO chronology begins on the week that has at least 4 days in the chronological year.

`week-year-of-century`::
> The week-year of the date's century.  The range is the same as for `year-of-century`.

`year`::
> The absolute year of the date.  In the ISO chronology, 43 B.C.E. is -42; in the Gregorian and Julian chronologies, it is -43.

`year-of-century`::
> The year of the date's century.  In the ISO chronology, the range is 0-99.  In the Gregorian and Julian chronologies, the range is 1-100.

`year-of-era`::
> The year of the date's era.  In the Gregorian and Julian chronologies, the value cannot be 0.

## Comparators

`date-comparator`

A comparator suitable for ordering date objects by their underlying instants.

## Exceptions

`(date-error `*obj*`)`

Returns `#t` if *obj* is an object signaled by an error as specified above, and `#f` otherwise.
