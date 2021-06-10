## Abstract

These procedures, except for those in the section **Conversions**,
have the same semantics as the
[SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html)
procedures with the same names.
Their SRFI 174 equivalents are shown.

## Constants

No SRFI 174 equivalents, because SRFI 174 provides only timespecs
of type `time-utc`.

`time-utc`  
Symbol representing UTC time.

`time-tai`  
Symbol representing TAI time.

`time-monotonic`  
Symbol representing monotonic time.

`time-process`  
Symbol representing time spent in current process.

`time-thread`  
Symbol representing time spent in current thread.

`time-duration`  
Symbol representing a duration.

## Time object and accessors

`make-time` *type nanosecond second -> time*

Creates a time object.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec`]

`time?` *object -> boolean*

#t if object is a time object, otherwise, #f.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec?`]

`time-type` *time -> time-type*

Time type.
[No SRFI 174 equivalent]

`time-nanosecond` *time -> integer*

Time nanosecond.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec-nanoseconds`]

`time-second` *time -> integer*

Time second.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec-seconds`]

## Time comparison procedures

All of the time comparison procedures require the time objects to be of the same type.
It is an error to use these procedures on time objects of different types.

The semantics for time objects of type `time-duration` are given in parentheses.

`time=?` *time1 time2 -> boolean*

#t if time1 is equal to time2, #f otherwise.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec=?`]

`time<?` *time1 time2 -> boolean*

#t if time1 is before (less than) time2, #f otherwise.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec<?`]

`time>?` *time1 time2 -> boolean*

#t if time1 is after (greater than) time2, #f otherwise.
[No SRFI 174 equivalent]

`time<=?` *time1 time2 -> boolean*

#t if time1 is before or at (less than or equal to) time2, #f otherwise.
[No SRFI 174 equivalent]

`time>=?` *time1 time2 -> boolean*

#t if time1 is at or after (greater than or equal to) time2, #f otherwise.
[No SRFI 174 equivalent]


## Time arithmetic procedures

`time-difference` *time1 time2 -> time-duration*

Returns a time object of type `time-duration` representing the time between
between *time1* and *time2*.
It is an error if time1 and time2 are of different time types. A new time object is created.
[No SRFI 174 equivalent]

`add-duration` *time1 time-duration -> time*

Returns the time resulting from adding *time-duratio*n to *time*.
The result has the same type as *time*.
[No SRFI 174 equivalent]

`subtract-duration` *time1 time-duration -> time*

Returns the time resulting from subtracting *time-duratio*n from *time*.
The result has the same type as *time*.
[No SRFI 174 equivalent]

## Conversion

`time->inexact` *time*

Number of seconds since the epoch as an inexact real number.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec->inexact`]

`inexact->time` *type inexact*

A time with given type with specified number of seconds from the epoch.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `ineact->timespec`]

`time-utc->tai` *time* [*leap-second*]

Returns a time object of type `time-utc` to an equivalent time object
of type `time-tai`.  If the `time-utc` object is equivalent
to two different `time-tai` objects, one of which is a leap second and
the other of which is not, the boolean argument *leap-second* shows
which TAI second is returned.
[No SRFI 174 equivalent]

`time-tai->utc` *time*

Converts a time object of type `time-tai` to an equivalent time object
of type `time-utc`.
[No SRFI 174 equivalent]

## Hash and comparator

`time-hash` *time*

Returns an exact integer hash code for *time*.
[[SRFI 174](https://srfi.schemergs.org/srfi-174/srfi-174.html) `timespec-hash`]

`time-comparator`  

A SRFI 128 comparator for two times of the same type
based on `time?, time=?, time<', time-hash`.
[No SRFI 174 equivalent]

## Implementation

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
