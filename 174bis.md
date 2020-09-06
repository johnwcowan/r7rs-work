## Constants

`time-duration`  
Symbol representing Time duration.

`time-monotonic`  
Symbol representing monotonic time.

`time-process`  
Symbol representing time spent in current process.

`time-tai`  
Symbol representing TAI time.

`time-thread`  
Symbol representing time spent in current thread.

`time-utc`  
Symbol representing UTC time.


## Time object and accessors

`make-time` *type nanosecond second -> time*  
Creates a time object.

`time?` *object -> boolean*  
#t if object is a time object, otherwise, #f.

`time-type` *time -> time-type*  
Time type.

`time-nanosecond` *time -> integer*  
Time nanosecond.

`time-second` *time -> integer*  
Time second.

## Time comparison procedures
All of the time comparison procedures require the time objects to be of the same type. It is an error to use these procedures on time objects of different types. For the point-in-time measurements (e.g., TIME-TAI and TIME-UTC), the semantics are described in plain text. For durations, (e.g., TIME-DURATION, TIME-CPU, the semantics are described in parentheses.


`time<=?` *time1 time2 -> boolean*  
#t if time1 is before or at (less than or equal to) time2, #f otherwise.

`time<?` *time1 time2 -> boolean*  
#t if time1 is before (less than) time2, #f otherwise.

`time=?` *time1 time2 -> boolean*  
#t if time1 at (equal) time2, #f otherwise.

`time>=?` *time1 time2 -> boolean*  
#t if time1 is at or after (greater than or equal to) time2, #f otherwise.

`time>?` *time1 time2 -> boolean*  
#t if time1 is after (greater than) time2, #f otherwise.


##Time arithmetic procedures

`time-difference` *time1 time2 -> time-duration*  
The TIME-DURATION between time1 and time2. It is an error if time1 and time2 are of different time types. A new time object is created.

`add-duration` *time1 time-duration -> time*  
The time resulting from adding time-duration to time1, which is a time object of the same time type as time1. A new time object is created.

`subtract-duration` *time1 time-duration -> time*  
The time resulting from subtracting time-duration to time1, which is a time object of the same time type as time1. A new time object is created.

## Conversion

`time->inexact` *time*  
Number of seconds since the epoch as an inexact real number.

`inexact->time` *type inexact*  
A time with given type with specified number of seconds from the epoch.

## Hash procedure

`time-hash` *time -> exact integer*  
Hash value.
