 ## Specification
 
 `(format-date `*port-or-boolean date combinator* ...`)`
 
 Formats a date object and outputs it to *port-or-boolean*
 based on the *combinator* objects.  If *port-or-boolean*
 is `#t`, the port is `(current-output)`; if it is `#f`,
 the result is a string that `format-date` returns; otherwise
 it returns an unspecified value.  The date object may be from
 [SRFI 19](https://srfi.schemers.org/srfi-19/srfi-19.html) or
 from [TimeAdvancedCowan](https://github.com/johnwcowan/r7rs-work/blob/master/TimeAdvancedCowan.md).
 
 If *combinator* is a number, character, or string, it is
 output as if by `display`.  If it is a symbol, see the
 Combinators section.  If it is a list of symbols, the
 first symbol is from the Combinators section and the
 remaining symbols are from the Modifiers section.
 
 ### Combinators

Specify one of the following symbols to generate
an output based on the date.

`am-pm`  
AM or PM for 12-hour clock

`century`  
high-order digits of year

`day-of-month`  
day of the month

`day-of-year`  
day of the year

`hour-12`  
hour on 12-hour clock

`hour-24`  
hour on 24-hour clock

`fold`  
time fold (0 = unfolded, 1 = folded)

`localized-date`  
localized year, month, and day

`localized-datetime`  
localized date and time

`localized-time`  
localized hour, minute, and second

`minute`  
minute of the hour

`month`  
numeric month

`month-name-long`  
localized name of month

`month-name-short`  
localized abbreviation for month

`nano`  
nanosecond of the second

`second`  
numeric second

`weekday-long`  
localized abbreviation for weekday

`weekday-mon`  
weekday (0 = Monday, 6 = Sunday)

`weekday-short`  
localized name of weekday

`weekday-sun`  
weekday (0 = Sunday, 6 = Saturday)

`week-iso`  
ISO week (first week containing 4 January)

`year-iso`  
year starting on ISO week 01

`year-long`  
numeric year

`year-short`  
2 low-order digits of year

`zone`  
standard time zone name

`zone-name`  
localized named time zone

`zone-offset`  
time zone offset from UTC (minutes)

### Modifiers

To specify the following, create a list whose first element is a combinator
and whose remaining elements are modifiers.

*number*
field width  

`none`
no padding  

`space`
pad with spaces  

`zero`
pad with 0s  

`alt`
localized alternative output  

`lcase`
lowercase output  

`ucase`
uppercase output  
