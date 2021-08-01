 ## Locale facts
 
 * the names of the months and the days of the week in a specified language,
 * the proper ordering of day, month, and year,
 * the names and starting dates of the Japanese eras or any others,
 * whether local clocks are 12-hour or 24-hour,
 * how to spell "AM" and "PM"
 * what the decimal separator is

## Combinators

* date (mm/dd/yy) [date *order-symbol date-postfix month-postfix day-postfix]
* year [year]
* locale's abbreviated month name (Jan...Dec) [short-month]
* locale's full month name (January...December) [long-month]
* month, zero padded (01...12) [padded-month]
* locale's abbreviated weekday name (Sun...Sat) [short-day-of-week]
* locale's full weekday name (Sunday...Saturday) [long-day-of-week]
* locale's abbreviated weekday name (Sun...Sat) [short-day-of-week]
* locale's full weekday name (Sunday...Saturday) [long-day-of-week]
* day of week (0...6) [day-of-week]
* day of month, zero padded (01...31) [padded-day-of-month]
* day of month, blank padded ( 1...31) [unpadded-day-of-month]
* day of year, zero padded [long-day-of-year] 
* hour, zero padded, 24-hour clock (00...23) [padded-hour-24]
* hour, zero padded, 12-hour clock (01...12) [paddd-hour-12]
* hour, blank padded, 24-hour clock ( 0...23) [unpadded-hour-24]
* hour, blank padded, 12-hour clock ( 1...12) [unpadded-hour-12]
* minute, zero padded (00...59) [padded-minute]
* second, zero padded (00...60) [padded-second]
* nanosecond, zero padded [nanoseconds]
* seconds+fractional seconds, using locale's decimal separator (e.g. 5.2) [fractional-seconds]
* locale's AM or PM [am-pm-indicator]
* number of full seconds since "the epoch" (in UTC) [utc-second]
* ISO 8601 week number of the year with Monday as first day of week (01..53) [iso-week-of-year]
* time zone [time-zone]

```
? last two digits of year (00...99)
? locale's date and time (e.g., "Fri Jul 14 20:28:42-0400 2000")
? ISO-8601 year-month-day format
? ISO-8601 hour-minute-second-timezone formatISO-8601 hour-minute-second format
? ISO-8601 year-month-day-hour-minute-second-timezone format
? ISO-8601 year-month-day-hour-minute-second format
```
[*] *order-symbol* can be any of `ymd`, `dmy`, `mdy`, perhaps others.
