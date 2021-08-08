 ## Locale facts
 
 * the names of the months and the days of the week in a specified language,
 * the proper ordering of day, month, and year,
 * the names and starting dates of the Japanese eras or any others,
 * whether local clocks are 12-hour or 24-hour,
 * how to spell "AM" and "PM"
 * what the decimal separator is

## Combinators

* date [date *order-symbol year-combinator year-postfix month-combinator month-postfix day-combinator day-postfix*]
* time [time hour-combinator hour-postfix minute-combinator minute-postfix second-combinator second-postfix nanosecond-combinator nanosecond-postfix]

### Year combinators

* year [year]
* Roman year [roman-year]

### Month combinators

* locale's abbreviated month name (Jan...Dec) [short-month]
* locale's full month name (January...December) [long-month]
* month, zero padded (01...12) [padded-month]
* month, unpadded [1..12] [unpadded-month]
* Roman month [roman-month]

### Week combinators

* ISO 8601 week number of the year with Monday as first day of week (01..53) [iso-week-of-year]

### Day combinators

* day of month, zero padded (01...31) [padded-day-of-month]
* day of month, blank padded ( 1...31) [unpadded-day-of-month]
* locale's abbreviated weekday name (Sun...Sat) [short-day-of-week]
* locale's full weekday name (Sunday...Saturday) [long-day-of-week]
* day of week (0...6) [day-of-week]
* day of year, zero padded [long-day-of-year] 

### Hour combinators

* hour, zero padded, 24-hour clock (00...23) [padded-hour-24]
* hour, zero padded, 12-hour clock (01...12) [padded-hour-12]
* hour, unpadded, 24-hour clock ( 0...23) [unpadded-hour-24]
* hour, unpadded, 12-hour clock ( 1...12) [unpadded-hour-12]

### Minute combinators

* minute, zero padded (00...59) [padded-minute]

### Second combinators

* second, zero padded (00...60) [padded-second]
* second, unpadded (0..60) [unpadded-second]
* nanosecond, zero padded [nanoseconds]
* seconds+fractional seconds, using locale's decimal separator (e.g. 5.2) [fractional-seconds]
* locale's AM or PM [am-pm-indicator]
* number of full seconds since "the epoch" (in UTC) [utc-second]
* time zone [time-zone]
* time zone offset [time-zone-offset]

[*] *order-symbol* can be any of `ymd`, `dmy`, `mdy`, perhaps others.
