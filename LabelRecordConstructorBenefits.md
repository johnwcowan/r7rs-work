
# Label Record Constructor Benefits

## From SRFI-57

The ability to populate record values by labels provides a more robust and readable alternative, especially useful when a record has more than two or three fields, or if it inherits fields from a type scheme. Field labels are checked for validity and the macro may be compiled to a positional constructor at expansion time, thus eliminating a large class of potential programmer errors at no cost in efficiency.
