This is an idea for representing SQL statements as S-expressions
without the conversion procedure knowing much about SQL.
The procedure accepts an S-expression (often constructed by quasiquote) and returns a corresponding
string of SQL.  Because of the conversion process, no parameter markers are required in the SQL.
The S-expressions aren't the prettiest things in the world, but I think they are reasonably legible.
The user, or the code that creates the S-expressions, still has to know SQL, of course.

Here are the representation convention.

  * A Scheme number, boolean, or bytevector is converted to a SQL number, boolean, or bit literal.
  
  * A Scheme string is output surrounded by single quotes.  If it contains a single quote,
    that character is doubled.

  * A Scheme symbol represents either a SQL keyword or an identifier.
    If it contains hyphens, the individual parts are
    output with spaces between them as if by `display`;
    they are assumed to be a sequence of keywords.
    If it contains periods, the individual parts are processed separately
    and output as separate symbols with dots between them.
    
  * A symbol that contains only lower-case or caseless letters (not just ASCII), digits,
    and underscore is output as is.  If it contains any other characters, it is output in double quotes.
    If it contains a double quote, that character is doubled.
    To get characters that are not allowed in symbols to be output as SQL identifiers,
    use vertical bars, backslash escapes, or both, according to the Scheme's implementation.

  * A list typically outputs as a comma -separated string of its elements surrounded by parentheses.
    A top-level list is treated as a vector.
    
  * A list whose car is a Lisp keyword (or, if there are no keywords, begins or ends with a colon)
    represents an infix operator.
    The elements of the lists have the operator (with no escaping) inserted between them,
    and they are wrapped in parentheses to override the operator priority.
    In addition to the usual symbolic operators like `:+` and `:=` you can use `:like`
    in the same way.  But operators like `UNION` are not usable, because their operands
    are whole `SELECT` statements that can't be wrapped in parentheses.
    
  * A vector with two or more elements outputs as a space-separated string of its contents.
  
  * A vector whose sole element is a list is output as a comma-separated string of its contents
    without parentheses.
    
  * A vector whose sole element is a string causes the string to be output as if by `display`.
    This is the ultimate escape for handling unfortunate things like escaped SQL identifiers that are
    all in lower case, because SQL transforms unquoted lower case identifiers to upper case.
    
Here are examples of a CREATE TABLE and SELECT statement
and one of several possible S-expression equivalents for each.  

```
CREATE TABLE IF NOT EXISTS items(
  itemid VARCHAR NOT NULL,
  modified_date VARCHAR NOT NULL,
  due_date VARCHAR NOT NULL,
  other_date VARCHAR NOT NULL,
  text CLOB NOT NULL,
    PRIMARY KEY (itemid));
    
SELECT foo.a, bar.a
  FROM foo, bar
  WHERE foo.b = bar.b;

(create-table-if-not-exists items
  (#(itemid varchar-not-null)
   #(modified_date varchar-not-null)
   #(due_date varchar-not-null)
   #(text clob not-null)
     (primary-key (itemid))))
     
(select #((foo.a bar.a))
 from #(foo bar)
 where (:= foo.b bar.b))
```

    