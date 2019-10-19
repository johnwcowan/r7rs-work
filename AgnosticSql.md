This is an idea for representing SQL statements as S-expressions without knowing much about SQL.
A single procedure accepts an S-expression (often constructed by quasiquote) and returns a corresponding
string of SQL.  Because of the conversion process, no parameter markers are required in the SQL.
The S-expressions aren't the prettiest things in the world, but I think they are reasonably legible.

  * A Scheme number, boolean, or bytevector is converted to a SQL number, boolean, or bit literal.
  
  * A Scheme string is output surrounded by single quotes.  If it contains a single quote,
    that character is doubled.

  * A Scheme symbol represents either a SQL keyword or an identifier.
    If it contains hyphens, the individual parts are processed separately
    and output with spaces between them.
    If it contains periods, the individual parts are processed separately
    and output with dots between them.
    
  * A symbol or symbol part that contains only lower-case or caseless letters (not just ASCII), digits,
    and underscore is output as is.  If it contains any other characters, it is output in double quotes.
    If it contains a double quote, that character is doubled.

  * A list typically outputs as a comma -separated string of its elements surrounded by parentheses.
    A top-level list is treated as a vector.
    
  * A list whose car is a Lisp keyword (or, if there are no keywords, begins or ends with a colon)
    represents an infix operator.  The elements of the lists have the operator (with no escaping)
    inserted between them,
    and they are wrapped in parentheses to override the operator priority.
    In addition to the usual symbolic operators like `:+` and `:=`,
    there are also infix operators like `:union-all`.
    
  * A vector typically outputs as a space-separated string of its contents.
  
  * However, a vector whose sole element is a list is output as a comma-separated string of its contents
    without parentheses.
    
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
  (#(itemid varchar-not-null))
   #(modified_date varchar-not-null))
   #(due_date varchar-not-null))
   #(text clob not-null))
     (primary-key (itemid))))
     
(select #((foo.a bar.a))
 from #(foo bar)
 where (:= foo.b bar.b))
```

    