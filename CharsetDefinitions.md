# DRAFT, DO NOT USE, WILL BECOME REDIRECT

## Introduction

This page provides modern definitions of the standard
[SRFI 14](http://srfi.schemers.org/srfi-14/srfi-14.html)
character sets in terms of Unicode properties.
The definitions published as part of Java 1.0, which
means they reflect data and interpretations as of Unicode 2.0.
The Unicode version at the time of writing (2019-11-13) is 12.1,
so some corrections and expansions are in order.

Unicode publishes many properties of characters in the
Unicode database online.
It's now recommended that Scheme implementers
who wish to provide Unicode versions of SRFI 14
use these to write a program
generating a current list of Unicode characters that fit into
each of the standard sets.  The program can then provide the result in some way
that the implementation can use, either as S-expressions or as binary files
or in some other way. This program should be re-run at least every six
months as new versions of Unicode are published.  However,
it is very rare to unheard-of for any character to be
removed from any character set.  Of course, as new characters are added to Unicode
(no characters have been removed since Unicode 2.0),
the standard character sets grow over time.

The Unicode files we need are found on the [unicode.org](http://unicode.org) web site.
The [UnicodeData.txt](https://www.unicode.org/Public/UNIDATA/UnicodeData.txt) file
contains the General Category, a 2-letter code that groups all Unicode characters
into one of thirty classes.  For example, "Lu" means "upper case letter"
and "Sm" means "mathematical symbol".
The [PropList.txt](https://www.unicode.org/Public/UNIDATA/PropList.txt) file
gives various properties of either single Unicode characters or ranges of them.
For example, the property "Deprecated" applies to the characters whose existence
is the result of a mistake or whose use is strongly discouraged.  Characters
can and generally do, have more than one property.

## Parsing UnicodeData.txt

UnicodeData.txt is a very straightforward file with multiple fields separated by
`;` characters.  Field 1 is the Unicode codepoint in hex: four, five, or six
characters.  Field 2 is normally the official name of the character.  Field 3
is the General Category.

However, there is one special convention to make the file shorter.  If the content
of field 2 begins with `<` and ends with `, First>`, then it represents
the first codepoint in a range of characters that all have the same properties
and whose names are generated algorithmically.  All such lines are immediately
followed by another special line beginning with `<` and ending with `, Last>`,
which specifies the last codepoint of the range.

For example, the consecutive lines
```
4E00;<CJK Ideograph, First>;Lo;0;L;;;;;N;;;;;
9FEF;<CJK Ideograph, Last>;Lo;0;L;;;;;N;;;;;
```

mean that all characters from U+4E00 to U+9FEF inclusive belong to category Lo.

## Parsing PropList.txt

The format of PropList.txt is more complicated but more flexible.
Comments  beginning with `#` may appear on any line and go to the
end of the line; a line beginning with `#` is a comment
All such comments, as well as blank lines, should be completely ignored.
Spaces within lines should also be discarded.

After that, the file contains two fields separated by `;`.  Field 1 is either
a single hex codepoint or else two hex codepoints  separated by `..`
designating the first and last codepoints in the range.
Field 2 is Unicode's standard name for the property.

## Definitions of the sets

Each set is defined as the union of specified general categories, properties, other character sets, single codepoints,
and ranges of codepoints using the `..` notation of PropList.txt.

```
char-set:lower-case = category Ll + property Other_Lowercase

char-set:upper-case = category Lu + property Other_Uppercase

char-set:title-case = category Lt

char-set:letter = char-set:lower-case + char-set:upper-case +
                  char-set:title-case + category Lm +
                  category Lo + category Nl

char-set:digit = category Nd

char-set:letter+digit = char-set:letter + char-set:digit

char-set:graphic = char-set:printing + char-set:whitespace

char-set:printing = char-set:letter+digit + char-set:punctuation +
                    char-set:symbol +
                    category Mn + category Mc + category Me +
                    category No

char-set:whitespace = property White_Space

char-set:iso-control = 0000..001F + 007F..009F

char-set:punctuation = category Pc + category Pd + category Ps +
                       category Pe + category Pi + category Pf + category Po

char-set:symbol =  category Sm + category Sc + category Sk + category So 

char-set:hex-digit = 0030..0039 + 0041..0046 + 0061..0066

char-set:blank = category Zs + 0009

char-set:ascii = 0000..007F
```