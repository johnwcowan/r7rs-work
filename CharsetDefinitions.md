This page provides modern definitions of the
[SRFI 14](http://srfi.schemers.org/srfi-14/srfi-14.html)
character sets.  Each set is defined as the union of
the following types of sources:

*  General categories, which are in field 3 of [UnicodeData](https://www.unicode.org/Public/UNIDATA/UnicodeData.txt)

* Properties, which are in field 2 of [PropList](https://www.unicode.org/Public/UNIDATA/PropList.txt)

* Another character set

* A single codepoint

* A range of codepoints

char-set:lower-case = category Ll + property Other_Lowercase

char-set:upper-case = category Lu + property Other_Uppercase

char-set:title-case = category Lt

char-set:letter = char-set:lower-case + char-set:upper-case +
char-set:title-case + category Lm + category Lo + category Nl

char-set:digit = category Nd

char-set:letter+digit = char-set:letter + char-set:digit

char-set:graphic = char-set:printing + char-set:whitespace

char-set:printing = char-set:letter+digit + char-set:punctuation +
char-set:symbol + category Mn + category Mc + category Me + category No

char-set:whitespace = property White_Space

char-set:iso-control = 0000..001F + 007F..009F

char-set:punctuation = category Pc + category Pd +
category Ps + category Pe + category Pi + category Pf + category Po

char-set:symbol =  category Sm + category Sc + category Sk + category So 

char-set:hex-digit = 0030..0039 + 0041..0046 + 0061..0066

char-set:blank = category Zs + 0009

char-set:ascii = 0000..007F