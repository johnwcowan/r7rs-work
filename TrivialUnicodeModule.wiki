Since the current votes in WG1 call for a module for Unicode casing and normalization, here's a trivial proposal for that module.

It's a subset of the R6RS (rnrs unicode 6) library, but the semantics of the procedures are supplied by UnicodeCowan.

{{{
 char-upcase char-downcase char-foldcase
 char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=
 char-alphabetic char-numeric char-whitespace char-upper-case char-lower-case
 string-upcase string-downcase string-foldcase
 string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=
 string-normalize-nfd string-normalize-nfkd string-normalize-nfc string-normalize-nfkc
}}}