See UcdCowan for basic UCD procedures.

It is an error to mutate any objects returned by these procedures.

== Blocks ==

''Blocks'' are disjoint objects that represent the allocation blocks into which the Unicode code point space is divided for administrative purposes.  Typically most of a block is allocated at once and contains characters from a single script, but there is often more than one block per script, some blocks contain characters from multiple scripts, and some characters in a block are allocated much later than the rest.  The list of blocks provided is implementation-dependent.  Since it is not possible to create new ones, `eqv?` may be used to compare them.

`(blocks)`

Returns a list of all blocks known to the implementation.

`(block-name `''block''`)`

Returns a string naming ''block''.

`(block-first `''block''`)`

Returns an exact integer representing the first (smallest) code point in ''block''.

`(block-last `''block''`)`

Returns an exact integer representing the last (largest) code point in ''block''.

== Named Sequences ==

Named sequences are disjoint objects which represent a sequence of Unicode code points that has a name specified by the Unicode Standard.  Named sequences may be provisional in one version of the UCD and then non-provisional in later versions.  The list of named sequences provided is implementation-dependent.  Since it is not possible to create new ones, `eqv?` may be used to compare them.

`(named-sequences)`

Returns a list of all named sequence objects known to the implementation.

`(named-sequence-name `''named-sequence''`)`

Returns a string naming ''named-sequence''.

`(named-sequence-code-points `''named-sequence''`)`

Returns a list of exact integers representing the code points of ''named-sequence''.

`(named-sequence-provisional? `''named-sequence''`)`

Returns `#t` if the ''named-sequence'' is provisional, or `#f` if not.

== Normalization corrections ==

''Normalization-corrections'' are disjoint objects that represent official corrections to the UCD normalization tables.  The list of normalization-corrections provided is implementation-dependent.  Since it is not possible to create new ones, `eqv?` may be used to compare them.

`(normalization-corrections)`

Returns a list of all normalization-corrections known to the implementation.

`(normalization-correction-description `''normalization-correction''`)`

Returns a string describing ''normalization-correction''.  Note that normalization-corrections don't have names.

`(normalization-correction-codepoint `''normalization-correction''`)`

Returns an exact integer specifying the code point of the character whose normalization is being corrected.

`(normalization-correction-old `''normalization-correction''`)`

Returns a list of exact integers specifying the normalization of `(normalization-correction-codepoint `''block''`)` before ''normalization correction'' is applied.

`(normalization-correction-new `''normalization-correction''`)`

Returns a list of exact integers specifying the normalization of `(normalization-correction-codepoint `''block''`)` after ''normalization correction'' is applied.

`(normalization-correction-version `''normalization-correction''`)`

Returns a list of three exact integers specifying the version of the UCD (in the format of `ucd-version`) in which ''normalization-correction'' was applied.

== Standardized variants ==

''Standardized-variants'' are disjoint objects that represent standardized variants of base charactesr.  The list of standardized-variants provided is implementation-dependent.  Since it is not possible to create new ones, `eqv?` may be used to compare them.

`(standardized-variants)`

Returns a list of all standardized-variants known to the implementation.

`(standardized-variants-description `''standardized-variant''`)`

Returns a string describing ''standardized-variant''.  Note that standardized-variants don't have names.

`(standardized-variants-when `''standardized-variant''`)`

Returns a string specifying the shaping environment under which ''standardized-variant'' is applied.

`(standardized-variant-base-codepoint `''standardized-variant''`)`

Returns an exact integer specifying the code point of the base character of ''standardized-variant''.

`(standardized-variant-variant-codepoint `''standardized-variant''`)`

Returns an exact integer specifying the code point of the base character of ''standardized-variant''.
'''Issue: this name is regrettable.'''

=== CJK radicals ===

''CJK-radicals'' are disjoint objects that represent the mapping between a radical (which in turn represents the semantic portion of a CJK character) and the ideographic character identical to that radical.  The list of cjk-radicals provided is implementation-dependent.  Since it is not possible to create new ones, `eqv?` may be used to compare them.

`(cjk-radicals)`

Returns a list of all CJK-radicals known to the implementation.

`(cjk-radical-number `''cjk-radical''`)`

Returns an exact integer identifying ''cjk-radical''.

`(cjk-radical-codepoint `''cjk-radical''`)`

Returns an exact integer representing the codepoint of ''cjk-radical''.

`(cjk-radical-ideograph `''cjk-radical''`)`

Returns an exact integer representing the codepoint of the ideograph equivalent of ''cjk-radical''.


=== Emoji sources ===

''Emoji-sources'' are disjoint objects that represent the mapping between Unicode and the sources of ''emoji'' (emoticon) characters.  Each emoji-source maps one or more Unicode codepoints to an exact integer representing a JIS codepoint as assigned by the !DoCoMo, KDDI, and !SoftBank sources.  The list of emoji-sources provided is implementation-dependent.  Since it is not possible to create new ones, `eqv?` may be used to compare them.

`(emoji-sources)`

Returns a list of all emoji-sources known to the implementation.

`(emoji-source-codepoints `''emoji-source''`)`

Returns a list of exact integers representing the Unicode codepoints of ''emoji-source''.

`(emoji-source-docomo `''emoji-source''`)`

Returns an exact integer representing the JIS code point as assigned by !DoCoMo in ''emoji-source'', or `#f` if there is none.

`(emoji-source-kddi `''emoji-source''`)`

Returns an exact integer representing the JIS code point as assigned by KDDI in ''emoji-source'', or `#f` if there is none.

`(emoji-source-docomo `''emoji-source''`)`

Returns an exact integer representing the JIS code point as assigned by !SoftBank in ''emoji-source'', or `#f` if there is none.
