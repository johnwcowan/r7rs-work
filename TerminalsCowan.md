## Introduction

Terminals used to be physical devices but now most of them are emulators, blah blah.
For the purposes of this application, a terminal might be a physical terminal,
an X textual window, a Windows console, or even an HTML multi-line field.

Physical terminals had 1-4 fixed sizes
available; modern terminals can be variable in size, but always an integral
number of rows and columns.  The operating system typically has some way of
notifying an application when the size of a terminal it is using changes.

## Issues

1\. Bidirectional behavior.

## The model

### Terminals

A terminal is represented in this SRFI by a rectangular grid of *locations*
whose total size depends on the current state of the terminal.
Each location holds one or more
Unicode characters that combine to form a single grapheme,
with the exception of wide East Asian characters,
which occupy two horizontally consecutive locations.
In addition, each location specifies a foreground color or *fgcolor*,
for the grapheme itself,
and a background color or *bgcolor*
for the rest of the space in the location..

The grid is not physically displayed on the terminal until
`term-sync` is invoked.

It is an error to read or write outside the boundaries of the grid.
The upper left corner is row 0, column 0, as is normally the case in Scheme.

### Location contents

The characters present in a single location constitute a single
Unicode default grapheme cluster.  In addition, the string may contain
any number of ANSI escape sequences and control sequences.  (FIXME: add links.)

### Color representations

Colors are represented by exact non-negative integers in the form `#xRRGGBB`,
where `RR` is a 256-bit redness value, `GG` is a 256-bit greenness value, and
`BB` is a 256-bit blueness value.  However, terminals are free to round these
colors to as few as 8 or even 2 colors if that's all they can support.

### Asian width

For the purposes of this SRFI,
every Unicode character is either zero-width, narrow (halfwidth),
ambiguous, or wide (fullwidth).  A zero-width character shares its
location with a narrow or wide character, which occupy 1 or 2
locations respectively.  Ambiguous characters are treated
as narrow or wide depending on the *ambiguous* argument to `term-init`,
but as narrow by default.

### Events

Terminals can report events according to FIXME [the UI event SRFI](UiEvents.md).
Calling `event-poll!` will report events for terminals, possibly intermixed with
other events.

## Terminal constructor

`(term-connect `*which bgcolor* [*ambig*]`)`

Initializes a terminal and returns an implementation-dependent terminal
object.

Which terminal is initialized is determined
by the implementation-dependent parameter *which*, except that a value
of `#t` represents the principal terminal associated with the application, if any.
The terminal is cleared: the bgcolor is set to *bgcolor* and all locations are set to spaces.

The *ambig* argument is not often needed, but is sometimes useful.
If it is true, the Unicode characters with ambiguous Asian width (see above)
are treated as wide (fullwidth); if false or omitted, as narrow (halfwidth).

## Grid properties

`(term-get `*t row column*`)`

Returns three values, *string*, *fgcolor*, and *bgcolor*, corresponding
to the contents of the location in *t* at *row* and *column*.

`(term-set! `*t row column string fgcolor bgcolor*`)`

Sets the location of *t* at *row* and *column* to the specified string
and colors.  It is an error if *string* does not conform to the rules
for what can appear at a single location.  If *string* contains a wide
character, the location in the same row and the following column is
automatically set to an empty string and the same colors as this location.'
Returns an unspecified value.

For legacy reasons, it is an error to set the location at the last row
and last column.

`(term-cursor-row `*t*`)`  
`(term-set-cursor-row! `*t n*`)`  
`(term-cursor-column `*t*`)`  
`(term-set-cursor-column! `*t n*`)`

Gets or sets the row/column at which the terminal's *cursor*
(a visual indication of some sort) is currently placed.

## Terminal properties

The following procedures get or the state of the external terminal rather than the grid.
They may or may not do anything, depending on the external terminal.
If a property is not retrievable, `#f` is returned.
They may take effect without waiting for a `term-sync` to be performed.

`(term-title `*t*`)`  
`(term-set!-title `*t string*`)`

Attempts to get or set the terminal title (typically displayed above the grid) to *string*.

`(term-width `*t*`)`  
`(term-set-width! `*t rows*`)`  
`(term-height `*t*`)`  
`(term-set-height! `*t columns*`)`

Attempts to get or set the width/height of both the external terminal and the grid.

`(term-font `*t*`)`  
`(term-set-font! `*t fontname*`)`

Attempts to get or set the terminal's font,
which is a string.  It is an error to set a font that is
not either monowidth or (if it supports Asian wide characters)
duowidth.

`(term-font-size `*t*`)`  
`(term-set-font-size! `*t n*`)`

Attempts to set or get the terminal's font size in points.
The initial value is implementation-defined and may depend on the terminal.

`(term-bold? `*t*`)`  
`(term-set-bold! `*t name italic? bold?*`)`  
`(term-italic? `*t*`)`  
`(term-set-italic?! `*t name italic? bold?*`)`

Attempts to get or set those characteristics of the specific font to be used.
For legacy reasons, the value of *bold?* may affect the fgcolor rather than the font.

## Reading and writing

`(term-read `*t start-row start-column end-row end-column* [ *rect?* [ *full?* ] ]`)`

The strings in the locations starting at *start-row* and *start-column* and
extending to *end-row* and *end-column* inclusive are concatenated and returned.
Color information is ignored.

If *rect?* is false or absent, the contents of 
the Z-shaped area
that begins at *start-row* and *start-column*
and extends to the end of *start-row*, followed
by all the rows between *start-row* and *end-row*
exclusive, followed by *end-row* from column 0 to
*end-column*, are returned.
But if *rect?* is true, only the contents of the rectangle
defined by the four corners is returned.
Trailing spaces are trimmed 
In either case, newline characters are inserted at the ends
of each row.

If *full?* is true, consecutive trailing spaces on each row
that extends to the last column are returned;
if it is false or absent, they are suppressed.
If *rect?* is true, the value of *full?* is always treated as true.

`(term-write! `*t row column string fgcolor bgcolor*`)`

In effect, performs repeated `term-set!` operations starting at
*row* and *column* and advancing to consecutive following
(FIXME: bidi) columns.
Each location holds as many characters of *string* as it can (see above).
If a newline character is written,
the current column is set to 0 and the current row is incremented.
If the escape sequence ESC M is written,
the current column is set to 0 and the current row is decremented.

## Actions

`(term-hide! `*t*`)`

Attempts to conceal the terminal from the user.

`(term-show! `*t*`)`

Attempts to reveal the terminal to the user.

`(term-clear! `*t bgcolor*`)`

All the locations of *t* are set to spaces of the specified *bgcolor*.

`(term-sync! `*t* [*hard?*]`)`

Causes the current state of the external terminal to be the same
as the current state of the grid.  The cursor is restored to its
previous location.

If *hard?* is true, nothing is assumed about the current state
of the external terminal.
If *hard?* is false or absent, the implementation may
keep track of the state of the terminal after the previous sync
and take it into account when implementing the current sync.

`(term-dispose! `*t*`)`

The terminal is shut down, and further operations on the
object are an error.  This may or may not have any external effect.

