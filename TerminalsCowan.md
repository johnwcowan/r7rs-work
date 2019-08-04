## Introduction

Terminals used to be physical devices but now most of them are emulators, blah blah.
For the purposes of this application, a terminal might be a physical terminal,
an X textual window, a Windows console, or even an HTML multi-line field.

Physical terminals had 1-4 fixed sizes
available; modern terminals can be variable in size, but always an integral
number of rows and columns.  The operating system typically has some way of
notifying an application when the size of a terminal it is using changes.

## The model

### Terminals

A terminal is represented in this SRFI by a rectangular grid of *locations*
whose total size depends on the current state of the terminal.
Each location holds one or more
Unicode characters that combine to form a single grapheme,
with the exception of wide East Asian characters,
which occupy two horizontally consecutive locations.
In addition, each location specifies a foreground color or *fgcolor*,
for the positive space (in the typographer's sense) of the characters,
and a background color or *bgcolor*
for the negative space (in the typographer's sense) of the characters.

The grid is not physically displayed on the terminal until
`term-sync` is invoked.

It is an error to read or write outside the boundaries of the grid.
The upper left corner is row 0, column 0, as is normally the case in Scheme.

### Location strings

It is an error unless the characters present in a single location constitute a single
Unicode default grapheme cluster.  In addition, the string may contain
any number of ANSI escape sequences and control sequences.  (FIXME: add links.)

### Color representations

Colors are represented by exact non-negative integers in the form `#xRRGGBB`,
where `RR` is a 256-bit redness value, `GG` is a 256-bit greenness value, and
`BB` is a 256-bit blueness value.  However, terminals are free to round these
colors to as few as 8 colors if that's all they can support.

### Asian width

For the purposes of this SRFI,
every Unicode character is either zero-width, narrow (halfwidth),
ambiguous, or wide (fullwidth).  A zero-width character shares its
location with a narrow or wide character, which occupy 1 or 2
locations respectively.  Ambiguous characters are treated
as narrow or wide depending on the *ambiguous* argument to `term-init`,
but as narrow by default.

### Events

In addition to displaying a grid of characters, a terminal
may be able to send *events* to the application.  Each event represents
something happening on a keyboard, mouse, or equivalent device.
Applications are free to consolidate consecutive mouse movement events.
For the purposes of this SRFI,
an event is represented by a four-element vector containing the following:

Element 0 is a symbol indicating the type of event, one of
`char`, `key`, `mouse-up`, `mouse-down`, `mouse-move`, `resize`, or `timeout`.

Element 1's meaning depends on the value of element 0.

  *  For the `char` event, it is the character that was input using the keyboard or equivalent.
     This is a Unicode character, not a keyboard key, and so the effects of keyboard
     drivers, compose sequences, and input method editors have already been taken
     into account.
     
  *  For the `key` event, it is a symbol representing
     a key that is not bound to a character.
     The following symbols are standardized:
     `up-arrow`, `down-arrow`, `left-arrow`, `right-arrow`,
     `insert`, `delete`, `home`, `end`, `page-up`, `page-down`,
     and `f1` through `f12`.
     Implementations may return other symbols.
     
  *  For the mouse events, it is the row on which the mouse is positioned.
  
  *  For the `resize` event, it is the number of rows in the new size of the terminal.
     The size of the grid has already been adjusted.
     
  *  For the `timeout` event, it is meaningless.
     
Element 2 is not used with `key`, `char`, and `timeout` events,
and its contents are undefined in those cases.
Mouse events use it to report the column on which the mouse is positioned;
resize events use it to report the number of columns in the new size of the terminal.

Element 3 represents the current state of the Shift, Ctrl, and Alt keys on the keyboard
as an exact integer.  Only the Alt key is reported for `char` events, as the states of
the Shift and Ctrl keys are incorporated into the Unicode character in element 1.


## Basic procedures

Note: Arguments named *t* are terminal objects as returned by `terminal-init`.

`(term-init `*which bgcolor* [*ambig* [*alt* ] ]`)`

Initializes a terminal and returns an implementation-dependent terminal
object.

Which terminal is initialized is determined
by the implementation-dependent parameter *which*, except that a value
of `#t` represents the principal terminal associated with the application, if any.
The terminal is cleared: all locations are set to spaces and the bgcolor
set to *bgcolor*.

The *ambig* and *alt* arguments are not often needed, but are sometimes useful.
If *ambig* is true, the Unicode characters with ambiguous Asian width (see above)
are treated as wide (fullwidth); if false or omitted, as narrow (halfwidth).

If *alt* is true, then when an ESC character (U+001B) is received that is not
followed very shortly thereafter by another character, it is assumed to be
equivalent to whatever the next character is with the `alt` modifier (again,
see above), and this is remembered and the ESC suppressed.
If *alt* is false or omitted, no such special processing is done.

`(term-shutdown `*t*`)`

The terminal identified by *t* is shut down, and further operations on the
object are ignored.

`(term-height `*t*`)`

The height of *t* (number of rows) is returned as an exact positive integer.

`(term-width `*t*`)`

The width of *t* (number of columns) is returned as an exact positive integer.

`(term-clear `*t bgcolor*`)`

All the locations of *t* are set to spaces of the specified *bgcolor*.

`(term-get `*t row column*`)`

Returns three values, *string, *fgcolor*, and *bgcolor*, corresponding
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

`(term-cursor-ref `*t*`)`

Returns two values, the row and column at which the terminal's *cursor*
(a visual indication of some sort) is currently placed.

`(term-cursor-set! `*t row column*`)`

Sets the terminal's cursor to *row* and *column*.
Returns an unspecified value.

`(term-read-string `*t start-row start-column end-row end-column* [ *rect?* [ *full* ] ]`)`

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
But if *rect* is true, only the contents of the rectangle
defined by the four corners is returned.
Trailing spaces are trimmed 
In either case, newline characters are inserted at the ends
of each row (FIXME: except the last? when?).

If *full?* is true, consecutive trailing spaces on each row
that extends to the last column are returned;
if it is false or absent, they are suppressed.
If *rect?* is true, the value of *full?* is always treated as true.

`(term-write-string `*t row column string fgcolor bgcolor*`)`

In effect, performs repeated `term-set` operations starting at
*row* and *column* and advancing to consecutive following
(FIXME: bidi) columns.
Each location holds as many characters of *string* as it can (see above).
If a newline character is written,
the current column is set to 0 and the current row is incremented.
If the escape sequence ESC M is written,
the current column is set to 0 and the current row is decremented.

`(term-sync `*t* [*hard?*]`)`

Causes the current state of the external terminal to be the same
as the current state of the grid.  The cursor is restored to its
previous location.

If *hard?* is true, nothing is assumed about the current state
of the external terminal.
If *hard?* is false or absent, the implementation is allowed to
keep track of the state of the terminal after the previous sync
and to take it into account when implementing the current sync.

`(event-poll! `*event timeout*`)`

Waits for the next event to become available or until *timeout* jiffies have passed.
The resulting event is written into the first few elements of the vector *event*.
If the value of *timeout* is 0 and no event is available, `event-poll!` returns
immediately.

The variables `term-shift`, `term-ctrl`, and `term-alt` have values that are distinct
positive powers of two.  Element 3 of an event consists of the sum of zero or more of them.

This procedure may be shared with other SRFIs providing events.


## The whole terminal

The following procedures affect the state of the external terminal
rather than the grid.  They may or may not do anything, depending
on the external terminal.  They take effect without waiting for a
`term-sync` to be performed.

`(term-title `*t string*`)`

Attempts to set the terminal title (typically displayed above the grid) to *string*.

`(term-resize `*t rows columns*`)`

Attempts to set the size of both the external terminal and the grid
to the specified number of rows and columns.

`(term-hide `*t*`)`

Attempts to conceal the terminal from the user.

`(term-show `*t*`)`

Attempts to reveal the terminal to the user.

`(term-font `*t name italic? bold?*`)`

Attempts to set the terminal's typeface using *name*,
which is a string.  It is an error if this typeface is
not either monowidth or (if it supports Asian wide characters)
duowidth.  The *italics?* and *bold?* flags attempt to set
those characteristics of the specific font to be used.
For legacy reasons, the value of *bold?* may affect the
fgcolor rather than the font. 

