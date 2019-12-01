## UI Events

A text terminal or graphics canvas
may be able to send *events* to the application.  Each event represents
something happening on a keyboard, mouse, or equivalent device.
For the purposes of this SRFI,
an event is represented an *event object*, an object with various
properties.  It is an error to try to retrieve a property from an event
that is not appropriate to the type of event.  Event objects cannot be mutated
by the user, only by the event dispatcher.

## Event constructor `(make-uievent)`

Returns an event object for `event-poll!` to fill in.

## Event properties

`(uievent-source `*ev*`)`

Returns the terminal or canvas object (or possibly another type of object)
that is reporting this event.

`(uievent-type `*ev*`)`

Returns a symbol indicating the type of event, one of
`char`, `key`, `mouse-up`, `mouse-down`, `mouse-move`, `resize`, `timeout`, `collision`, or `none`.
Terminals never report `collision` events.

`(uievent-char `*ev*`)`

Returns the character input using the keyboard or equivalent.
This is a Unicode character, not a keyboard key, and so the effects of keyboard
drivers, compose sequences, and input method editors have already been taken
into account.  It is an error if the event source is not `char`.
     
`(uievent-key `*ev*`)`

Returns a symbol representing
a key that is not bound to a character.
The following symbols are standardized:
`up-arrow`, `down-arrow`, `left-arrow`, `right-arrow`,
`insert`, `delete`, `home`, `end`, `page-up`, `page-down`,
and `f1` through `f12`.
Implementations may return other symbols.
It is an error if the event type is not `key`.

`(uievent-x `*ev*`)`

For the mouse events, returns the column (for terminals) or x-coordinate (for canvases)
on which the mouse is positioned.
  
For `resize` events, returns the number of columns or horizontal pixels
in the new size of the terminal or canvas.
The size of the UI has already been adjusted.
     
`(uievent-y `*ev*`)`

For the mouse events, returns the row (terminal) or y-coordinate (for canvases)
    on which the mouse is positioned.
    
For a `resize` event, returns the number of rows or vertical pixels in the new size of the terminal.
The size of the UI has already been adjusted.
    
`(uievent-turtle `*ev*`)`

Returns the turtle object that has collided with
the edge of the canvas or another turtle or passive shape.
The speed of this turtle has been set to 0.
     
Element 3's meaning also depends on the value of element 1.

`(uievent-FIXME `*ev*`)`

Returns the canvas or shape object representing the
canvas border or shape border with which a turtle has collided.
Note that when two turtles collide, two events are reported in arbitrary order.

`(uievent-modifiers `*ev*`)`

Returns an exact integer representing the current state of the Shift, Ctrl, and Alt/Option keys.
For `char` events, the Shift key has already been absorbed into the Unicode character.
Note that a key combination like Ctrl+C will be reported as the character `#\x3`.
In these cases the Shift and/or Ctrl keys may or may not be reported.

Implementations are free to consolidate consecutive mouse movement events.

## Event handling

`(event-poll! `*event timeout* [*alt?*]`)`

Waits for the next event to become available or until *timeout* jiffies have passed;
the implementation may round up the number of jiffies to suit its granularity.
The resulting event is written into the first five elements of the vector *event*.
If the value of *timeout* is 0 and no event is available, `event-poll!` returns
immediately, setting element 1 to `none`; elements 2-4 are unspecified.

The variables `term-shift`, `term-ctrl`, and `term-alt` have values that are distinct
positive powers of two.  Element 3 of an event consists of the sum of zero or more of them.

If *alt?* is true, then when an ESC character (`#\x1B;`) is received that is
followed very shortly thereafter by another `char` event, only a single event
is reported with the `alt` modifier.
If *alt?* is false or omitted, no such special processing is done.
