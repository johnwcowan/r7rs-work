## Events

A text terminal or graphics canvas
may be able to send *events* to the application.  Each event represents
something happening on a keyboard, mouse, or equivalent device.
For the purposes of this SRFI,
an event is represented by a vector of at least five elements containing the following:

Element 0 is the terminal or canvas object (or possibly another type of object)
that is reporting this event.

Element 1 is a symbol indicating the type of event, one of
`char`, `key`, `mouse-up`, `mouse-down`, `mouse-move`, `resize`, `timeout`, `collision`, or `none`.
Terminals never report `collision` events.

Element 2's meaning depends on the value of element 1.

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
     
  *  For the mouse events, it is the row (terminals) or y-coordinate (for canvases)
     on which the mouse is positioned.
  
  *  For the `resize` event, it is the number of rows or vertical pixels
     in the new size of the terminal or canvas.
     The size of the UI has already been adjusted.
     
  *  For the `timeout` event, it is meaningless.
  
  *  For the `collision` event, it is the turtle object that has collided with
     the edge of the canvas or another turtle or passive shape.
     The speed of this turtle is set to 0.
     
Element 3's meaning also depends on the value of element 1.

  * For `key`, `char`, and `timeout` events, it is not used and its value is undefined.
  
  * For the mouse events, it is the column (terminal) or x-coordinate (canvas)
    on which the mouse is positioned.
    
  * For a `resize` event, it is the number of rows or vertical pixels in the new size of the terminal.
    The size of the UI has already been adjusted.
    
  * For a `collision` event, it is the canvas or shape object representing the
    canvas border or shape border with which the turtle in element 2 has collided.
    If this element is a turtle, its speed is also set to 0.

Element 4 represents the current state of the Shift, Ctrl, and Alt/Option keys.
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
