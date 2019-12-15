## Conventions

The x-coordinate increases toward the left,
the y-coordinate increases downward,
therefore the top left corner is the origin.

All positions and sizes are in pixels.

Colors are #xRRGGBB numbers,
but they may be rounded by the canvas to values it can support
(in the worst case only black and white).

## Issues

1\. Should shapes be put at the front of the z-order
when they are created (as now)
or when they are displayed?

## Canvas constructor

`(connect-canvas `*where*`)`

Connect to or create a canvas in a place
specified by the implementation-dependent object *where*.
The standardized properties are `left`, `top`, `width`, `height`,
and `color`.

## Canvas predicates

`(canvas? `*obj*`)`

Returns `#t` if *obj* is a canvas and `#f` otherwise.

`(canvas-user-resizeable? `*canvas*`)`

Returns `#t` if *canvas* can be resized by the user
(as opposed to the program) and `#f` otherwise.

## Shape constructors

Constructors make instances of the seven shapes supported by this SRFI.
When a shape is created it is not yet visible on the canvas.
When visible shapes overlap, those created most recently are on the top.

`(make-point `*canvas x y*`)`

Create and return a point at coordinates *x* and *y*.

`(make-line `*canvas x1 y1 x2 y2* ...`)`

Create and return a line (not necessarily straight)
with a segment from point (*x1, y1*) to (*x2, y2*),
and a segment from (*x2, y2*) to point (*x3, y3*) and so on.

`(make-polygon `*canvas x1 y1 x2 y2* ...`)`

Create and return a line from point (*x1, y1*) to (*x2, y2*),
and a line from (*x2, y2*) to point (*x3, y3*) and so on.
Finally a line is drawn from the last point to the first.

`(make-rectangle `*canvas left top width height*`)`

Create and return a rectangle
whose upper left corner is (*left, top*)
and whose lower right corner is (*left + width, top + height*).

`(make-ellipse `*canvas left top width height*`)`

Create and return an ellipse that just fits in the bounding box
whose upper left corner is (*left, top*)
and whose lower right corner is (*left + width, top + height*).

`(make-image `*string x y width height source*`)`

Create and return an image scaled to fit within the bounding box
whose upper left corner is (*left, top*)
and whose lower right corner is (*left + width, top + height*).
If *source* is a string, it is an URL.
Support for URLs beginning `file:` is required, and
for those beginning with `http:` and `https:` are recommended.

`(make-turtle `*x y*`)`

Create and return a turtle at coordinates (*x, y*).
A turtle is a shape that has an inherent direction and speed,
so that it moves by itself.
Its exact appearance is implementation-dependent,
but it must be possible or a user to determine the approximate
inherent direction of a turtle by looking at it;
it cannot be completely symmetrical.

## Shape predicates

`(shape? `*obj*`)`  
`(shape? `*obj canvas*`)`

Returns `#t` if *obj* is a shape and `#f` otherwise.
The second form asks if *obj* is a shape associated with *canvas*.

There is also a predicate for each shape type: `point?`, `line?`, etc.

## Shape properties

The following procedures are polymorphic in the shape argument.

`(shape-pen-color `*shape*`)`  
`(shape-pen-set-color! `*shape color*`)`

Gets or sets the pen color used to draw the outline of the shape
as well as any text displayed on the shape,
and the line behind a moving turtle when its pen is down.
The initial pen color is the inverse of the canvas foreground color.

`(shape-pen-width `*shape*`)`  
`(shape-pen-set-width! `*shape color*`)`

Gets or sets the pen width used to draw the outline of the shape
and the line behind a moving turtle when its pen is down.
The initial pen width is 1 pixel.

`(shape-fill-color `*shape*`)`  
`(shape-fill-set-color! `*shape color*`)`

Gets or sets the color used to draw the inside of the shape.
The initial fill color is whatever the color of the canvas was
when the shape was created.

`(shape-halign `*shape*`)`  
`(shape-set-halign! `*shape value*`)`

Gets or sets the horizontal alignment of text on the shape.
Possible values are `left`, `centered` (the default), and `right`.

`(shape-valign `*shape*`)`  
`(shape-set-valign! `*shape value*`)`

Gets or sets the vertical alignment of text on the shape.
Possible values are `top`, `centered` (the default), and `bottom`.

`(shape-font `*shape*`)`  
`(shape-set-font! `*shape fontname*`)`

Gets or sets the name of the font used to draw text on *shape*.
The available font names are implementation dependent.

`(shape-fontsize `*shape*`)`  
`(shape-set-fontsize! `*shape fontsize*`)`

Gets or sets the size of the font used to draw text on *shape*
as an exact integer.

`(shape-bold? `*shape*`)`  
`(shape-set-bold! `*shape boolean*`)`  
`(shape-italic? `*shape*`)`  
`shape-(set-italic! `*shape boolean*`)`

Gets or sets the bold/italic apperance of the font used to draw text on *shape*
as boolean values.

## Shape actions

`(shape-show! `*shape*`)`  
`(shape-hide! `*shape color*`)`

Shows or hides *shape*.

`(shape-move! `*shape x y*`)`

Moves *shape* so that its origin point is at (*x, y*).
The origin of a point is itself;
the origin of a line or polygon is the first point drawn;
the origin of a rectangle, ellipse, or image is the upper left point;
the origin of a turtle is the center of the turtle.

`(shape-expose!`*shape*`)`  
`(shape-bury! `*shape*`)`

Please *shape* on the top/bottom of the stack of overlapping shapes.

`(shape-display-text! `*shape text*`)`

Draw *text* (a string) on *shape*, centered by default.
The default font for drawing text and its appearance properties are implementation dependent.
Not all shapes can be drawn on.

`(shape-mouse-link!` *turtle*`)`  
`(shape-mouse-unlink!` *turtle*`)`

Causes *shape* to move in the same direction and distance that the mouse moves.
However, it does not move the shape to the position of the mouse or vice versa.
While a shape is linked, mouse movements may or may not not be received by the UI event system.
The number of shapes that can be linked with the mouse may be limited by the implementation.

`(shape-dispose! `*shape*`)`

Destroys the shape, removing it from the canvas altogether.
It is an error to get or set any property of a shape
or take any action on it after it has been disposed of.

## Turtle properties

`(turtle-speed `*turtle*`)`  
`(turtle-set-speed! `*turtle n*`)`

Gets or sets the turtle's speed in spontaneous motion to *n*.
The initial value is 0, which means the turtle moves only
when it is told to move.

`(turtle-direction `*turtle*`)`  
`(turtle-set-direction! `*turtle n*`)`

Gets or sets the inherent direction of the turtle as an angle in degrees.
The initial value is 0, which means the turtle points
in the negative y-direction of the canvas (normally straight up).

## Turtle actions

`(turtle-down! `*turtle*`)`

Put the turtle's pen down,
so that it draws a line behind itself as it moves.
Note that this line is not itself a shape.
It is superimposed on all shapes on the canvas.
The initial state of a turtle's pen is down.

`(turtle-up! `*turtle*`)`

Put the turtle's pen up, so that it does not draw a line.

`(turtle-forward! `*turtle distance*`)`  
`(turtle-backward! `*turtle distance*`)`

Moves the turtle forward/backward the given distance in its inherent direction.
The speed with which the turtle moves is specified by the `turtle-speed` property,
unless that value is 0, in which case the turtle's speed is implementation-specified.

`(turtle-left! `*turtle degrees*`)`  
`(turtle-right! `*turtle degrees*`)`

Turn the turtle's inherent direction by *degrees*.
The use of degrees instead of radians is traditional,
stemming from the original Logo turtles.

## Canvas properties

`(canvas-color `*canvas*`)`  
`(canvas-set-color! `*canvas color*`)`

Get or set the background color of *canvas*.  

`(canvas-mouse-x `*canvas*`)`  
`(canvas-mouse-y `*canvas*`)`

Returns the x or y coordinate of the mouse,
or -1 if the mouse is not in the canvas.
The mouse position cannot be set.

`(canvas-shapes `*canvas*`)`

Return a list of all shapes associated with *canvas*.
It is an error to mutate the pairs of this list.

`(canvas-left `*canvas*`)`  
`(canvas-set-left! `*canvas left*`)`

Attempt to get or set the x-coordinate of the origin of *canvas*
relative to some arbitrary coordinate system such as the whole screen.`(canvas-left `*canvas*`)`  

`(canvas-top `*canvas*`)`  
`(canvas-set-top! `*canvas top*`)`

Attempt to get or set the y-coordinate of the origin of *canvas*
relative to some arbitrary coordinate system such as the whole screen.

`(canvas-width `*canvas*`)`  
`(canvas-set-width! `*canvas x*`)`

Attempt to get or set the width of *canvas*.

`(canvas-height `*canvas*`)`  
`(canvas-set-height! `*canvas height*`)`

Attempt to get or set the height of *canvas*.

`(canvas-title `*string*`)`  
`(canvas-set-title! `*canvas title*`)`

Attempt to get or set the title of *canvas*.
If *canvas* has no associated title,
return the empty string or silently take no action.

## Canvas actions

`(canvas-show! `*canvas*`)`  
`(canvas-hide! `*canvas*`)`

Reveal or conceal the canvas.  If this is not possible, nothing happens.

`(canvas-clear! `*canvas*`)`

Clear the canvas by hiding all visible shapes
and repainting everything with the canvas color.

`(canvas-dispose! ` *canvas*`)`

Disconnect from or eliminate *canvas*,
disposing of all shapes created from it.

### Events

Canvases can report events according to FIXME [the UI event SRFI](UiEvents.md).
Calling `uievent-poll` will report events for canvases, possibly intermixed with
other events.
