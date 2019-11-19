## Conventions

The x-coordinate increases toward the left,
the y-coordinate increases downward,
therefore the top left corner is the origin.

All positions and sizes are in pixels.

Colors are #xRRGGBB numbers,
but they may be rounded by the canvas to values it can support
(in the worst case only black and white).

## Canvas constructor

`(connect-canvas `*alist*`)`

Connect to or create a canvas in a place specified by alist.
The standardized properties are `left`, `top`, `width`, `height`,
and `color`.

## Canvas predicates

`(canvas? `*obj*`)`

Returns `#t` if *obj* is a canvas and `#f` otherwise.

`(canvas-user-resizeable? `*canvas*`)`

Returns `#t` if *canvas* can be resized by the user
(as opposed to the program) and `#f` otherwise.

## Canvas properties

`(canvas-color `*canvas*`)`  
`(canvas-set-color! `*canvas color*`)`

Get or set the background color of *canvas*.  

`(canvas-left `*canvas*`)`  
`(canvas-set-left! `*canvas left*`)`

Get or set the x-coordinate of the origin of *canvas*
relative to some arbitrary coordinate system such as the whole screen.`(canvas-left `*canvas*`)`  

`(canvas-top `*canvas*`)`  
`(canvas-set-top! `*canvas top*`)`

Get or set the y-coordinate of the origin of *canvas*
relative to some arbitrary coordinate system such as the whole screen.

`(canvas-width `*canvas*`)`  
`(canvas-set-width! `*canvas x*`)`

Get or set the width of *canvas*.

`(canvas-height `*canvas*`)`  
`(canvas-set-height! `*canvas height*`)`

Get or set the height of *canvas*.

`(canvas-title `*string*`)`  
`(canvas-set-title! `*canvas title*`)`

Get or set the title of *canvas*.
If *canvas* has no associated title,
return the empty string or silently take no action.

`(canvas-mouse-x `*canvas*`)`  
`(canvas-mouse-y `*canvas*`)`

Returns the x or y coordinate of the mouse,
or -1 if the mouse is not in the canvas.
The mouse position cannot be set.

`(canvas-shapes `*canvas*`)`

Return a list of all shapes associated with *canvas*.
It is an error to mutate the pairs of this list.

## Canvas actions

`(canvas-show `*canvas*`)`  
`(canvas-hide `*canvas*`)`

Reveal or conceal the canvas.  If this is not possible, nothing happens.

`(canvas-clear `*canvas*`)`

Clear the canvas by hiding all visible shapes
and repainting everything with the canvas color.

`(canvas-dispose ` *canvas*`)`

Disconnect from or eliminate *canvas*,
disposing of all shapes created from it.

## Text

`(canvas-display-text `*canvas x y text*`)`

Display *text* on the canvas starting at coordinates *x* and *y*.
Text is written on top of any shapes that occupy all or part of its display area,
but text does not move when a shape does; it remains anchored to the canvas.
The default font and its properties are implementation dependent.

`(text-color `*canvas*`)`  
`(set-text-color! `*canvas color*`)`

Gets or sets the color of the pen used to draw the text.

`(text-font `*canvas*`)`  
`(set-text-font! `*canvas fontname*`)`

Gets or sets the name of the font used to draw the text.
The available font names are implementation dependent.

`(text-size `*canvas*`)`  
`(set-text-size! `*canvas fontsize*`)`

Gets or sets the size of the font used to draw the text.

`(text-bold? `*canvas*`)`  
`(set-text-bold! `*canvas*`)`

Gets or sets the bold status of the font used to draw the text.

`(text-italic? `*canvas*`)`  
`(set-text-italic! `*canvas*`)`

Gets or sets the italic status of the font used to draw the text.

## Shape constructors

Constructors make instances of the seven shapes supported by this SRFI.
When a shape is created it is not yet visible on the canvas.
When visible shapes overlap, those created most recently are on the top.

`(make-point `*canvas x y*`)`

Create a point at coordinates *x* and *y* and return it.

`(make-line `*canvas x1 y1 x2 y2* ...`)`

Create a line from point (*x1, y1*) to (*x2, y2*),
and a line from there to point (*x3, y3*) and so on.

`(make-polygon `*canvas x1 y1 x2 y2* ...`)`

Create a line from point (*x1, y1*) to (*x2, y2*),
and a line from there to point (*x3, y3*) and so on.
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

Create an return an image scaled to fit within the bounding box
whose upper left corner is (*left, top*)
and whose lower right corner is (*left + width, top + height*).
If *source* is a string, it is an URL.
Support for URLs beginning `file:` is required, and
for those beginning with `http:` and `https:` are recommended.

`(make-turtle `*x y*`)`

Creates a turtle at coordinates (*x, y*).
A turtle is a shape that can and does move by itself.
Its exact size and shape are implementation-dependent,
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

`(pen-color `*shape*`)`  
`(pen-set-color! *shape ` color*`)`

Gets or sets the pen color used to draw the outline of the shape.
The initial pen color is black.

`(pen-width `*shape*`)`  
`(pen-set-width! *shape ` color*`)`

Gets or sets the pen width used to draw the outline of the shape.
The initial pen width is 1 pixel.

`(pen-fill-color `*shape*`)`  
`(pen-set-fill-color! *shape ` color*`)`

Sets the pen color used to draw the inside of the shape.
The initial pen color is whatever the color of the canvas was
when the shape was created.

## Shape actions

`(shape-show! `*shape*`)`  
`(shape-hide! *shape ` color*`)`

Shows or hides *shape*.  When *shape* is hidden.

`(shape-move! `*shape x y*`)`

Moves *shape* so that its origin point is at (*x, y*).
The origin of a point is itself;
the origin of a line, polygon, or image is the first point drawn;
the origin of a rectangle, ellipse, or image is the upper left point;
the origin of a turtle is the center of the turtle image.

`(shape-dispose `*shape*`)`

Destroys the shape, removing it from the canvas altogether.

## Turtle properties

`(turtle-speed `*turtle n*`)`  
`(turtle-set-speed! `*turtle n*`)`

Gets or sets the turtle's speed in spontaneous motion to *n*.
The initial value is 0, which means the turtle moves only
when it is told to move.

`(turtle-angle `*turtle n*`)`  

Gets the angle in degrees in which the turtle points.
The initial value is 0, which means the turtle points straight up.

## Turtle actions

`(turtle-down! `*turtle*`)`

Put the turtle's pen down, so that it draws a line behind itself.
Note that this line is not itself a shape.  It is superimposed
on all shapes and text on the canvas.

`(turtle-up! `*turtle*`)`

Put the turtle's pen up, so that it does not draw a line.

`(turtle-forward! `*turtle distance*`)`  
`(turtle-backward! `*turtle distance*`)`

Moves the turtle forward/backward the given distance.
The speed with which the turtle moves is specified by the `turtle-speed` property,
unless that value is 0, in which case the turtle's speed is implementation-specified.

`(turtle-left! `*turtle degrees*`)`  
`(turtle-right! `*turtle degrees*`)`

Turn the turtle's inherent direction by *degrees*.
The use of degrees instead of radians is traditional,
stemming from the original Logo turtles.

`(turtle-mouse-link!` *turtle*`)`  
`(turtle-mouse-unlink!` *turtle*`)`

Links/unlinks the relative movement of the turtle with the relative movement of the mouse.
However, it does not move the turtle to the position of the mouse or vice versa.
While they are linked, mouse movements may not be received by the UI event system.
Has no effect if this cannot be done.

## Events

See the [UI event SRFI](UiEvents.md).
