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
`(set-canvas-color! `*canvas color*`)`

Get or set the background color of *canvas*.  

`(canvas-left `*canvas*`)`  
`(set-canvas-left! `*canvas left*`)`

Get or set the x-coordinate of the origin of *canvas*
relative to some arbitrary coordinate system such as the whole screen.`(canvas-left `*canvas*`)`  

`(canvas-top `*canvas*`)`  
`(set-canvas-top! `*canvas top*`)`

Get or set the y-coordinate of the origin of *canvas*
relative to some arbitrary coordinate system such as the whole screen.

`(canvas-width `*canvas*`)`  
`(set-canvas-width! `*canvas x*`)`

Get or set the width of *canvas*.

`(canvas-height `*canvas*`)`  
`(set-canvas-height! `*canvas height*`)`

Get or set the height of *canvas*.

`(canvas-title `*string*`)`  
`(set-canvas-title! `*canvas title*`)`

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
The size and shape of a turtle are implementation-dependent,
but it must be possible or a user to determine the angle
in which the tortoise points by looking at it.

## Shape predicates

`(shape? `*obj*`)`  
`(shape? `*obj canvas*`)`

Returns `#t` if *obj* is a shape and `#f` otherwise.
The second form asks if *obj* is a shape associated with *canvas*.

There is also a predicate for each shape type: `point?`, `line?`, etc.

## Shape properties

The following procedures are polymorphic in the shape argument.

`(pen-color `*shape*`)`  
`(set-pen-color! *shape ` color*`)`

Gets or sets the pen color used to draw the outline of the shape.
The initial pen color is black.

`(pen-width `*shape*`)`  
`(set-pen-width! *shape ` color*`)`

Gets or sets the pen width used to draw the outline of the shape.
The initial pen width is 1 pixel.

`(pen-fill-color `*shape*`)`  
`(set-pen-fill-color! *shape ` color*`)`

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
`(set-turtle-speed `*turtle n*`)`

Gets or sets the turtle's speed in spontaneous motion to *n*.
The initial value is 0, which means the turtle moves only
when it is told to move.

`(turtle-angle `*turtle n*`)`  

Gets the angle in degrees in which the turtle points.
The initial value is 0, which means the turtle points straight up.

## Turtle actions

`(turtle-down! `*turtle*`)`

Put the turtle's pen down, so that it draws a line behind itself.
Note that this line is not itself a shape.

`(turtle-up! `*turtle*`)`

Put the turtle's pen up, so that it does not draw a line.

`(turtle-forward! `*turtle distance*`)`  
`(turtle-backward! `*turtle distance*`)`

Moves the turtle forward/backward the given distance.
The speed with which the turtle moves is specified by the `turtle-speed` property,
unless that value is 0, in which case the turtle's speed is implementation-specified.

`(turtle-left! `*turtle degrees*`)`  
`(turtle-right! `*turtle degrees*`)`

Turn the turtle's inherent angle by *degrees*.
The use of degrees instead of radians is traditional,
stemming from the original Logo turtles.

## Events

See the [UI event SRFI](UiEvents.md).