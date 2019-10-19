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
The standardized properties are `left`, `top`, `height`, `width`,
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

`(canvas-height `*canvas*`)`  
`(set-canvas-height! `*canvas height*`)`

Get or set the height of *canvas*.

`(canvas-width `*canvas*`)`  
`(set-canvas-width! `*canvas x*`)`

Get or set the width of *canvas*.

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
and repainting everythin with the canvas color.

`(canvas-dispose ` *canvas*`)`

Disconnect from or eliminate *canvas*,
disposing of all shapes created from it.

