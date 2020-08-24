The following Schemes have implementations of property lists:

|Scheme|Get property|Put property|Remove property|Get p-list|
|Chicken|get|put or (setter get)|remprop! |symbol-plist|
|Chez|getprop|putprop|remprop|property-list (returns copy)|
|Ikarus|getprop|putprop|remprop|property-list (returns copy)|
|Larceny|getprop|putprop|remprop|(none)|
|Bigloo|getprop|putprop! |remprop! |symbol-plist|
|Guile|symbol-property|set-symbol-property! |symbol-property-remove! |(none)|
|

Chicken also provides:
* get-properties, which searches for multiple properties simultaneously
* (setter symbol-plist), which replaces the whole property list

Kawa's underlying implementation provides property lists for its Elisp implementation,
but they aren't directly exposed to Scheme except through the Java FFI.

For a pre-SRFI for disembodied plists, see
[PropertyListAPI](http://htmlpreview.github.io?https://github.com/johnwcowan/r7rs-work/blob/master/PropertyListAPI.html).
