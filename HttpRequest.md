This is a simple API for making basic HTTP requests.

## Issues

Are the request and reply objects [dictionaries](Dictionaries.md) or records?

## Specification

### Procedures

`(http-request `*request*`)` => reply

Accepts an HTTP request object and either returns an HTTP reply object
or raises a condition satisfying `http-error?`.

`(http-error? `*obj*`)`

Returns `#t` if *obj* is a condition raised when
something has gone wrong in an `http-request` call,
and `#f` otherwise.

`(http-connect-error? `*obj*`)`

Returns `#t` if *obj* is a condition raised when
`http-request` failed to connect to the HTTP server,
and `#f` otherwise.  Payload is the request object.

`(http-timeout-error? `*obj*`)`

Returns `#t` if *obj* is a condition raised when
 `http-request` timed out either connecting to,
sending to, or receiving from the HTTP server,
and `#f` otherwise.  Payload is the request object.

`(http-response-error? `*obj*`)`

Returns `#t` if *obj* is a condition raised when
 `http-request` returned a result whose status code
is 300 or greater.  Payload is the response object.

`(http-redirect-error? `*obj*`)`

Returns `#t` if *obj* is a condition raised when
 repeated HTTP requests involve either a redirection loop
or too many redirections.  Payload is the response object.

`(http-error-payload `*http-error*`)`

Returns the payload associated with an HTTP error.
This can be either a request object or a reply object, depending on the type of HTTP error.

### Request objects

A request object contains the following elements
(it is an error to omit *verb* or *url*):

*verb*:  A symbol representing the HTTP method to transmit.
The name of the symbol is uppercased before transmitting it.

*url*:  A string representing the URL to be sent to the server.

*headers*:  A [dictionary](Dictionaries.md) containing headers to be sent.
Keys are lower-case symbols without a trailing colon.

*cookie-jar*:  A A [dictionary](Dictionaries.md) containing cookies to possibly be sent.
Keys are lists of the form `(`*domain path name*`)`;
values are strings.  If omitted or `#f`, treated as an empty dictionary.

*content*:  A [SRFI 158](http://srfi.schemers.org/srfi-158/srfi-158.html)
accumulator to which zero or more bytevectors can be passed which
form the body of the request.
Passing an end-of-file object indicates the end of the request body.

The implementation must process all of the bytevector's contents
before returning from the accumulator procedure.
This allows the caller to reuse the bytevector.

### Reply objects

A response object may contain the following elements:

*request*:  The request object to which this is a response.

*status*:  An exact integer representing the HTTP status code.

*url*:  The URL of a newly created resource

*headers*:  The headers of the response.
Multiple headers are coalesced in the order they appear in the response,
with a space character separating them.

*cookie-jar*:  A cookie-jar (see above).  Cookies are inserted
or replaced according to the cookie protocol.
If omitted or `#f`, cookies are not processed.

*previous*:  Another request object, showing that this request
is the result of a redirection.

*content*:  A SRFI 158 generator.  Invoking it returns a bytevector
containing bytes from the response body,
or an end of file object if no bytes are available.

It is an error for the caller to assume that the contents of the
bytevector will remain unchanged after the generator is invoked again.
This allows the implementation to reuse the bytevector.
