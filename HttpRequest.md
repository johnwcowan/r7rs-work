This is a simple API for making basic HTTP requests.

## Issues

1. Should request objects be dictionaries
or immutable records?  The API surface gets bigger
with records, and they are harder to debug,
but they are less easy to damage
with an ill-considered mutator.

1. Should we use  accumulators and generators instead of ports?
Ports are more convenient, but they require custom-port support, as in
[SRFI 181](https://srfi.schemers.org/srfi-181/srfi-181.html).

## Specification

### Procedures

`(http-request `*request*`)` => response

Accepts an HTTP request object and either returns an HTTP response object
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
This can be either a request object or a response object,
depending on the type of HTTP error.

### Request objects

A request object is a dictionary containing the following keys
as symbols.  It is an error to omit *verb* or *url*; the other keys
are optional.
Keys not specified by this SRFI have implementation-dependent meaning.

*verb*:  A symbol representing the HTTP method to transmit.
The name of the symbol is uppercased before transmitting it.

*url*:  A string representing the URL to be sent to the server.

*request-port*:  A binary input port.  Bytes read from it
by `http-request` are sent as the body of the request.

OR

*request-generator*:  A SRFI 158 generator.
`http-request` reads objects from it that must be either bytes
(exact integers in the range 0-255) or bytevectors.
They are sent as the body of the request.

### Response objects

A response object is a SRFI 225 dictionary that may contain
the keys below.
Keys not specified by this SRFI have implementation-dependent meaning.

*request*:  The request object to which this is a response.

*status*:  An exact integer representing the HTTP status code.

*url*:  The URL of a newly created resource

*headers*:  The headers of the response as a SRFI 225 dictionary.
Multiple headers are coalesced in the order they appear in the response,
with a space character separating them.

*headers-dtd*: A SRFI 225 dictionary-type descriptor which specifies
how to access *headers*.  If omitted or #f, `eqv-alist-dtd` is used.

*cookie-jar*:  A cookie-jar (see above).  Cookies are inserted
or replaced according to the cookie protocol.
If omitted or `#f`, cookies are not processed.

*cookie-jar-dtd*: A SRFI 225 dictionary-type descriptor which specifies
how to access *headers*.

*previous*:  Another request object, showing that this request
is the result of a redirection.  If omitted, there is no previous request.

*response-port*:  A binary output port.  `http-request` writes
the bytes of the response body are written to it.

OR

*request-accumulator*:  A SRFI 158 generator.
`http-request` writes bytes or bytevectors
to it which represent the body of the request.
