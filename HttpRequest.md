This is a simple API for making basic HTTP requests. 

## Issues

Should some of the dictionaries be degeneralized to always have the same DTO?

## Specification

Note that this specification depends on [SRFI 225](https://srfi.schemers.org/srfi-225/srfi-225.html).

### Procedures

`(http-request `*request request-dto*`)` => response

Accepts an HTTP request object, which is a SRFI 225
dictionary, and either returns an HTTP response object
or raises a condition satisfying `http-error?`.
The *request-dto* object is the dictionary-type object
for *request*.

`(http-error? `*obj*`)`

Returns `#t` if *obj* is a condition raised when
something has gone wrong in an `http-request` call,
and `#f` otherwise.  It always returns `#t` if one
of the four following predicates returns `#t`.

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

*headers*: A SRFI 225 dictionary that maps the names of headers
as symbols to the contents of the header as a string.

*headers-dto*: The dictionary-type object that describes
*headers*.  If it is omitted or `#f`, `eqv-alist-dto` is assumed.

*cookie-jar*: A SRFI 225 dictionary that maps the identifying information
for a cookie to the value of the cookie as a string.  See
the cookie jar section below.  If this key is supplied, any
"Cookie" header in *headers* is ignored.

*cookie-jar-dto*: The dictionary-type object that describes
*cookie-jar*.  If it is omitted or `#f`, `equal-alist-dto` is assumed.

*request-port*:  A binary input port containing the body of the
request.  The http-request` procedure reads all available bytes
from it and sends them as the body of the request.

*request-generator*:  A SRFI 158 generator.
`http-request` reads objects from it that must be either bytes
(exact integers in the range 0-255) or bytevectors.
They are sent as the body of the request.

*response-body-type*: a symbol, either `port` or `generator`,
to specify how the response body should be made available.
If it is `#f` or omitted, the response body is ignored.

It is an error if both *request-port* and *request-generator*
are present.  It is not an error if they are both absent,
which means that there is no request body.

### Response objects

A response object is a SRFI 225 dictionary that may contain
the keys below.
Keys not specified by this SRFI have implementation-dependent meaning.

*request*:  The request object to which this is a response.

*status*:  An exact integer representing the HTTP status code.

*url*:  The URL of a newly created resource, or `#f` if none.

*headers*:  The headers of the response as a SRFI 225 dictionary.
Multiple headers are coalesced in the order they appear in the response,
with a space character separating them.

*headers-dto*: A SRFI 225 dictionary-type descriptor which specifies
how to access *headers*.  If omitted or #f, `eqv-alist-dto` is used.

*cookie-jar*:  A cookie-jar (see below).  Cookies are inserted
or replaced according to the cookie protocol.
If omitted or `#f`, cookies are not processed.  If the client is
maintaining a cookie jar, this value should displace it so that
it can grow and shrink accordingly.

*cookie-jar-dto*: A SRFI 225 dictionary-type obj which specifies
how to access *headers*.

*previous*:  Another response object, showing that this request
is the result of a redirection.  If omitted, there is no previous request.

*response-port*:  A binary output port.  `http-request` writes
the bytes of the response body are written to it.

*response-accumulator*:  A SRFI 158 acummulator.
`http-request` writes bytes or bytevectors
to it which represent the body of the request.

The request object specifies whether *response-port*
or *response-accumulator* is present.

## Cookie jars

A cookie jar is, perhaps unsurprisingly, another SRFI 225
dictionary.  The key is a pair whose car is an URL-like
string and whose cdr is the cookie's key, also a string; the combination
is unique in the jar.  Each corresponding value is also a pair:
the car is the cookie's value and the cdr is an alist mapping
the cookie's attribute names to their values.  The "URL-like string"
is constructed from a scheme that depends on the Secure attribute
("https" if yes, "http" if no), the Domain attribute, and the Path
attribute.

A cookie in the jar is sent to the server if:

 * The URL-like string matches the URL being sent to the server.
   An "https" scheme in the URL matches an "http" scheme in the
   URL-like string, but not vice versa, and a longer path in the URL
   matches a shorter path in the URL-like string.
 * The expiration date has not yet passed.  (If it has, the cookie removed
   from the jar.)  Session cookies are always sent.
   
The other cookie attributes currently don't affect cookie transmission.

